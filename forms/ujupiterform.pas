unit uJupiterForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, ExtCtrls,
  ButtonPanel, StdCtrls, Menus, ComCtrls, Buttons, JupiterConsts,
  JupiterFormTabSheet, jupiterformutils, JupiterApp, uJupiterAction,
  jupiterDesktopApp, jupiterformcomponenttils, JupiterVariable,
  jupiterStringUtils, jupiterDatabaseWizard;

type

  { TFJupiterForm }

  TFJupiterForm = class(TForm)
    acOptions: TActionList;
    edSearch: TEdit;
    fpOptions: TFlowPanel;
    Image1: TImage;
    MenuItem1: TMenuItem;
    Separator3: TMenuItem;
    miLookColumn: TMenuItem;
    miAjustRatioRight: TMenuItem;
    miAjustRatioLeft: TMenuItem;
    Separator1: TMenuItem;
    miParams: TMenuItem;
    miUpdate: TMenuItem;
    pnBottom: TPanel;
    pnSearchBar: TPanel;
    pmOptions: TPopupMenu;
    Separator2: TMenuItem;
    tmrAutoUpdater: TTimer;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure miAjustRatioLeftClick(Sender: TObject);
    procedure miAjustRatioRightClick(Sender: TObject);
    procedure miLookColumnClick(Sender: TObject);
    procedure miParamsClick(Sender: TObject);
    procedure miUpdateClick(Sender: TObject);
    procedure pnSearchBarClick(Sender: TObject);
    procedure tmrAutoUpdaterTimer(Sender: TObject);
    procedure Internal_OnAfterActionExecute(Sender : TObject);
    procedure Internal_OnShortCutClick(Sender : TObject);
  private
    FFormID : String;
    FHint : String;
    FPercentDivisor : Integer;

    FShowSearchBar : Boolean;
    FActionGroup   : TJupiterActionGroup;
    FOwnerTab      : TJupiterFormTabSheet;
    FParams        : TJupiterVariableList;

    procedure Internal_AddShortcutsToMenu;
    procedure Internal_SetSearchBar(prNewValue : Boolean);
  published
    property ActionGroup    : TJupiterActionGroup  read FActionGroup    write FActionGroup;
    property ShowSearchBar  : Boolean              read FShowSearchBar  write Internal_SetSearchBar default False;
    property OwnerTab       : TJupiterFormTabSheet read FOwnerTab       write FOwnerTab;
    property Params         : TJupiterVariableList read FParams         write FParams;
    property FormID         : String               read FFormID;
    property Hint           : String               read FHint           write FHint;
    property PercentDivisor : Integer              read FPercentDivisor write FPercentDivisor;

    procedure Internal_UpdateComponents; virtual;
    procedure Internal_UpdateDatasets; virtual;
    procedure Internal_UpdateCalcs; virtual;
    procedure Internal_PrepareForm; virtual;
    procedure Internal_Resize; virtual;
    function Internal_OnRequestData : TJupiterVariableList; virtual;

    procedure Internal_BuildMenuParams;
    procedure Internal_ClickMenuClick(Sender: TObject);
  public
    procedure PrepareForm; virtual;
    procedure UpdateForm(prUpdateDatasets : Boolean = True; prUpdateComponentes : Boolean = True; prUpdateCalcs : Boolean = True); virtual;

    function IsWindowForm : Boolean;

    procedure DoSecureClose;
  end;

var
  FJupiterForm: TFJupiterForm;

implementation

uses SQLDB;

{$R *.lfm}

{ TFJupiterForm }

procedure TFJupiterForm.Internal_OnAfterActionExecute(Sender : TObject);
begin
  tmrAutoUpdater.Enabled := False;

  Self.UpdateForm();

  tmrAutoUpdater.Enabled := True;
end;

procedure TFJupiterForm.Internal_OnShortCutClick(Sender: TObject);
begin
  if not (Sender is TMenuItem) then
    Exit;

  vrJupiterApp.RunMacro(TMenuItem(Sender).Tag, Self.Internal_OnRequestData);
end;

procedure TFJupiterForm.Internal_AddShortcutsToMenu;
var
  vrWizard : TJupiterDatabaseWizard;
  vrQry : TSQLQuery;
  vrComponent : TJupiterComponentReference;
begin
  vrWizard := vrJupiterApp.NewWizard;
  vrQry := vrWizard.NewQuery;
  try
    if vrWizard.Count('SHORTCUTS', ' 1 = 1 ') = 0 then
      Exit;

    vrQry.SQL.Add(' SELECT * FROM SHORTCUTS ');
    vrQry.Open;
    vrQry.First;

    JupiterComponentsAddPopupMenuSeparator(pmOptions);

    while not vrQry.EOF do
    begin
      vrComponent := JupiterComponentsAddPopupMenuItem(pmOptions, vrQry.FieldByName('DESCRIPTION').AsString, vrQry.FieldByName('SHORTCUT').AsString, NULL_KEY);

      TMenuItem(vrComponent.Component).Tag := vrQry.FieldByName('DESTINY').AsInteger;
      TMenuItem(vrComponent.Component).OnClick := @Internal_OnShortCutClick;

      vrQry.Next;
    end;
  finally
    FreeAndNil(vrWizard);
    FreeAndNil(vrQry);
  end;
end;

procedure TFJupiterForm.FormShow(Sender: TObject);
begin
  try
    Self.PrepareForm;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFJupiterForm.Image1Click(Sender: TObject);
begin

end;

procedure TFJupiterForm.MenuItem1Click(Sender: TObject);
begin
  vrJupiterApp.RunMacro(TRIGGER_ONSHOWPARAMS, Self.Params);
end;

procedure TFJupiterForm.miAjustRatioLeftClick(Sender: TObject);
begin
  try
    Self.PercentDivisor := Self.PercentDivisor + 10;
  finally
    Self.UpdateForm(False, True, False);
  end;
end;

procedure TFJupiterForm.miAjustRatioRightClick(Sender: TObject);
begin
  try
    Self.PercentDivisor := Self.PercentDivisor - 10;
  finally
    Self.UpdateForm(False, True, False);
  end;
end;

procedure TFJupiterForm.miLookColumnClick(Sender: TObject);
begin
  miLookColumn.Checked := not miLookColumn.Checked;
end;

procedure TFJupiterForm.miParamsClick(Sender: TObject);
begin

end;

procedure TFJupiterForm.miUpdateClick(Sender: TObject);
begin
  tmrAutoUpdater.Enabled := False;

  Self.UpdateForm();

  tmrAutoUpdater.Enabled := True;
end;

procedure TFJupiterForm.pnSearchBarClick(Sender: TObject);
begin

end;

procedure TFJupiterForm.tmrAutoUpdaterTimer(Sender: TObject);
begin
  tmrAutoUpdater.Enabled := False;

  if Self.Showing then
    Self.UpdateForm();

  tmrAutoUpdater.Enabled := True;
end;

procedure TFJupiterForm.Internal_SetSearchBar(prNewValue: Boolean);
begin
  try
    Self.FShowSearchBar := prNewValue;
  finally
    Self.UpdateForm(False, True, False);
  end;
end;

procedure TFJupiterForm.FormActivate(Sender: TObject);
begin
  Self.UpdateForm();
end;

procedure TFJupiterForm.FormCreate(Sender: TObject);
begin
  Self.FFormID := JupiterStringUtilsGenerateGUID;

  Self.FPercentDivisor := 30;

  Self.FActionGroup := TJupiterActionGroup.Create;
  Self.FActionGroup.FlowPanel := fpOptions;

  Self.FActionGroup.ActionList := Self.acOptions;
  Self.FActionGroup.ImageList  := TJupiterDesktopApp(vrJupiterApp).ImageList;

  Self.FActionGroup.OnRequestData  := @Internal_OnRequestData;
  Self.FActionGroup.OnAfterExecute := @Internal_OnAfterActionExecute;

  pmOptions.Images := TJupiterDesktopApp(vrJupiterApp).ImageList;

  Self.FParams := TJupiterVariableList.Create;

  Self.Height := PercentOfScreen(Screen.Height, 80);
  Self.Width  := PercentOfScreen(Screen.Width, 80);
end;

procedure TFJupiterForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Self.FParams);
  FreeAndNil(Self.FActionGroup);
end;

procedure TFJupiterForm.FormResize(Sender: TObject);
begin
  Self.Internal_Resize;
end;

procedure TFJupiterForm.Internal_UpdateComponents;
begin
  DrawForm(Self);

  miParams.Enabled := Self.Params.Count > 0;

  pnBottom.Caption := '                              ' + Self.FHint;
  pnBottom.Visible := Trim(Self.FHint) <> EmptyStr;

  Self.ActionGroup.UpdateActions;

  miAjustRatioLeft.Caption  := 'Aumentar faixa à esquerda (' + IntToStr(Self.PercentDivisor) + '%)';
  miAjustRatioRight.Caption := 'Aumentar faixa central (' + IntToStr(100 - Self.PercentDivisor) + '%)';

  miAjustRatioRight.Enabled := Self.PercentDivisor >= 20;
  miAjustRatioLeft.Enabled := Self.PercentDivisor <= 80;

  pnSearchBar.Visible := Self.ShowSearchBar;

  if pnSearchBar.Visible then
  begin
    pnSearchBar.Top := fpOptions.Top + fpOptions.Height + 1;

    pnSearchBar.Height := GetTextHeight('PESQUISAR', edSearch.Font) + FORM_MARGIN_TOP;
  end;
end;

procedure TFJupiterForm.Internal_UpdateDatasets;
begin

end;

procedure TFJupiterForm.Internal_UpdateCalcs;
begin
  miParams.Caption := Format('Parâmetros (%0:d)', [Self.Params.Count]);
end;

procedure TFJupiterForm.Internal_PrepareForm;
begin
  fpOptions.Align := alTop;

  tmrAutoUpdater.Interval := vrJupiterApp.Params.VariableById(FORM_UPDATE_TIME).AsInteger;

  Self.Internal_BuildMenuParams;
end;

procedure TFJupiterForm.Internal_Resize;
begin
  edSearch.Top   := FORM_MARGIN_TOP;
  edSearch.Left  := FORM_MARGIN_LEFT;
  edSearch.Width := pnSearchBar.Width - FORM_MARGIN_LEFT - FORM_MARGIN_RIGHT;

  pnSearchBar.Width := FORM_MARGIN_TOP + edSearch.Height + FORM_MARGIN_BOTTOM;
end;

function TFJupiterForm.Internal_OnRequestData: TJupiterVariableList;
begin
  Result := TJupiterVariableList.Create;

  Result.AddVariable('FORMID', Self.FormID);
end;

procedure TFJupiterForm.Internal_BuildMenuParams;
var
  vrVez : Integer;
  vrObjReference : TJupiterComponentReference;
begin
  for vrVez := 0 to Self.Params.Count - 1 do
  begin
    if Self.Params.VariableByIndex(vrVez).Title <> EmptyStr then
      vrObjReference := JupiterComponentsAddPopupMenuItem(nil, Self.Params.VariableByIndex(vrVez).Title, EmptyStr, NULL_KEY)
    else
      vrObjReference := JupiterComponentsAddPopupMenuItem(nil, Self.Params.VariableByIndex(vrVez).ID, EmptyStr, NULL_KEY);

    TMenuItem(vrObjReference.Component).Tag := vrVez;
    TMenuItem(vrObjReference.Component).OnClick := @Internal_ClickMenuClick;

    miParams.Add(TMenuItem(vrObjReference.Component));
  end;
end;

procedure TFJupiterForm.Internal_ClickMenuClick(Sender: TObject);
var
  vrResult : String;
  vrVariable : TJupiterVariable;
begin
  vrVariable := Self.Params.VariableByIndex(TMenuItem(Sender).Tag);

  try
    vrResult := InputBox('Alterar valor do parâmetro ' + vrVariable.Title, 'Valor', vrVariable.Value);

    if vrResult <> '' then
      Self.Params.VariableById(vrVariable.ID).Value := vrResult;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFJupiterForm.PrepareForm;
begin
  try
    Self.Internal_PrepareForm;

    if Self.IsWindowForm then
      if vrJupiterApp.Params.VariableById(FORM_ALWAYS_MODAL).AsBool then
        Self.WindowState := wsMaximized;
  finally
    Self.Internal_AddShortcutsToMenu;

    Self.FActionGroup.Render;
  end;
end;

procedure TFJupiterForm.UpdateForm(prUpdateDatasets: Boolean; prUpdateComponentes: Boolean; prUpdateCalcs: Boolean);
begin
  if Self.Owner is TJupiterFormTabSheet then
    TJupiterFormTabSheet(Self.Owner).Caption := Self.Caption;

  if prUpdateDatasets then
    Self.Internal_UpdateDatasets;

  if prUpdateComponentes then
    Self.Internal_UpdateComponents;

  if prUpdateCalcs then
    Self.Internal_UpdateCalcs;

  Self.Internal_Resize;
end;

function TFJupiterForm.IsWindowForm: Boolean;
begin
  Result := not Assigned(OwnerTab);
end;

procedure TFJupiterForm.DoSecureClose;
begin
  if Assigned(OwnerTab) then
  begin
    Self.OwnerTab.DoClose;

    Exit;
  end;

  Self.Close;
end;

end.

