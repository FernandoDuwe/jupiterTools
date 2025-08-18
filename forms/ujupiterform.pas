unit uJupiterForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, ExtCtrls,
  ButtonPanel, StdCtrls, Menus, ComCtrls, Buttons, JupiterConsts,
  JupiterFormTabSheet, jupiterformutils, JupiterApp, uJupiterAction,
  jupiterDesktopApp, jupiterformcomponenttils, JupiterVariable,
  jupiterStringUtils, jupiterDatabaseWizard, uJupiterAppScript,
  jupiterthread;

type

  { TFJupiterForm }

  TFJupiterForm = class(TForm)
    acOptions: TActionList;
    acIncreaseLefPanel: TAction;
    acIncreaseCenterPanel: TAction;
    acExitIfModal: TAction;
    edSearch: TEdit;
    fpOptions: TFlowPanel;
    Image1: TImage;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miWorkMenu: TMenuItem;
    miThreads: TMenuItem;
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
    procedure acExitIfModalExecute(Sender: TObject);
    procedure acIncreaseCenterPanelExecute(Sender: TObject);
    procedure acIncreaseLefPanelExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure miAjustRatioLeftClick(Sender: TObject);
    procedure miAjustRatioRightClick(Sender: TObject);
    procedure miLookColumnClick(Sender: TObject);
    procedure miParamsClick(Sender: TObject);
    procedure miUpdateClick(Sender: TObject);
    procedure miWorkMenuClick(Sender: TObject);
    procedure pnSearchBarClick(Sender: TObject);
    procedure tmrAutoUpdaterTimer(Sender: TObject);
    procedure Internal_OnAfterActionExecute(Sender : TObject);
    procedure Internal_OnShortCutClick(Sender : TObject);
  private
    FFormID : String;
    FPercentDivisor : Integer;
    FThreadController : TJupiterThreadList;

    FShowSearchBar : Boolean;
    FActionGroup   : TJupiterActionGroup;
    FOwnerTab      : TJupiterFormTabSheet;
    FParams        : TJupiterVariableList;

    procedure Internal_AddShortcutsToMenu;
    procedure Internal_SetSearchBar(prNewValue : Boolean);
  protected
    FPrepared  : Boolean;
    FResizing  : Boolean;
    FHint : String;

  published
    property ActionGroup      : TJupiterActionGroup  read FActionGroup      write FActionGroup;
    property ShowSearchBar    : Boolean              read FShowSearchBar    write Internal_SetSearchBar default False;
    property OwnerTab         : TJupiterFormTabSheet read FOwnerTab         write FOwnerTab;
    property Params           : TJupiterVariableList read FParams           write FParams;
    property FormID           : String               read FFormID;
    property Hint             : String               read FHint             write FHint;
    property PercentDivisor   : Integer              read FPercentDivisor   write FPercentDivisor;
    property Prepared         : Boolean              read FPrepared         write FPrepared;
    property ThreadController : TJupiterThreadList   read FThreadController write FThreadController;

    procedure Internal_UpdateComponents; virtual;
    procedure Internal_UpdateDatasets; virtual;
    procedure Internal_UpdateCalcs; virtual;
    procedure Internal_PrepareForm; virtual;
    procedure Internal_Resize; virtual;
    function  Internal_OnRequestData : TJupiterVariableList; virtual;
    procedure Internal_OnCloseIfModal; virtual;

    procedure Internal_BuildMenuParams;
    procedure Internal_ClickMenuClick(Sender: TObject);
    procedure Internal_CreateShortcutList;

    function Internal_IsMainPage : Boolean; virtual;
    function Internal_EnableWorkMenu : Boolean; virtual;
    procedure Internal_AddToWorkMenu; virtual;
  public
    procedure Pause; virtual;
    procedure Resume; virtual;

    procedure PrepareForm; virtual;
    procedure UpdateForm(prUpdateDatasets : Boolean = True; prUpdateComponentes : Boolean = True; prUpdateCalcs : Boolean = True); virtual;

    function IsWindowForm : Boolean; virtual;

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
  vrVez : Integer;
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

    for vrVez := 0 to TJupiterDesktopApp(vrJupiterApp).DynamicShortcutList.Count - 1 do
      with TJupiterVariableList(TJupiterDesktopApp(vrJupiterApp).DynamicShortcutList.GetAtIndex(vrVez)) do
      begin
        vrComponent := JupiterComponentsAddPopupMenuItem(pmOptions, VariableById('Description').Value, VariableById('Shortcut').Value, NULL_KEY);

        TMenuItem(vrComponent.Component).Tag := VariableById('Destiny').AsInteger;
        TMenuItem(vrComponent.Component).OnClick := @Internal_OnShortCutClick;
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

procedure TFJupiterForm.MenuItem2Click(Sender: TObject);
begin
  Self.ThreadController.StopAll;
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

procedure TFJupiterForm.miWorkMenuClick(Sender: TObject);
begin
  Self.Internal_AddToWorkMenu;
end;

procedure TFJupiterForm.pnSearchBarClick(Sender: TObject);
begin

end;

procedure TFJupiterForm.tmrAutoUpdaterTimer(Sender: TObject);
begin
  tmrAutoUpdater.Enabled := False;

  if Self.Showing then
    Self.UpdateForm();

  tmrAutoUpdater.Enabled := not Application.Terminated;
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

procedure TFJupiterForm.acIncreaseLefPanelExecute(Sender: TObject);
begin
  miAjustRatioLeftClick(Sender);
end;

procedure TFJupiterForm.acIncreaseCenterPanelExecute(Sender: TObject);
begin
  miAjustRatioRightClick(Sender);
end;

procedure TFJupiterForm.acExitIfModalExecute(Sender: TObject);
begin
  if not Self.Internal_IsMainPage then
    if Self.IsWindowForm then
      Self.Internal_OnCloseIfModal;
end;

procedure TFJupiterForm.FormCreate(Sender: TObject);
begin
  Self.FResizing := False;
  Self.FPrepared := False;

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

  Self.Height      := PercentOfScreen(Screen.Height, 80);
  Self.Width       := PercentOfScreen(Screen.Width, 80);

  Self.WindowState := wsNormal;
  Self.Position    := poScreenCenter;

  Self.FThreadController := TJupiterThreadList.Create;
end;

procedure TFJupiterForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FThreadController);
  FreeAndNil(Self.FParams);
  FreeAndNil(Self.FActionGroup);
end;

procedure TFJupiterForm.FormResize(Sender: TObject);
begin
  Self.Internal_Resize;
end;

procedure TFJupiterForm.Internal_UpdateComponents;
begin
  miParams.Enabled := Self.Params.Count > 0;
  miThreads.Enabled := Self.ThreadController.Count > 0;
  miWorkMenu.Enabled := Self.Internal_EnableWorkMenu;

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

    edSearch.Top    := FORM_MARGIN_TOP;
    edSearch.Left   := FORM_MARGIN_LEFT;
    edSearch.Width  := pnSearchBar.Width - (FORM_MARGIN_LEFT + FORM_MARGIN_RIGHT);
    edSearch.Height := GetTextHeight('PESQUISAR', edSearch.Font) + FORM_MARGIN_TOP;

    pnSearchBar.Height := edSearch.Top + edSearch.Height + FORM_MARGIN_BOTTOM;
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
  if Self.Prepared and Self.Showing then
  begin                       {
    if not Self.IsWindowForm then
    begin
      Self.FResizing := True;

      Self.Visible := False;
      try
//        Self.WindowState := wsNormal;

        Self.Height := 100;
        Self.Width  := 100;
      finally
        Self.Visible     := True;
        Self.WindowState := wsMaximized;
        Self.FResizing   := False;
      end;
    end;                       }

    Self.Internal_UpdateComponents;
    Self.Refresh;
    Self.Repaint;
  end;
end;

function TFJupiterForm.Internal_OnRequestData: TJupiterVariableList;
begin
  Result := TJupiterVariableList.Create;

  Result.AddVariable('FORMID', Self.FormID);
end;

procedure TFJupiterForm.Internal_OnCloseIfModal;
begin
  Self.Close;
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

procedure TFJupiterForm.Internal_CreateShortcutList;
begin
  TJupiterDesktopApp(vrJupiterApp).SetShortCutList(acOptions);
end;

function TFJupiterForm.Internal_IsMainPage: Boolean;
begin
  Result := False;
end;

function TFJupiterForm.Internal_EnableWorkMenu: Boolean;
begin
  Result := False;
end;

procedure TFJupiterForm.Internal_AddToWorkMenu;
begin
  //
end;

procedure TFJupiterForm.Pause;
begin
  tmrAutoUpdater.Enabled := False;
end;

procedure TFJupiterForm.Resume;
begin
  tmrAutoUpdaterTimer(Self);

  tmrAutoUpdater.Enabled := True;
end;

procedure TFJupiterForm.PrepareForm;
var
  vrWaitPanel : TPanel;
  vrModalConfig : String;
begin
  vrWaitPanel := TPanel.Create(Self);
  vrWaitPanel.Parent := Self;
  vrWaitPanel.Align := alClient;
  vrWaitPanel.Caption := 'Aguarde...';
  vrWaitPanel.BringToFront;

  Application.ProcessMessages;

  try
    Self.Internal_PrepareForm;

    Self.Internal_CreateShortcutList;

    vrModalConfig := FORM_ALWAYS_MODAL;

    if not Self.Internal_IsMainPage then
      vrModalConfig := FORM_ALWAYS_MODAL_CHILD;

    if Self.IsWindowForm then
      if vrJupiterApp.Params.VariableById(vrModalConfig).AsBool then
        Self.WindowState := wsMaximized;

    if not vrJupiterApp.Params. VariableById('Interface.PerformanceMode').AsBool then
      DrawForm(Self);
  finally
    Self.Internal_AddShortcutsToMenu;

    PopupMenuShortcutsToActionShortcut(acOptions, pmOptions);

    Self.FActionGroup.Render;

    Self.Prepared := True;

    Application.ProcessMessages;
    FreeAndNil(vrWaitPanel);

    tmrAutoUpdater.Enabled := True;
  end;
end;

procedure TFJupiterForm.UpdateForm(prUpdateDatasets: Boolean; prUpdateComponentes: Boolean; prUpdateCalcs: Boolean);
begin
  if not Self.Prepared then
    Exit;

  tmrAutoUpdater.Enabled := False;
  tmrAutoUpdater.Enabled := Self.Prepared;

  if Self.Owner is TJupiterFormTabSheet then
    TJupiterFormTabSheet(Self.Owner).Caption := Self.Caption;

  if prUpdateDatasets then
    Self.Internal_UpdateDatasets;

  if prUpdateComponentes then
    Self.Internal_UpdateComponents;

  if prUpdateCalcs then
    Self.Internal_UpdateCalcs;
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
