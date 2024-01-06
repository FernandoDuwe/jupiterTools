unit JupiterForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ComCtrls, StdCtrls, Menus, ActnList, JupiterRoute, JupiterConsts,
  JupiterVariable, JupiterAction, JupiterFormGenerator, JupiterGeneratorForm,
  jupiterformutils, JupiterStandardModule, LCLType;

type

  { TFJupiterForm }

  TFJupiterForm = class(TForm)
    acActionShortcuts: TActionList;
    acF1: TAction;
    acF2: TAction;
    acF3: TAction;
    acF4: TAction;
    acF5: TAction;
    acF6: TAction;
    acF7: TAction;
    acF8: TAction;
    acF9: TAction;
    acF10: TAction;
    acF11: TAction;
    acF12: TAction;
    imgInfo: TImage;
    lbHelp: TLabel;
    miViewParams: TMenuItem;
    miRefresh: TMenuItem;
    miEditGenerator: TMenuItem;
    pnHint: TPanel;
    ppMenu: TPopupMenu;
    sbActions: TScrollBox;
    sbStatus: TStatusBar;
    procedure acF10Execute(Sender: TObject);
    procedure acF11Execute(Sender: TObject);
    procedure acF12Execute(Sender: TObject);
    procedure acF1Execute(Sender: TObject);
    procedure acF2Execute(Sender: TObject);
    procedure acF3Execute(Sender: TObject);
    procedure acF4Execute(Sender: TObject);
    procedure acF5Execute(Sender: TObject);
    procedure acF6Execute(Sender: TObject);
    procedure acF7Execute(Sender: TObject);
    procedure acF8Execute(Sender: TObject);
    procedure acF9Execute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure miEditGeneratorClick(Sender: TObject);
    procedure miFormParamsClick(Sender: TObject);
    procedure miRefreshClick(Sender: TObject);
    procedure miViewParamsClick(Sender: TObject);
  private
    FActions     : TJupiterActionList;
    FParams      : TJupiterVariableList;
    FRoute       : TJupiterRoute;
    FGenerator   : TJupiterGeneratorForm;
    FHint        : String;
    FSearchText  : String;
    FPrepared    : Boolean;
    FIsModal     : Boolean;
    FSpecialSize : Boolean;

    FSaveGeneratorAction : TJupiterAction;

    procedure Internal_SetHint(prHint : String);
  published
    property Actions     : TJupiterActionList    read FActions     write FActions;
    property Hint        : String                read FHint        write Internal_SetHint;
    property IsModal     : Boolean               read FIsModal     write FIsModal;
    property Prepared    : Boolean               read FPrepared    write FPrepared;
    property Params      : TJupiterVariableList  read FParams      write FParams;
    property Route       : TJupiterRoute         read FRoute       write FRoute;
    property Generator   : TJupiterGeneratorForm read FGenerator   write FGenerator;
    property SearchText  : String                read FSearchText;
    property SpecialSize : Boolean               read FSpecialSize write FSpecialSize;

    property SaveGeneratorAction : TJupiterAction read FSaveGeneratorAction write FSaveGeneratorAction;
  protected
    procedure Internal_SaveGeneratorClick(Sender : TObject); virtual;

    procedure Internal_UpdateComponents; virtual;
    procedure Internal_UpdateDatasets; virtual;
    procedure Internal_UpdateCalcs; virtual;

    procedure Internal_PrepareForm; virtual;
  public
    procedure PrepareForm; virtual;
    procedure UpdateForm(prUpdateDatasets : Boolean = True; prUpdateComponentes : Boolean = True; prUpdateCalcs : Boolean = True); virtual;
    procedure Search(prSearch : String); virtual;
  end;

var
  FJupiterForm: TFJupiterForm;

implementation

uses JupiterApp, JupiterDialogForm, uGenerator, JupiterFormTabSheet;

{$R *.lfm}

{ TFJupiterForm }

procedure TFJupiterForm.FormActivate(Sender: TObject);
begin
  Self.UpdateForm();
end;

procedure TFJupiterForm.acF1Execute(Sender: TObject);
begin
  if Assigned(Self.Actions.GetActionButton(0, sbActions)) then
    Self.Actions.GetActionButton(0, sbActions).Click;
end;

procedure TFJupiterForm.acF10Execute(Sender: TObject);
begin
  if Assigned(Self.Actions.GetActionButton(9, sbActions)) then
    Self.Actions.GetActionButton(9, sbActions).Click;
end;

procedure TFJupiterForm.acF11Execute(Sender: TObject);
begin
  if Assigned(Self.Actions.GetActionButton(10, sbActions)) then
    Self.Actions.GetActionButton(10, sbActions).Click;
end;

procedure TFJupiterForm.acF12Execute(Sender: TObject);
begin
  if Assigned(Self.Actions.GetActionButton(11, sbActions)) then
    Self.Actions.GetActionButton(11, sbActions).Click;
end;

procedure TFJupiterForm.acF2Execute(Sender: TObject);
begin
  if Assigned(Self.Actions.GetActionButton(1, sbActions)) then
    Self.Actions.GetActionButton(1, sbActions).Click;
end;

procedure TFJupiterForm.acF3Execute(Sender: TObject);
begin
  if Assigned(Self.Actions.GetActionButton(2, sbActions)) then
    Self.Actions.GetActionButton(2, sbActions).Click;
end;

procedure TFJupiterForm.acF4Execute(Sender: TObject);
begin
  if Assigned(Self.Actions.GetActionButton(3, sbActions)) then
    Self.Actions.GetActionButton(3, sbActions).Click;
end;

procedure TFJupiterForm.acF5Execute(Sender: TObject);
begin
  if Assigned(Self.Actions.GetActionButton(4, sbActions)) then
    Self.Actions.GetActionButton(4, sbActions).Click;
end;

procedure TFJupiterForm.acF6Execute(Sender: TObject);
begin
  if Assigned(Self.Actions.GetActionButton(5, sbActions)) then
    Self.Actions.GetActionButton(5, sbActions).Click;
end;

procedure TFJupiterForm.acF7Execute(Sender: TObject);
begin
  if Assigned(Self.Actions.GetActionButton(6, sbActions)) then
    Self.Actions.GetActionButton(6, sbActions).Click;
end;

procedure TFJupiterForm.acF8Execute(Sender: TObject);
begin
  if Assigned(Self.Actions.GetActionButton(7, sbActions)) then
    Self.Actions.GetActionButton(7, sbActions).Click;
end;

procedure TFJupiterForm.acF9Execute(Sender: TObject);
begin
  if Assigned(Self.Actions.GetActionButton(8, sbActions)) then
    Self.Actions.GetActionButton(8, sbActions).Click;
end;

procedure TFJupiterForm.FormCreate(Sender: TObject);
begin
  Self.FSpecialSize := False;
  Self.FPrepared    := False;

  Self.FActions   := TJupiterActionList.Create;
  Self.FParams    := TJupiterVariableList.Create;
  Self.FRoute     := TJupiterRoute.Create(ROOT_PATH);
  Self.FGenerator := TJupiterGeneratorForm.Create;

  Self.FSaveGeneratorAction := TJupiterAction.Create('Salvar', @Internal_SaveGeneratorClick);

  Self.FActions.PopupMenu   := ppMenu;
  Self.FActions.CompactMode := TJupiterStandardModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Standard')).CompactMode;

  Self.IsModal  := False;
end;

procedure TFJupiterForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Self.FActions);
  FreeAndNil(Self.FParams);
  FreeAndNil(Self.FRoute);
  FreeAndNil(Self.FGenerator);
  FreeAndNil(Self.FSaveGeneratorAction);
end;

procedure TFJupiterForm.FormKeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TFJupiterForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Exit;

  try
    if Key = VK_F1 then
      Self.Actions.GetActionButton(0, sbActions).Click;

    if Key = VK_F2 then
      Self.Actions.GetActionButton(1, sbActions).Click;

    if Key = VK_F3 then
      Self.Actions.GetActionButton(2, sbActions).Click;

    if Key = VK_F4 then
      Self.Actions.GetActionButton(3, sbActions).Click;

    if Key = VK_F5 then
      Self.Actions.GetActionButton(4, sbActions).Click;

    if Key = VK_F6 then
      Self.Actions.GetActionButton(5, sbActions).Click;

    if Key = VK_F7 then
      Self.Actions.GetActionButton(6, sbActions).Click;

    if Key = VK_F8 then
      Self.Actions.GetActionButton(7, sbActions).Click;

    if Key = VK_F9 then
      Self.Actions.GetActionButton(8, sbActions).Click;

    if Key = VK_F10 then
      Self.Actions.GetActionButton(9, sbActions).Click;

    if Key = VK_F11 then
      Self.Actions.GetActionButton(10, sbActions).Click;

    if Key = VK_F12 then
      Self.Actions.GetActionButton(11, sbActions).Click;
  except
  end;
end;

procedure TFJupiterForm.FormResize(Sender: TObject);
begin
  if Self.Prepared then
    Self.FActions.UpdateActions(sbActions);
end;

procedure TFJupiterForm.FormShow(Sender: TObject);
begin
  try
    Self.PrepareForm;

    if ((Self.IsModal) and (not Self.SpecialSize)) then
    begin
      if vrJupiterApp.Params.Exists('Interface.Form.ModalShowMaximized') then
        Self.WindowState := wsMaximized
      else
      begin
        Self.Position := poDefault;

        Self.Height := PercentOfScreen(Screen.Height, 80);
        Self.Width := PercentOfScreen(Screen.Width, 80);

        Self.Position := poScreenCenter;
      end;
    end;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFJupiterForm.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  Exit;

  if UTF8Key = #112 then
    Self.Actions.GetActionButton(0, sbActions).Click;

  if UTF8Key = #113 then
    Self.Actions.GetActionButton(1, sbActions).Click;

  if UTF8Key = #114 then
    Self.Actions.GetActionButton(2, sbActions).Click;

  if UTF8Key = #115 then
    Self.Actions.GetActionButton(3, sbActions).Click;

  if UTF8Key = #116 then
    Self.Actions.GetActionButton(4, sbActions).Click;

  if UTF8Key = #117 then
    Self.Actions.GetActionButton(5, sbActions).Click;

  if UTF8Key = #118 then
    Self.Actions.GetActionButton(6, sbActions).Click;

  if UTF8Key = #119 then
    Self.Actions.GetActionButton(7, sbActions).Click;

  if UTF8Key = #120 then
    Self.Actions.GetActionButton(8, sbActions).Click;

  if UTF8Key = #121 then
    Self.Actions.GetActionButton(9, sbActions).Click;

  if UTF8Key = #122 then
    Self.Actions.GetActionButton(10, sbActions).Click;

  if UTF8Key = #123 then
    Self.Actions.GetActionButton(11, sbActions).Click;
end;

procedure TFJupiterForm.miEditGeneratorClick(Sender: TObject);
var
  vrGenerator : TFGenerator;
begin
  Application.CreateForm(TFGenerator, vrGenerator);
  try
    vrGenerator.IsModal := True;
    vrGenerator.FormID  := Self.Params.VariableById(FIELD_ID_GENERADOR).Value;
    vrGenerator.ShowModal;
  finally
    vrGenerator.Release;
    FreeAndNil(vrGenerator);
  end;
end;

procedure TFJupiterForm.miFormParamsClick(Sender: TObject);
begin
end;

procedure TFJupiterForm.miRefreshClick(Sender: TObject);
begin
  Self.UpdateForm;
end;

procedure TFJupiterForm.miViewParamsClick(Sender: TObject);
var
  vrDialog : TJupiterDialogForm;
begin
  vrDialog := TJupiterDialogForm.Create;
  try
    vrDialog.Title    := 'Parâmetros de ' + Self.Caption;
    vrDialog.Hint     := 'Parâmetros utilizados por esse formulário. Esses parâmetros são controlados pelo Jupiter.';
    vrDialog.OnlyShow := True;

    vrDialog.Fields.CopyValues(Self.Params);

    vrDialog.Show;
  finally
    FreeAndNil(vrDialog);
  end;
end;

procedure TFJupiterForm.Internal_SetHint(prHint: String);
begin
  Self.FHint := prHint;

  lbHelp.Caption := prHint;
  pnHint.Visible := Trim(Self.FHint) <> EmptyStr;

  if pnHint.Visible then
  begin
    pnHint.Height := 66;
    lbHelp.Top    := 24;
    lbHelp.Left   := 64;
  end;

  Self.UpdateForm(False);
end;

procedure TFJupiterForm.Internal_SaveGeneratorClick(Sender: TObject);
begin
  //
end;

procedure TFJupiterForm.Internal_UpdateComponents;
begin
  sbActions.Visible := Self.Actions.Size > 0;

  miEditGenerator.Enabled := Params.Exists(FIELD_ID_GENERADOR);
  miViewParams.Enabled    := Params.Size > 0;
end;

procedure TFJupiterForm.Internal_UpdateDatasets;
begin
  //
end;

procedure TFJupiterForm.Internal_UpdateCalcs;
begin
  if pnHint.Visible then
  begin
    if ((not Self.Params.Exists('Hint.Success')) and (not Self.Params.Exists('Hint.Error'))) then
    begin
      pnHint.Color      := clDefault;
      lbHelp.Font.Color := clDefault;
    end;

    if Self.Params.Exists('Hint.Success') then
    begin
      pnHint.Color      := clMoneyGreen;
      lbHelp.Font.Color := clWhite;
    end;

    if Self.Params.Exists('Hint.Error') then
      pnHint.Color      := clInfoBk;
  end;

  if not Self.IsModal then
    if vrJupiterApp.Params.Exists('Interface.CurrentForm.Title') then
      if ((Trim(Self.Caption) <> EmptyStr) and (Self.Caption <> vrJupiterApp.Params.VariableById('Interface.CurrentForm.Title').Value)) then
        vrJupiterApp.Params.VariableById('Interface.CurrentForm.Title').Value := Self.Caption;
end;

procedure TFJupiterForm.Internal_PrepareForm;
begin
  //
end;

procedure TFJupiterForm.PrepareForm;
var
  vrVez : Integer;
begin
  if Self.Prepared then
    Exit;

  try
    sbStatus.Visible := Self.BorderStyle <> bsNone;

    Self.Internal_PrepareForm;

    if Self.Params.Exists(FIELD_ID_GENERADOR) then
    begin
      Self.Generator.FormID := Self.Params.VariableById(FIELD_ID_GENERADOR).Value;

      // Adiciona as ações no formulario
      Self.Actions.CopyFromList(Self.Generator.Actions, Self.SaveGeneratorAction);
    end;

    if Self.Params.Exists('title') then
      Self.Caption := Self.Params.VariableById('title').Value;
  finally
    ppMenu.Images := vrJupiterApp.MainIcons;

    Self.Actions.BuildActions(sbActions);

    Self.Prepared := True;
  end;
end;

procedure TFJupiterForm.UpdateForm(prUpdateDatasets : Boolean = True; prUpdateComponentes : Boolean = True; prUpdateCalcs : Boolean = True);
begin
  try
    if prUpdateDatasets then
      Self.Internal_UpdateDatasets;

    if prUpdateComponentes then
      Self.Internal_UpdateComponents;

    if prUpdateCalcs then
      Self.Internal_UpdateCalcs;
  finally
    if Parent is TJupiterFormTabSheet then
      TJupiterFormTabSheet(Parent).Update;

    Application.ProcessMessages;
  end;
end;

procedure TFJupiterForm.Search(prSearch: String);
begin
  try
    Self.FSearchText := prSearch;
  finally
    Self.UpdateForm;
  end;
end;

end.

