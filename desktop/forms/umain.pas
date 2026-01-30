unit uMain;
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ActnList, ExtCtrls, Buttons, uJupiterForm, JupiterFormTab, StdCtrls,
  jupiterMainMenuGenerator, JupiterApp, JupiterConsts, JupiterVariable,
  JupiterVariableDataProvider, jupiterformutils, jupitertreeviewmenugenerator,
  uPSComponent, jupiterDesktopApp, uContextMenu, uMenuNavigator,
  udatabasefinder, uQuickJump;

type

  { TFMain }

  TFMain = class(TFJupiterForm)
    acNewTab: TAction;
    acNextTab: TAction;
    acPreviousTab: TAction;
    acCloseTab: TAction;
    acCloseAllButThis: TAction;
    acUpdateAndResize: TAction;
    ApplicationProperties1: TApplicationProperties;
    ilIconFamily: TImageList;
    ilTabs: TImageList;
    jtMainTab: TJupiterFormTab;
    mmMainMenu: TMainMenu;
    pmTabOptions: TPopupMenu;
    sbStatus: TStatusBar;
    procedure acCloseAllButThisExecute(Sender: TObject);
    procedure acCloseTabExecute(Sender: TObject);
    procedure acNewTabExecute(Sender: TObject);
    procedure acNextTabExecute(Sender: TObject);
    procedure acPreviousTabExecute(Sender: TObject);
    procedure acUpdateAndResizeExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure jtMainTabChange(Sender: TObject);
    procedure jtMainTabCloseTab(Sender: TObject);
    procedure jtMainTabCloseTabClicked(Sender: TObject);
    procedure jtMainTabResize(Sender: TObject);
    procedure pmTabOptionsPopup(Sender: TObject);
    procedure sbStatusDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
  private
    FNewTabClick : Boolean;
    FComboBox : TComboBox;

    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateCalcs; override;
    procedure Internal_CreatePopMenuTab;
    function  Internal_IsMainPage : Boolean; override;
    procedure Internal_Resize; override;

    procedure Internal_CreateComboBox;
    procedure Internal_CloseCurrentTab(Sender: TObject);
    procedure Internal_MoveLeftTab(Sender: TObject);
    procedure Internal_MoveRightTab(Sender: TObject);
    procedure Internal_CloseAllButCurrentTab(Sender: TObject);
    procedure Internal_GoToNextTab(Sender: TObject);
    procedure Internal_GoToPreviousTab(Sender: TObject);
    procedure Internal_OpenAsModal(Sender: TObject);
    procedure Internal_MenuGoToTabClick(Sender : TObject);
    Procedure Internal_OnGlobalException(Sender : TObject; E : Exception);
    function CurrentForm : TForm;
  public
    CurrentMessage : TPanel;

    procedure NewTab(Form : TForm);
    procedure UpdateChildren;
  end;

var
  FMain: TFMain;

implementation

uses JupiterFormTabSheet, LCLType, LCLProc, uJupiterDesktopAppScript;

{$R *.lfm}

{ TFMain }

procedure TFMain.jtMainTabCloseTabClicked(Sender: TObject);
begin
  jtMainTab.CloseTab(jtMainTab.ActivePageIndex);
end;

procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  while jtMainTab.PageCount > 0 do
    jtMainTab.CloseTab(0);
end;

procedure TFMain.acNewTabExecute(Sender: TObject);
begin
  Self.FNewTabClick := True;

  try
    vrJupiterApp.RunMacro('menu.newTab.click', TJupiterVariableList.Create);
  finally
    Self.FNewTabClick := False;
  end;
end;

procedure TFMain.acCloseTabExecute(Sender: TObject);
begin
  Self.Internal_CloseCurrentTab(Sender);
end;

procedure TFMain.acCloseAllButThisExecute(Sender: TObject);
begin
  Self.Internal_CloseAllButCurrentTab(Sender);
end;

procedure TFMain.acNextTabExecute(Sender: TObject);
begin
  Self.Internal_GoToNextTab(Sender);
end;

procedure TFMain.acPreviousTabExecute(Sender: TObject);
begin
  Self.Internal_GoToPreviousTab(Sender);
end;

procedure TFMain.acUpdateAndResizeExecute(Sender: TObject);
begin
  Self.Internal_Resize;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  inherited;

  Self.Internal_CreateComboBox;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  while jtMainTab.PageCount > 0 do
    jtMainTab.CloseTab(0);

  inherited;
end;

procedure TFMain.FormResize(Sender: TObject);
begin
  inherited;
end;

procedure TFMain.FormShow(Sender: TObject);
var
  vrVez : Integer;
begin
  inherited;

  if not vrJupiterApp.SecureMode then
    vrJupiterApp.RunMacro(TRIGGER_ONSTART, TJupiterVariableList.Create);

  if ((ParamCount > 1) and (AnsiUpperCase(ParamStr(1)) = '-MACRO')) then
    JupiterAppDesktopOpenCodeRunner(ParamStr(2));

  if ((ParamCount > 1) and (AnsiUpperCase(ParamStr(1)) = '-SCRIPT')) then
    JupiterAppDesktopOpenCodeRunner(ParamStr(2));

  for vrVez := 0 to ParamCount do
    vrJupiterApp.Params.AddVariable('Param' + IntToStr(vrVez), ParamStr(vrVez));
end;

procedure TFMain.jtMainTabChange(Sender: TObject);
var
  vrVez : Integer;
  vrForm : TForm;
begin
  for vrVez := 0 to jtMainTab.PageCount - 1 do
  begin
    if not (jtMainTab.Pages[vrVez] is TJupiterFormTabSheet) then
      Continue;

    if not Assigned(TJupiterFormTabSheet(jtMainTab.Pages[vrVez]).Form) then
      Continue;

    vrForm := TJupiterFormTabSheet(jtMainTab.Pages[vrVez]).Form;

    if not (vrForm is TFJupiterForm) then
      Continue;

    if jtMainTab.ActivePageIndex = vrVez then
      TFJupiterForm(vrForm).Resume
    else
      TFJupiterForm(vrForm).Pause;
  end;
end;

procedure TFMain.jtMainTabCloseTab(Sender: TObject);
begin

end;

procedure TFMain.jtMainTabResize(Sender: TObject);
var
  vrVez : Integer;
  vrForm : TForm;
begin
  for vrVez := 0 to jtMainTab.PageCount - 1 do
  begin
    if not (jtMainTab.Pages[vrVez] is TJupiterFormTabSheet) then
      Continue;

    if not Assigned(TJupiterFormTabSheet(jtMainTab.Pages[vrVez]).Form) then
      Continue;

    vrForm := TJupiterFormTabSheet(jtMainTab.Pages[vrVez]).Form;

    if not (vrForm is TFJupiterForm) then
      Continue;

    TFJupiterForm(vrForm).UpdateForm(False, True, False);
  end;
end;

procedure TFMain.pmTabOptionsPopup(Sender: TObject);
begin
  Self.Internal_CreatePopMenuTab;
end;

procedure TFMain.sbStatusDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
begin
  if Panel = sbStatus.Panels[0] then
    Self.FComboBox.SetBounds(Rect.Left + 2, Rect.Top + 2, Rect.Right - Rect.Left - 4, Rect.Bottom - Rect.Top - 4);
end;

procedure TFMain.Internal_MenuGoToTabClick(Sender: TObject);
begin
  if not (Sender is TMenuItem) then
    Exit;

  jtMainTab.PageIndex := TMenuItem(Sender).Tag;
  jtMainTabChange(Sender);
end;

procedure TFMain.Internal_OnGlobalException(Sender: TObject; E: Exception);
begin
  ShowMessage('Erro: ' + E.Message + #13#10 + 'Origem: ' + Sender.ClassName);
end;

function TFMain.CurrentForm: TForm;
begin
  Result := nil;

  if jtMainTab.PageCount = 0 then
    Exit;

  Result := TJupiterFormTabSheet(jtMainTab.Pages[jtMainTab.PageIndex]).Form;
end;

procedure TFMain.Internal_PrepareForm;
var
  vrMainMenu : TJupiterMainMenuGenerator;
begin
  inherited Internal_PrepareForm;

  Application.OnException := @Self.Internal_OnGlobalException;

  Self.FNewTabClick := False;

  TJupiterDesktopApp(vrJupiterApp).ImageList := ilIconFamily;
  TJupiterDesktopApp(vrJupiterApp).GetExternalImages;
                               {
  tmrAutoUpdater.Interval := FORM_UPDATE_TIME_MILISECONDS;

  if vrJupiterApp.Params. VariableById('Interface.PerformanceMode').AsBool then
    tmrAutoUpdater.Interval := FORM_UPDATE_TIME_MILISECONDS_LOW;
                                }
  Self.Internal_CreateShortcutList;

  vrMainMenu := TJupiterMainMenuGenerator.Create(vrJupiterApp.InternalDatabase);
  try
    vrMainMenu.MainMenu := mmMainMenu;
    vrMainMenu.Render;
  finally
    FreeAndNil(vrMainMenu);
  end;
end;

procedure TFMain.Internal_UpdateComponents;
var
  vrVez : Integer;
begin
  inherited Internal_UpdateComponents;

  Self.Caption := vrJupiterApp.AppName;

  sbStatus.Font.Size := 9;
  sbStatus.Panels[0].Width := PercentOfScreen(Self.Width, 30);

  for vrVez := 0 to jtMainTab.PageCount - 1 do
  begin
    if not (jtMainTab.Page[vrVez] is TJupiterFormTabSheet) then
      Continue;

    if Length((jtMainTab.Page[vrVez] as TJupiterFormTabSheet).Form.Caption) > vrJupiterApp.Params.VariableById(FORM_DESCRIPTION_MAXSIZE).AsInteger then
      jtMainTab.Page[vrVez].Caption := Copy((jtMainTab.Page[vrVez] as TJupiterFormTabSheet).Form.Caption, 1, vrJupiterApp.Params.VariableById(FORM_DESCRIPTION_MAXSIZE).AsInteger) + '...        '
    else
      jtMainTab.Page[vrVez].Caption := (jtMainTab.Page[vrVez] as TJupiterFormTabSheet).Form.Caption + '        ';

    if (jtMainTab.Page[vrVez] as TJupiterFormTabSheet).Form.Hint <> EmptyStr then
    begin
      jtMainTab.Page[vrVez].Hint := (jtMainTab.Page[vrVez] as TJupiterFormTabSheet).Form.Hint;
      jtMainTab.Page[vrVez].ShowHint := True;
    end;
  end;

  jtMainTab.Align := alClient;
  jtMainTab.Visible := jtMainTab.PageCount > 0;

  if not vrJupiterApp.SecureMode then
    vrJupiterApp.ScriptLineList.ExecuteNext;
end;

procedure TFMain.Internal_UpdateCalcs;
begin
  inherited Internal_UpdateCalcs;

  if not vrJupiterApp.SecureMode then
    if vrJupiterApp.Params.Exists(TRIGGER_ONUPDATE) then
      if vrJupiterApp.Params.VariableById(TRIGGER_ONUPDATE).Value <> '' then
        vrJupiterApp.RunMacroNoMessage(vrJupiterApp.Params.VariableById(TRIGGER_ONUPDATE).Value, TJupiterVariableList.Create);

  if not vrJupiterApp.SecureMode then
    TJupiterDesktopApp(vrJupiterApp).ExecutePeriodicTasks;
end;

procedure TFMain.Internal_CreatePopMenuTab;
var
  vrMenuItem : TMenuItem;
  vrVez : Integer;
begin
  pmTabOptions.Items.Clear;

  if jtMainTab.PageCount = 0 then
    Exit;

  for vrVez := 0 to jtMainTab.PageCount - 1 do
  begin
    vrMenuItem := TMenuItem.Create(pmTabOptions);
    vrMenuItem.Caption := jtMainTab.Page[vrVez].Caption;
    vrMenuItem.Tag := vrVez;
    vrMenuItem.Checked := vrVez = jtMainTab.PageIndex;
    vrMenuItem.OnClick := @Internal_MenuGoToTabClick;

    pmTabOptions.Items.Add(vrMenuItem);
  end;

  vrMenuItem := TMenuItem.Create(pmTabOptions);
  vrMenuItem.Caption := '-';
  pmTabOptions.Items.Add(vrMenuItem);

  vrMenuItem := TMenuItem.Create(pmTabOptions);
  vrMenuItem.Caption := 'Próxima aba';
  vrMenuItem.ShortCut := TextToShortCut('Ctrl+Tab');
  vrMenuItem.OnClick := @Internal_GoToNextTab;
  vrMenuItem.ImageIndex := ICON_RIGHT;
  pmTabOptions.Items.Add(vrMenuItem);

  vrMenuItem := TMenuItem.Create(pmTabOptions);
  vrMenuItem.Caption := 'Aba anterior';
  vrMenuItem.ShortCut := TextToShortCut('Ctrl+Shift+Tab');
  vrMenuItem.OnClick := @Internal_GoToPreviousTab;
  vrMenuItem.ImageIndex := ICON_LEFT;
  pmTabOptions.Items.Add(vrMenuItem);

  vrMenuItem := TMenuItem.Create(pmTabOptions);
  vrMenuItem.Caption := 'Fechar aba';
  vrMenuItem.ShortCut := TextToShortCut('Ctrl+F4');
  vrMenuItem.OnClick := @Internal_CloseCurrentTab;
  vrMenuItem.ImageIndex := ICON_CANCEL;
  pmTabOptions.Items.Add(vrMenuItem);

  vrMenuItem := TMenuItem.Create(pmTabOptions);
  vrMenuItem.Caption := 'Mover aba para a esquerda';
  vrMenuItem.OnClick := @Internal_MoveLeftTab;
  vrMenuItem.Enabled := jtMainTab.PageIndex <> 0;
  vrMenuItem.ImageIndex := ICON_UP;
  pmTabOptions.Items.Add(vrMenuItem);

  vrMenuItem := TMenuItem.Create(pmTabOptions);
  vrMenuItem.Caption := 'Mover aba para a direita';
  vrMenuItem.OnClick := @Internal_MoveRightTab;
  vrMenuItem.Enabled := jtMainTab.PageIndex < (jtMainTab.PageCount - 1);
  vrMenuItem.ImageIndex := ICON_DOWN;
  pmTabOptions.Items.Add(vrMenuItem);

  vrMenuItem := TMenuItem.Create(pmTabOptions);
  vrMenuItem.Caption := 'Abrir formulário como modal';
  vrMenuItem.OnClick := @Internal_OpenAsModal;
  vrMenuItem.Enabled := jtMainTab.PageCount > 0;
  vrMenuItem.ImageIndex := ICON_APPLICATION;
  pmTabOptions.Items.Add(vrMenuItem);

  vrMenuItem := TMenuItem.Create(pmTabOptions);
  vrMenuItem.Caption := 'Fechar todas as abas, exceto essa';
  vrMenuItem.ShortCut := TextToShortCut('Ctrl+Shift+F4');
  vrMenuItem.OnClick := @Internal_CloseAllButCurrentTab;
  vrMenuItem.ImageIndex := ICON_CANCEL;
  pmTabOptions.Items.Add(vrMenuItem);
end;

function TFMain.Internal_IsMainPage: Boolean;
begin
  Result := True;
end;

procedure TFMain.Internal_Resize;
var
  vrVez : Integer;
begin
  inherited Internal_Resize;

  if jtMainTab.Visible then
    jtMainTab.Invalidate;
end;

procedure TFMain.UpdateChildren;
var
  vrVez : Integer;
begin
  for vrVez := 0 to jtMainTab.PageCount - 1 do
  begin
    if not Assigned(jtMainTab.Pages[vrVez]) then
      Continue;

    if not (jtMainTab.Pages[vrVez] is TJupiterFormTabSheet) then
      Continue;

    if not Assigned(TJupiterFormTabSheet(jtMainTab.Pages[vrVez]).Form) then
      Continue;

    TFJupiterForm(TJupiterFormTabSheet(jtMainTab.Pages[vrVez]).Form).UpdateForm();
  end;
end;

procedure TFMain.Internal_CreateComboBox;
begin
  Self.FComboBox := TComboBox.Create(Self);
  Self.FComboBox.Parent := sbStatus;
  Self.FComboBox.Style := csDropDownList;

  jtMainTab.ComboBox := Self.FComboBox;
end;

procedure TFMain.Internal_CloseCurrentTab(Sender: TObject);
begin
  jtMainTab.CloseTab(jtMainTab.PageIndex);
end;

procedure TFMain.Internal_MoveLeftTab(Sender: TObject);
begin
  jtMainTab.Pages[jtMainTab.PageIndex].PageIndex := jtMainTab.Pages[jtMainTab.PageIndex].PageIndex - 1;
end;

procedure TFMain.Internal_MoveRightTab(Sender: TObject);
begin
  jtMainTab.Pages[jtMainTab.PageIndex].PageIndex := jtMainTab.Pages[jtMainTab.PageIndex].PageIndex + 1;
end;

procedure TFMain.Internal_CloseAllButCurrentTab(Sender: TObject);
var
  vrTab : TTabSheet;
  vrVez : Integer;
begin
  tmrAutoUpdater.Enabled := False;

  vrTab := jtMainTab.Pages[jtMainTab.PageIndex];

  vrVez := 0;

  while jtMainTab.PageCount > 1 do
  begin
    if jtMainTab.Pages[vrVez] = vrTab then
      vrVez := vrVez + 1;

    jtMainTab.CloseTab(vrVez);
    vrVez := 0;
  end;

  jtMainTabChange(Sender);

  tmrAutoUpdater.Enabled := True;
end;

procedure TFMain.Internal_GoToNextTab(Sender: TObject);
begin
  if (jtMainTab.PageIndex + 1) >= jtMainTab.PageCount then
  begin
    jtMainTab.PageIndex := 0;
    jtMainTabChange(Sender);

    Exit;
  end;

  jtMainTab.PageIndex := jtMainTab.PageIndex + 1;
  jtMainTabChange(Sender);
end;

procedure TFMain.Internal_GoToPreviousTab(Sender: TObject);
begin
  if (jtMainTab.PageIndex - 1) <= 0 then
  begin
    jtMainTab.PageIndex := jtMainTab.PageCount - 1;
    jtMainTabChange(Sender);

    Exit;
  end;

  jtMainTab.PageIndex := jtMainTab.PageIndex + 1;
  jtMainTabChange(Sender);
end;

procedure TFMain.Internal_OpenAsModal(Sender: TObject);
begin
  if not (jtMainTab.Pages[jtMainTab.PageIndex] is TJupiterFormTabSheet) then
    Exit;

  with TJupiterFormTabSheet(jtMainTab.Pages[jtMainTab.PageIndex]) do
  begin
    Form.Parent := nil;
    Form.BorderStyle := bsSizeable;

    Form := nil;
  end;

  jtMainTab.CloseTab(jtMainTab.PageIndex);
end;

procedure TFMain.NewTab(Form: TForm);
var
  vrSS : TShiftState;
  vrModal : Boolean;
begin
  vrSS := GetKeyShiftState;

  vrModal := False;

  if ((ssCtrl in vrSS) and (not FNewTabClick)) then
    vrModal := True;

  if Form is TFContextMenu then
    vrModal := True;

  if Form is TFQuickJump then
    vrModal := True;

  if vrModal then
  begin
    Form.ShowModal;

    Exit;
  end;

  Form.Align       := alClient;
  Form.WindowState := wsMaximized;

  if Assigned(Self.CurrentForm) then
  begin
    if ((Self.CurrentForm is TFMenuNavigator) and (TFMenuNavigator(Self.CurrentForm).ClickItem)) then
    begin
      TFMenuNavigator(Self.CurrentForm).AddForm(Form);

      Exit;
    end;

    if ((Self.CurrentForm is TFDatabaseFinder) and (TFDatabaseFinder(Self.CurrentForm).ClickItem)) then
    begin
      TFDatabaseFinder(Self.CurrentForm).AddForm(Form);

      Exit;
    end;
  end;

  jtMainTab.Visible := True;
  jtMainTab.AddForm(Form);

  if Form is TFJupiterForm then
    TFJupiterForm(Form).OwnerTab := jtMainTab.Pages[jtMainTab.PageCount - 1] as TJupiterFormTabSheet;

  jtMainTabChange(Self);
end;

end.

