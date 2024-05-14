unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Menus, PopupNotifier, Buttons, JupiterApp, JupiterRoute,
  JupiterConsts, JupiterObject, JupiterForm, JupiterAction, JupiterEnviroment,
  JupiterRunnable, jupiterformutils, JupiterFileDataProvider, jupiterScript,
  JupiterDirectoryDataProvider, JupiterVariable, jupitershortcut,
  jupiterScriptFunctions, JupiterToolsModule, jupiterdatabase, jupiterappmodule,
  JupiterFormTab, SQLDB, uPSComponent_Default, LMessages, PairSplitter,
  ActnList, ButtonPanel, EditBtn;

type

  { TFMain }

  TFMain = class(TFJupiterForm)
    acPrompt: TAction;
    acShortcuts: TActionList;
    acMenu: TAction;
    acChangeSearchAction: TAction;
    acCtrlEnter: TAction;
    acChangeTab: TAction;
    acCloseTab: TAction;
    acIncFontSize: TAction;
    acDecFontSize: TAction;
    ApplicationProperties1: TApplicationProperties;
    cbNavigationMenu: TCoolBar;
    cbNavigation: TComboBox;
    edSearch: TEdit;
    ilIconFamily: TImageList;
    ilTabs: TImageList;
    JupiterFormTab1: TJupiterFormTab;
    miChart: TMenuItem;
    miStartAutoUpdaterCMD: TMenuItem;
    miNotepadPlugin: TMenuItem;
    Separator8: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    miEditorSQL: TMenuItem;
    miShortcut: TMenuItem;
    pnBody: TPanel;
    ppTabsMenu: TPopupMenu;
    Separator7: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Separator6: TMenuItem;
    miSearchMode: TMenuItem;
    miNewCheckList: TMenuItem;
    miMaximizedForms: TMenuItem;
    miPastasJupiter: TMenuItem;
    ppNotifier: TPopupNotifier;
    Separator5: TMenuItem;
    miAutoUpdate: TMenuItem;
    miClearSearch: TMenuItem;
    miGenerator: TMenuItem;
    Separator4: TMenuItem;
    miDecFontSize: TMenuItem;
    miIncFontSize: TMenuItem;
    miUpdate: TMenuItem;
    miNew: TMenuItem;
    miCurrentTaskStartTime: TMenuItem;
    miCurrentTaskEndTime: TMenuItem;
    miMessage: TMenuItem;
    miConfig: TMenuItem;
    miExit: TMenuItem;
    Separator3: TMenuItem;
    Separator2: TMenuItem;
    miNewTask: TMenuItem;
    miOpen: TMenuItem;
    miOpenCSV: TMenuItem;
    miOpenCurrentTask: TMenuItem;
    Separator1: TMenuItem;
    miFormParams: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    mmMainMenu: TMainMenu;
    pnMenu: TPanel;
    spSplitter: TSplitter;
    tbHome: TToolButton;
    tbMenu: TToolButton;
    tbOptions: TToolBar;
    TbSystemBar: TToolButton;
    tmPopupEnd: TTimer;
    tmInternalThread: TTimer;
    tmSearch: TTimer;
    tbSearch: TToolBar;
    tbSystemButtons: TToolBar;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    tbMessage: TToolButton;
    ToolButton2: TToolButton;
    tbPrompt: TToolButton;
    ToolButton3: TToolButton;
    tvMenu: TTreeView;
    procedure acChangeSearchActionExecute(Sender: TObject);
    procedure acChangeTabExecute(Sender: TObject);
    procedure acCloseTabExecute(Sender: TObject);
    procedure acDecFontSizeExecute(Sender: TObject);
    procedure acIncFontSizeExecute(Sender: TObject);
    procedure acMenuExecute(Sender: TObject);
    procedure acPromptExecute(Sender: TObject);
    procedure acCtrlEnterExecute(Sender: TObject);
    procedure acCustomShortcut(Sender: TObject);
    procedure ApplicationProperties1Activate(Sender: TObject);
    procedure ApplicationProperties1Restore(Sender: TObject);
    procedure cbNavigationMenuChange(Sender: TObject);
    procedure edSearchChange(Sender: TObject);
    procedure edSearchChangeBounds(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure JupiterFormTab1Change(Sender: TObject);
    procedure JupiterFormTab1ChangeFromComboBox(Sender: TObject);
    procedure JupiterFormTab1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure JupiterFormTab1CloseTab(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure miChartClick(Sender: TObject);
    procedure miStartAutoUpdaterClick(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure MenuItem21Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure miEditorSQLClick(Sender: TObject);
    procedure miNotepadPluginClick(Sender: TObject);
    procedure miSearchModeClick(Sender: TObject);
    procedure miAutoUpdateClick(Sender: TObject);
    procedure miClearSearchClick(Sender: TObject);
    procedure miPastaAssetsClick(Sender: TObject);
    procedure miDecFontSizeClick(Sender: TObject);
    procedure miIncFontSizeClick(Sender: TObject);
    procedure miMaximizedFormsClick(Sender: TObject);
    procedure miPastasJupiterClick(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure miPastasModulesClick(Sender: TObject);
    procedure miPastasDatasetsClick(Sender: TObject);
    procedure miPastasTempClick(Sender: TObject);
    procedure miConfigClick(Sender: TObject);
    procedure miCurrentTaskEndTimeClick(Sender: TObject);
    procedure miCurrentTaskStartTimeClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miGeneratorClick(Sender: TObject);
    procedure miNewTaskClick(Sender: TObject);
    procedure miNewCheckListClick(Sender: TObject);
    procedure miOpenCSVClick(Sender: TObject);
    procedure miOpenCurrentTaskClick(Sender: TObject);
    procedure miStartAutoUpdaterCMDClick(Sender: TObject);
    procedure miUpdateClick(Sender: TObject);
    procedure pnBodyClick(Sender: TObject);
    procedure ppNotifierClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure tbHomeClick(Sender: TObject);
    procedure tbMenuClick(Sender: TObject);
    procedure tbMessageClick(Sender: TObject);
    procedure tbPromptClick(Sender: TObject);
    procedure tmInternalThreadTimer(Sender: TObject);
    procedure tmPopupEndTimer(Sender: TObject);
    procedure tmSearchTimer(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure tvMenuChange(Sender: TObject; Node: TTreeNode);
    procedure tvMenuClick(Sender: TObject);
    procedure menuFolderClick(Sender: TObject);
    procedure MenuRouteListClick(Sender: TObject);
  private
    FPrepared : Boolean;

    FSearchMode    : TJupiterSearchMode;
    FMenuRouteList : TJupiterMenuRouteList;

    procedure Internal_ListMenuItens;
    procedure Internal_GetExternalIcons;

    procedure Internal_ShowRoute(prRoute : TJupiterRoute; prList : TJupiterObjectList; prNode : TTreeNode);
    procedure Internal_MessagesCountSetValue(prID, prNewValue : String);
    procedure Internal_CurrentStatusSetValue(prID, prNewValue : String);
    procedure Internal_CurrentMessageStatusSetValue(prID, prNewValue : String);
    procedure Internal_CurrentFormTitleSetValue(prID, prNewValue : String);
    function  Internal_GoToPageItem(prItemRouteForm : String) : Boolean;

    procedure Internal_OnBeforeNavigate(Sender: TObject);
    procedure Internal_OnAfterNavigate(Sender: TObject);
    procedure Internal_OnShowPopup(Sender: TObject);
    procedure Internal_CallFormAction(prActionIndex : Integer);
  protected
    procedure Internal_UpdateComponents; override;
    procedure Internal_PrepareForm; override;
    procedure Internal_LoadDirectoryStructure(prDirectory : String; prAfterThan : TMenuItem);
    procedure Internal_PrepareMainMenu;
    procedure Internal_PrepareShortCuts;
    function  Internal_ListAllShortcuts : String; override;
    procedure Internal_Activate; override;
  published
    property MenuRouteList : TJupiterMenuRouteList read FMenuRouteList write FMenuRouteList;
  public

  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}

uses LCLType, JupiterDialogForm, StrUtils, JupiterStandardModule, JupiterFormTabSheet;

{ TFMain }

procedure TFMain.tbHomeClick(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(ROOT_FORM_PATH), False);
end;

procedure TFMain.tbMenuClick(Sender: TObject);
begin
  try
    pnMenu.Visible := not pnMenu.Visible;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFMain.tbMessageClick(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(MESSAGES_PATH), True);
end;

procedure TFMain.tbPromptClick(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(PROMPT_FORM_PATH), True);
end;

procedure TFMain.tmInternalThreadTimer(Sender: TObject);
begin
  tmInternalThread.Enabled := False;
  try
    if vrJupiterApp.Params.Exists('Jupiter.Standard.Triggers.OnExecuteCurrentThread') then
          if Trim(vrJupiterApp.Params.VariableById('Jupiter.Standard.Triggers.OnExecuteCurrentThread').Value) <> EmptyStr then
             vrJupiterApp.Threads.NewThread('Gatilho: Thread interna', TJupiterRunnable.Create(vrJupiterApp.Params.VariableById('Jupiter.Standard.Triggers.OnExecuteCurrentThread').Value));
  finally
    tmInternalThread.Enabled := True;
  end;
end;

procedure TFMain.tmPopupEndTimer(Sender: TObject);
begin
  if ppNotifier.Visible then
    ppNotifier.Hide;

  tmPopupEnd.Enabled := False;
end;

procedure TFMain.tmSearchTimer(Sender: TObject);
begin
  if Self.FSearchMode = jsmForm then
  begin
    if Assigned(vrJupiterApp.CurrentForm) then
      vrJupiterApp.CurrentForm.Search(edSearch.Text);
  end
  else
  begin
    if Trim(edSearch.Text) <> EmptyStr then
    begin
      Self.Internal_ListMenuItens;

      SearchOnTreeView(tvMenu, edSearch.Text);
    end;
  end;

  tmSearch.Enabled := False;
end;

procedure TFMain.ToolButton1Click(Sender: TObject);
begin
  Self.IsModal := True;

  Self.UpdateForm;
end;

procedure TFMain.ToolButton2Click(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(CONFIG_PATH), True);
end;

procedure TFMain.ToolButton3Click(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(MENU_SELECT_FORM_PATH), True);
end;

procedure TFMain.tvMenuChange(Sender: TObject; Node: TTreeNode);
var
  vrAction : TJupiterAction;
begin
  if not Assigned(tvMenu.Selected) then
    Exit;

  if not Assigned(tvMenu.Selected.Data) then
    Exit;

  vrAction := TJupiterAction(tvMenu.Selected.Data);

  if Assigned(vrAction.Route) then
    vrJupiterApp.NavigateTo(vrAction.Route, False);
end;

procedure TFMain.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
begin

end;

procedure TFMain.cbNavigationMenuChange(Sender: TObject);
begin

end;

procedure TFMain.ApplicationProperties1Restore(Sender: TObject);
begin
  if ((miAutoUpdate.Checked) and (Self.FPrepared)) then
  begin
    Self.Activate;

    Self.UpdateForm;

    if vrJupiterApp.Params.Exists('Jupiter.Standard.Triggers.OnUpdate') then
      if Trim(vrJupiterApp.Params.VariableById('Jupiter.Standard.Triggers.OnUpdate').Value) <> EmptyStr then
        vrJupiterApp.Threads.NewThread('Gatilho: Ao atualizar a aplicação', TJupiterRunnable.Create(vrJupiterApp.Params.VariableById('Jupiter.Standard.Triggers.OnUpdate').Value));
  end;
end;

procedure TFMain.ApplicationProperties1Activate(Sender: TObject);
begin

end;

procedure TFMain.acPromptExecute(Sender: TObject);
begin
  tbPrompt.Click;
end;

procedure TFMain.acCtrlEnterExecute(Sender: TObject);
begin
  ToolButton3.Click;
end;

procedure TFMain.acCustomShortcut(Sender: TObject);
var
  vrShortcut : TJupiterShortcut;
  vrStr : TStrings;
begin
  if not (Sender is TAction) then
    Exit;

  vrShortcut := vrJupiterApp.Shortcuts.GetShortcutByIndex(TAction(Sender).Tag);

  if vrShortcut.FileName <> EmptyStr then
  begin
    TJupiterRunnable.Create(vrShortcut.FileName, True);

    Exit;
  end;

  if vrShortcut.CodeRun <> EmptyStr then
  begin
    vrStr := TStringList.Create;
    try
      vrStr.Clear;
      vrStr.Add(vrShortcut.CodeRun);

      vrJupiterApp.RunScript(vrStr);
    finally
      vrStr.Clear;
      FreeAndNil(vrStr);
    end;
  end;
end;

procedure TFMain.acMenuExecute(Sender: TObject);
begin
  tbMenu.Click;
end;

procedure TFMain.acChangeSearchActionExecute(Sender: TObject);
begin
  miSearchMode.Click;

  if edSearch.CanFocus then
    edSearch.SetFocus;
end;

procedure TFMain.acChangeTabExecute(Sender: TObject);
var
  vrNextTab : Integer;
begin
  vrNextTab := JupiterFormTab1.ActivePageIndex + 1;

  if vrNextTab > (JupiterFormTab1.PageCount - 1) then
    vrNextTab := 0;

  JupiterFormTab1.ActivePageIndex := vrNextTab;
end;

procedure TFMain.acCloseTabExecute(Sender: TObject);
begin
  MenuItem20.Click;
end;

procedure TFMain.acDecFontSizeExecute(Sender: TObject);
var
  vrStr : TStrings;
begin
  vrStr := TStringList.Create;
  try
    miDecFontSize.Click;
  finally
    vrStr.Clear;
    vrStr.Add('Novo tamanho de fonte: ' + vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);

    vrJupiterApp.Popup('Tamanho de fonte alterada', vrStr);
  end;
end;

procedure TFMain.acIncFontSizeExecute(Sender: TObject);
var
  vrStr : TStrings;
begin
  vrStr := TStringList.Create;
  try
    miIncFontSize.Click;
  finally
    vrStr.Clear;
    vrStr.Add('Novo tamanho de fonte: ' + vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);

    vrJupiterApp.Popup('Tamanho de fonte alterada', vrStr);
  end;
end;

procedure TFMain.edSearchChange(Sender: TObject);
begin
  tmSearch.Enabled := False;
  tmSearch.Enabled := True;
end;

procedure TFMain.edSearchChangeBounds(Sender: TObject);
begin

end;

procedure TFMain.FormActivate(Sender: TObject);
begin
  inherited;

  vrJupiterApp.Ready := True;

  Self.FPrepared := True;

  FormResize(Sender);

  ApplicationProperties1Restore(Sender);
end;

procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  tmInternalThread.Enabled := False;
  tmPopupEnd.Enabled := False;
  tmSearch.Enabled := False;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  Self.FPrepared := False;

  inherited;

  Self.FMenuRouteList := TJupiterMenuRouteList.Create;
  Self.FMenuRouteList.MainMenu := mmMainMenu;

  Self.FSearchMode := jsmForm;

  Self.Internal_LoadDirectoryStructure(EmptyStr, MenuItem4);

  if TJupiterStandardModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Standard')).HideMenuTree then
    pnMenu.Visible := False;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Self.FMenuRouteList);

  inherited;
end;

procedure TFMain.FormResize(Sender: TObject);
begin
  cbNavigationMenu.Bands[1].Width := PercentOfScreen(Width, 30);
  cbNavigationMenu.Bands[2].Width := PercentOfScreen(Width, 30);

  if Assigned(vrJupiterApp.CurrentForm) then
  begin
    vrJupiterApp.CurrentForm.Repaint;
    vrJupiterApp.CurrentForm.WindowState := wsNormal;
    vrJupiterApp.CurrentForm.WindowState := wsMaximized;

    Application.ProcessMessages;
  end;
end;

procedure TFMain.FormShow(Sender: TObject);
begin
  Self.IsModal := True;

  inherited;

  MenuItem10.Click;

  vrJupiterApp.MainIcons := ilIconFamily;

  if TJupiterStandardModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Standard')).GetUserStartRoute <> EmptyStr then
  begin
    if not Self.Internal_GoToPageItem(TJupiterStandardModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Standard')).GetUserStartRoute) then
      vrJupiterApp.NavigateTo(TJupiterRoute.Create(ROOT_FORM_PATH), False);
  end
  else
    vrJupiterApp.NavigateTo(TJupiterRoute.Create(ROOT_FORM_PATH), False);

  miNewCheckList.OnClick := @miNewCheckListClick;

  if vrJupiterApp.Params.Exists('Jupiter.Standard.Triggers.OnStart') then
    if Trim(vrJupiterApp.Params.VariableById('Jupiter.Standard.Triggers.OnStart').Value) <> EmptyStr then
      vrJupiterApp.Threads.NewThread('Gatilho: Ao iniciar a aplicação', TJupiterRunnable.Create(vrJupiterApp.Params.VariableById('Jupiter.Standard.Triggers.OnStart').Value));

  if vrJupiterApp.Params.Exists('Jupiter.Standard.Triggers.OnExecuteCurrentThread') then
        if Trim(vrJupiterApp.Params.VariableById('Jupiter.Standard.Triggers.OnExecuteCurrentThread').Value) <> EmptyStr then
           tmInternalThread.Enabled := True;
end;

procedure TFMain.JupiterFormTab1Change(Sender: TObject);
begin
  try
    if Sender is TJupiterFormTab then
      vrJupiterApp.CurrentForm := TFJupiterForm(TJupiterFormTabSheet(TJupiterFormTab(Sender).ActivePage).Form);

    if Sender is TJupiterFormTabSheet then
      vrJupiterApp.CurrentForm := TFJupiterForm(TJupiterFormTabSheet(Sender).Form);

    if Assigned(vrJupiterApp.CurrentForm) then
      vrJupiterApp.CurrentForm.Activate;

    if miClearSearch.Checked then
      edSearch.Text := EmptyStr
    else
      if Assigned(vrJupiterApp.CurrentForm) then
        vrJupiterApp.CurrentForm.Search(edSearch.Text);
  finally
    Self.UpdateForm();
  end;
end;

procedure TFMain.JupiterFormTab1ChangeFromComboBox(Sender: TObject);
begin
  JupiterFormTab1Change(Sender);
end;

procedure TFMain.JupiterFormTab1Changing(Sender: TObject; var AllowChange: Boolean);
begin
  AllowChange := True;
end;

procedure TFMain.JupiterFormTab1CloseTab(Sender: TObject);
begin
  JupiterFormTab1Change(Sender);
end;

procedure TFMain.MenuItem10Click(Sender: TObject);
begin
  pnMenu.Width := PercentOfScreen(Self.Width, 30);
end;

procedure TFMain.MenuItem11Click(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(CLI_MANAGER_PATH), True);
end;

procedure TFMain.MenuItem12Click(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(LAYOUT_READER_PATH), True);
end;

procedure TFMain.MenuItem13Click(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(TIME_CONTROL_PATH), True);
end;

procedure TFMain.MenuItem14Click(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(TIME_CONTROL_PATH), True);
end;

procedure TFMain.MenuItem15Click(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(USER_PREF_PATH), True);
end;

procedure TFMain.MenuItem17Click(Sender: TObject);
var
  vrRoute : TJupiterRoute;
begin
  vrRoute := TJupiterRoute.Create(GENERATOR_FORM_PATH);
  vrRoute.Params.AddVariable('Goto', 'ReleaseNotes', 'Ir para');

  vrJupiterApp.NavigateTo(vrRoute, True);
end;

procedure TFMain.MenuItem18Click(Sender: TObject);
var
  vrCurrentTab : TTabSheet;
  vrVez : Integer;
begin
  vrCurrentTab := JupiterFormTab1.ActivePage;

  for vrVez := JupiterFormTab1.PageCount - 1 downto 0 do
  begin
    if vrCurrentTab = JupiterFormTab1.Page[vrVez] then
      Continue;

    JupiterFormTab1.CloseTab(vrVez);
  end;
end;

procedure TFMain.MenuItem19Click(Sender: TObject);
begin
  acChangeTab.Execute;
end;

procedure TFMain.MenuItem1Click(Sender: TObject);
begin

end;

procedure TFMain.miChartClick(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(CHART_VIEWER_PATH), False);
end;

procedure TFMain.miStartAutoUpdaterClick(Sender: TObject);
var
  vrDialog     : TJupiterDialogForm;
  vrEnviroment : TJupiterEnviroment;
  vrScript     : TJupiterScript;
begin
  vrDialog     := TJupiterDialogForm.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  vrScript     := TJupiterScript.Create;
  try
    vrDialog.Title := 'Novo arquivo .jpas';
    vrDialog.Hint  := 'Crie um novo arquivo programável Jupiter.';

    vrDialog.Fields.AddField('NAME', 'Nome do programa', '');
    vrDialog.Fields.AddField('PATH', 'Salvar em', vrEnviroment.FullPath('modules/tools/library/'));

    if vrDialog.Show then
      vrScript.CreateNewFile(vrDialog.Fields.VariableFormById('NAME').Value,
                             vrDialog.Fields.VariableFormById('PATH').Value);

    Self.UpdateForm;
  finally
    FreeAndNil(vrDialog);
    FreeAndNil(vrEnviroment);
    FreeAndNil(vrScript);
  end;
end;

procedure TFMain.MenuItem20Click(Sender: TObject);
begin
  if JupiterFormTab1.PageCount <= 1 then
    Exit;

  JupiterFormTab1.CloseTab(JupiterFormTab1.ActivePageIndex);
end;

procedure TFMain.MenuItem21Click(Sender: TObject);
var
  vrRoute : TJupiterRoute;
begin
  vrRoute := TJupiterRoute.Create(WEB_EXPLORER_FORM_PATH);
  vrRoute.Params.AddVariable('destiny', 'https://github.com/FernandoDuwe/jupiterTools', 'Destino');

  vrJupiterApp.NavigateTo(vrRoute, True);
end;

procedure TFMain.MenuItem6Click(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
  vrFile       : String;
  vrRoute      : TJupiterRoute;
begin
  vrEnviroment := TJupiterEnviroment.Create();
  try
    vrEnviroment.BasePath := vrJupiterApp.Params.VariableById('Enviroment.WorkDir').Value;

    vrFile := vrEnviroment.OpenFile('*.jpas');

    if vrFile <> EmptyStr then
    begin
      vrJupiterApp.Params.VariableById('Enviroment.WorkDir').Value := ExtractFileDir(vrFile);

      vrRoute := TJupiterRoute.Create(SCRIPTEDITOR_FORM_PATH);

      vrRoute.Params.AddVariable('title', 'Arquivo: ' + ExtractFileName(vrFile), 'Título');
      vrRoute.Params.AddVariable('filename', vrFile, 'Arquivo');

      vrJupiterApp.NavigateTo(vrRoute, False);
    end;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFMain.MenuItem8Click(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(PROCESS_MONITOR_PATH), True);
end;

procedure TFMain.MenuItem9Click(Sender: TObject);
var
  vrDialog     : TJupiterDialogForm;
  vrStr        : TStrings;
  vrEnviroment : TJupiterEnviroment;
begin
  vrDialog     := TJupiterDialogForm.Create;
  vrStr        := TStringList.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrStr.Clear;

    vrDialog.Title := 'Novo Layout';
    vrDialog.Hint  := 'Crie um novo layout.';

    vrDialog.Fields.AddField('NAME', 'Nome do Layout', '');

    if vrDialog.Show then
    begin
      vrStr.Add('Field;Type;Start;Size;');

      vrStr.SaveToFile(vrEnviroment.FullPath('modules/tools/library/') + DirectorySeparator + vrDialog.Fields.VariableById('NAME').Value + '.jlt');

      Self.UpdateForm;
    end;
  finally
    FreeAndNil(vrDialog);
    FreeAndNil(vrStr);
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFMain.miEditorSQLClick(Sender: TObject);
var
  vrRoute : TJupiterRoute;
begin
  vrRoute := TJupiterRoute.Create(SQLEDITOR_FORM_PATH);
  vrRoute.Params.AddVariable('Timestamp', FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));

  vrJupiterApp.NavigateTo(vrRoute, False);
end;

procedure TFMain.miNotepadPluginClick(Sender: TObject);
var
  vrEnviroment  : TJupiterEnviroment;
  vrNotepadPath : String;
  vrVariable    : TJupiterVariableList;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  vrVariable   := TJupiterVariableList.Create;
  try
    vrNotepadPath := 'C:\Program Files\Notepad++\';

    if not vrEnviroment.Exists(vrNotepadPath) then
      vrNotepadPath := 'C:\Program Files (x86)\Notepad++\';

    if not vrEnviroment.Exists(vrNotepadPath) then
      raise Exception.Create('Diretório de instalação do Notepad++ não encontrado. Não é possível continuar.');

    if not vrEnviroment.Exists(vrNotepadPath + 'plugins\jupiter_notepad_plugin\') then
      CreateDir(vrNotepadPath + 'plugins\jupiter_notepad_plugin\');

    vrEnviroment.CopyFileTo(vrEnviroment.FullPath('/jupiter_notepad_plugin.dll'), vrNotepadPath + 'plugins\jupiter_notepad_plugin\jupiter_notepad_plugin.dll');

    if not vrEnviroment.Exists(vrNotepadPath + 'plugins\jupiter_notepad_plugin\jupiter_notepad_plugin.csv') then
      vrEnviroment.CreateExternalFile(vrNotepadPath + 'plugins\jupiter_notepad_plugin\jupiter_notepad_plugin.csv', 'ID;DESCRIPTION;VALUE;');

    vrVariable.FileName := vrNotepadPath + 'plugins\jupiter_notepad_plugin\jupiter_notepad_plugin.csv';
    vrVariable.AddConfig('Jupiter.Home', vrEnviroment.BasePath, 'Jupiter home path');

    Application.MessageBox('Instalação concluída com sucesso', 'Plugin Jupiter Notepad++', MB_ICONINFORMATION + MB_OK);
  finally
    FreeAndNil(vrEnviroment);
    FreeAndNil(vrVariable);
  end;
end;

procedure TFMain.miSearchModeClick(Sender: TObject);
begin
  try
    if Self.FSearchMode = jsmForm then
      Self.FSearchMode := jsmActions
    else
      Self.FSearchMode := jsmForm;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFMain.miAutoUpdateClick(Sender: TObject);
begin
  miAutoUpdate.Checked := not miAutoUpdate.Checked;
end;

procedure TFMain.miClearSearchClick(Sender: TObject);
begin
  miClearSearch.Checked := not miClearSearch.Checked;
end;

procedure TFMain.miPastaAssetsClick(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    TJupiterRunnable.Create(vrEnviroment.FullPath('assets/'), True);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFMain.miDecFontSizeClick(Sender: TObject);
begin
  try
    vrJupiterApp.Params.AddConfig('Interface.Font.Size',
                                  IntToStr(StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value) - 1),
                                  'Tamanho da fonte');
  finally
    Self.UpdateForm;
  end;
end;

procedure TFMain.miIncFontSizeClick(Sender: TObject);
begin
  try
    vrJupiterApp.Params.AddConfig('Interface.Font.Size',
                                  IntToStr(StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value) + 1),
                                  'Tamanho da fonte');
  finally
    Self.UpdateForm;
  end;
end;

procedure TFMain.miMaximizedFormsClick(Sender: TObject);
begin
  try
    if not vrJupiterApp.Params.Exists('Interface.Form.ModalShowMaximized') then
      vrJupiterApp.Params.AddConfig('Interface.Form.ModalShowMaximized', 'True', 'Mostrar formulários modais maximizados')
    else
      vrJupiterApp.Params.DeleteVariable('Interface.Form.ModalShowMaximized');
  finally
    Self.UpdateForm;
  end;
end;

procedure TFMain.miPastasJupiterClick(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    TJupiterRunnable.Create(vrEnviroment.BasePath, True);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFMain.MenuItem5Click(Sender: TObject);
var
  vrStrMessage : String;
  vrVez        : Integer;
begin
  vrStrMessage := vrJupiterApp.AppName;
  vrStrMessage := vrStrMessage + #13#10;
  vrStrMessage := vrStrMessage + 'Versão: ' + vrJupiterApp.GetVersion + #13#10;
  vrStrMessage := vrStrMessage + 'SO: ' + GetCurrentOS + #13#10;


  vrStrMessage := vrStrMessage + #13#10;
  vrStrMessage := vrStrMessage + 'Módulos instalados:' + #13#10;

  for vrVez := 0 to vrJupiterApp.ModulesList.Size - 1 do
    vrStrMessage := vrStrMessage + '  * ' + vrJupiterApp.ModulesList.GetModuleByIndex(vrVez).ModuleTitle + ' (' + vrJupiterApp.ModulesList.GetModuleByIndex(vrVez).ModuleID + ');' + #13#10;

  vrStrMessage := vrStrMessage + #13#10;
  vrStrMessage := vrStrMessage + 'SOs suportados:' + #13#10;
  vrStrMessage := vrStrMessage + '  * Windows;' + #13#10;
  vrStrMessage := vrStrMessage + '  * Linux;' + #13#10;

  vrStrMessage := vrStrMessage + #13#10;
  vrStrMessage := vrStrMessage + 'https://github.com/FernandoDuwe/jupiterTools';

  Application.MessageBox(PAnsiChar(vrStrMessage), 'Sobre', MB_ICONINFORMATION + MB_OK);
end;

procedure TFMain.miPastasModulesClick(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    TJupiterRunnable.Create(vrEnviroment.FullPath('modules/'), True);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFMain.miPastasDatasetsClick(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    TJupiterRunnable.Create(vrEnviroment.FullPath('datasets/'), True);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFMain.miPastasTempClick(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    TJupiterRunnable.Create(vrEnviroment.FullPath('temp/'), True);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFMain.miConfigClick(Sender: TObject);
begin

end;

procedure TFMain.miCurrentTaskEndTimeClick(Sender: TObject);
begin
  try
    TJupiterToolsModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools')).SetEndTime;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFMain.miCurrentTaskStartTimeClick(Sender: TObject);
begin
  try
    TJupiterToolsModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools')).SetStartTime;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFMain.miExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFMain.miGeneratorClick(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(GENERATOR_FORM_PATH), True);
end;

procedure TFMain.miNewTaskClick(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(NEWTASK_FORM_PATH), True);
end;

procedure TFMain.miNewCheckListClick(Sender: TObject);
var
  vrDialog     : TJupiterDialogForm;
  vrStr        : TStrings;
  vrEnviroment : TJupiterEnviroment;
begin
  vrDialog     := TJupiterDialogForm.Create;
  vrStr        := TStringList.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrStr.Clear;

    vrDialog.Title := 'Nova Checklist';
    vrDialog.Hint  := 'Crie uma nova checklist.';

    vrDialog.Fields.AddField('NAME', 'Nome da Checklist', '');

    if vrDialog.Show then
    begin
      vrStr.Add('Item;Checked;');

      vrStr.SaveToFile(vrEnviroment.FullPath('modules/tools/checklists/') + DirectorySeparator + vrDialog.Fields.VariableById('NAME').Value + '.ckl');

      Self.UpdateForm;
    end;
  finally
    FreeAndNil(vrDialog);
    FreeAndNil(vrStr);
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFMain.miOpenCSVClick(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
  vrFile       : String;
  vrRoute      : TJupiterRoute;
begin
  vrEnviroment := TJupiterEnviroment.Create();
  try
    vrEnviroment.BasePath := vrJupiterApp.Params.VariableById('Enviroment.WorkDir').Value;

    vrFile := vrEnviroment.OpenFile('*.csv');

    if vrFile <> EmptyStr then
    begin
      vrJupiterApp.Params.VariableById('Enviroment.WorkDir').Value := ExtractFileDir(vrFile);

      vrRoute := TJupiterRoute.Create(EXPLORER_FORM_PATH);

      vrRoute.Params.AddVariable('type', DATAPROVIDER_TYPE_LIST_CSV, 'Tipo');
      vrRoute.Params.AddVariable('path', vrFile, 'Arquivo');
      vrRoute.Params.AddVariable('hint', 'Arquivo: ' + vrFile, 'Dica');
      vrRoute.Params.AddVariable('title', 'Arquivo: ' + vrFile, 'Dica');
      vrRoute.Params.AddVariable('hideColumns', 'Line', 'Campos a esconder');

      vrJupiterApp.NavigateTo(vrRoute, False);
    end;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFMain.miOpenCurrentTaskClick(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(TASK_FORM_PATH), False);
end;

procedure TFMain.miStartAutoUpdaterCMDClick(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
  vrStr : TStrings;
begin
  vrStr        := TStringList.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrStr.Clear;
    vrStr.Add('timeout 5');
    vrStr.Add('europa autoupdater');

    vrEnviroment.CreateFile(vrEnviroment.FullPath('temp/autoUpdater.bat'), vrStr.Text);

    JupiterRunCommandLineNoWait(vrEnviroment.FullPath('temp/autoUpdater.bat'));

    Application.Terminate;
  finally
    vrStr.Clear;

    FreeAndNil(vrEnviroment);
    FreeAndNil(vrStr);
  end;
end;

procedure TFMain.miUpdateClick(Sender: TObject);
begin
  Self.IsModal := True;

  Self.UpdateForm;
end;

procedure TFMain.pnBodyClick(Sender: TObject);
begin

end;

procedure TFMain.ppNotifierClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  tmPopupEnd.Enabled := False;
end;

procedure TFMain.tvMenuClick(Sender: TObject);
begin

end;

procedure TFMain.menuFolderClick(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
    if TMenuItem(Sender).Count > 0 then
      Exit;

    TJupiterRunnable.Create(TMenuItem(Sender).Hint, True);
  end;
end;

procedure TFMain.MenuRouteListClick(Sender: TObject);
var
  vrJupiterMenuRoute : TJupiterMenuRoute;
  vrStr : TStrings;
begin
  if not (Sender is TMenuItem) then
    Exit;

  vrJupiterMenuRoute := Self.MenuRouteList.MenuRouteByMenuItem(TMenuItem(Sender));

  if not Assigned(vrJupiterMenuRoute) then
    Exit;

  if ((not vrJupiterMenuRoute.Params.Exists('command')) and (not vrJupiterMenuRoute.Params.Exists('script'))) then
    Exit;

  if ((Trim(vrJupiterMenuRoute.Params.VariableById('command').Value) = EmptyStr) and (Trim(vrJupiterMenuRoute.Params.VariableById('script').Value) = EmptyStr)) then
    Exit;

  if Trim(vrJupiterMenuRoute.Params.VariableById('command').Value) <> EmptyStr then
  begin
    vrStr := TStringList.Create;
    try
      vrStr.Clear;
      vrStr.Add(vrJupiterMenuRoute.Params.VariableById('command').Value);

      vrJupiterApp.RunScript(vrStr);
    finally
      vrStr.Clear;
      FreeAndNil(vrStr);
    end;
  end
  else
    TJupiterRunnable.Create(vrJupiterMenuRoute.Params.VariableById('script').Value, True);
end;

procedure TFMain.Internal_ListMenuItens;
var
  vrMenuList : TJupiterObjectList;
begin
  vrMenuList := vrJupiterApp.GetActions(TJupiterRoute.Create(ROOT_PATH));

  tvMenu.Items.Clear;

  ShowRouteOnTreeView(tvMenu, TJupiterRoute.Create(ROOT_PATH), vrMenuList, nil);
end;

procedure TFMain.Internal_GetExternalIcons;
var
  vrFile       : TJupiterFileDataProvider;
  vrEnviroment : TJupiterEnviroment;
  vrVez        : Integer;

  vrBitmap  : TBitmap;
  vrPicture : TPicture;
begin
  vrEnviroment := TJupiterEnviroment.Create();
  vrFile       := TJupiterFileDataProvider.Create;
  try
    vrFile.Path := vrEnviroment.FullPath('assets' + GetDirectorySeparator);
    vrFile.SubFolders := True;
    vrFile.ProvideData;

    for vrVez := 0 to vrFile.Size - 1 do
    begin
      if not vrEnviroment.IsPictureFile(vrFile.GetRowByIndex(vrVez).Fields.VariableById('File').Value) then
        Continue;

      vrPicture := TPicture.Create;
      try
        vrPicture.LoadFromFile(vrFile.GetRowByIndex(vrVez).Fields.VariableById('File').Value);

        vrBitmap := TBitmap.Create;
        vrBitmap.Assign(vrPicture.Graphic);

        ilIconFamily.Add(vrBitmap, nil);
      finally
        FreeAndNil(vrPicture);
      end;
    end;
  finally
    FreeAndNil(vrFile);
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFMain.Internal_ShowRoute(prRoute: TJupiterRoute; prList: TJupiterObjectList; prNode : TTreeNode);
var
  vrVez    : Integer;
  vrNode   : TTreeNode;
  vrAction : TJupiterAction;
begin
  for vrVez := 0 to prList.Size - 1 do
  begin
    vrAction := TJupiterAction(prList.GetAtIndex(vrVez));

    if not Assigned(vrAction.Location) then
      Continue;

    if vrAction.Location.Path <> prRoute.Path then
      Continue;

    if Assigned(prNode) then
      vrNode := tvMenu.Items.AddChild(prNode, vrAction.Title)
    else
      vrNode := tvMenu.Items.Add(nil, vrAction.Title);

    vrNode.ImageIndex := vrAction.Icon;
    vrNode.SelectedIndex := vrAction.Icon;
    vrNode.Data := vrAction;

    if Assigned(vrAction.Route) then
      Self.Internal_ShowRoute(vrAction.Route, prList, vrNode);
  end;
end;

procedure TFMain.Internal_MessagesCountSetValue(prID, prNewValue : String);
begin
  sbStatus.Panels[1].Text := 'Mensagens: ' + prNewValue;

  tbMessage.Hint := sbStatus.Panels[1].Text;
  miMessage.Hint := sbStatus.Panels[1].Text;
end;

procedure TFMain.Internal_CurrentStatusSetValue(prID, prNewValue: String);
begin
  sbStatus.Panels[0].Text := prNewValue;
end;

procedure TFMain.Internal_CurrentMessageStatusSetValue(prID, prNewValue: String);
begin
  sbStatus.Panels[2].Text := prNewValue;
end;

procedure TFMain.Internal_CurrentFormTitleSetValue(prID, prNewValue: String);
begin
  Self.Caption := Format('%0:s - %1:s', [prNewValue, vrJupiterApp.AppName]);
end;

function TFMain.Internal_GoToPageItem(prItemRouteForm: String) : Boolean;
var
  vrVez    : Integer;
  vrRoute  : String;
  vrFormId : String;
begin
  Result := False;

  for vrVez := 0 to tvMenu.Items.Count -1 do
  begin
    if not Assigned(tvMenu.Items[vrVez]) then
      Continue;

    if not Assigned(tvMenu.Items[vrVez].Data) then
      Continue;

    vrRoute  := EmptyStr;
    vrFormId := EmptyStr;

    if TJupiterAction(tvMenu.Items[vrVez].Data).Route.Params.Exists('destinyPath') then
      vrRoute := TJupiterAction(tvMenu.Items[vrVez].Data).Route.Params.VariableById('destinyPath').Value;

    if TJupiterAction(tvMenu.Items[vrVez].Data).Route.Params.Exists('Generator.FormId') then
      vrFormId := TJupiterAction(tvMenu.Items[vrVez].Data).Route.Params.VariableById('Generator.FormId').Value;

    if ((vrFormId = prItemRouteForm) or (vrRoute = prItemRouteForm)) then
    begin
      Result := True;

      tvMenu.Selected := tvMenu.Items[vrVez];

//      tvMenuChange(Self, tvMenu.Items[vrVez]);
      Exit;
    end;
  end;
end;

procedure TFMain.Internal_OnBeforeNavigate(Sender: TObject);
begin
  if miClearSearch.Checked then
    edSearch.Text := EmptyStr;
end;

procedure TFMain.Internal_OnAfterNavigate(Sender: TObject);
begin
  if edSearch.Text <> EmptyStr then
    edSearchChange(Sender);
end;

procedure TFMain.Internal_OnShowPopup(Sender: TObject);
begin
  tmPopupEnd.Enabled := True;
end;

procedure TFMain.Internal_CallFormAction(prActionIndex: Integer);
begin
  //
end;

procedure TFMain.Internal_UpdateComponents;
var
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Internal_UpdateComponents;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    if Assigned(vrJupiterApp.CurrentForm) then
      vrJupiterApp.CurrentForm.UpdateForm;

    if pnMenu.Visible then
    begin
      tbMenu.ImageIndex := ICON_LEFT;

      tbMenu.Hint := 'Esconder menu (Ctrl + L)';

      Self.Internal_ListMenuItens();
    end
    else
    begin
      tbMenu.ImageIndex := ICON_RIGHT;

      tbMenu.Hint := 'Exibir menu (Ctrl + L)';
    end;

    edSearch.TextHint := IfThen(Self.FSearchMode = jsmActions, 'Pesquisar no menu', 'Pesquisar');
    miSearchMode.Caption := IfThen(Self.FSearchMode = jsmActions, 'Pesquisa: Menu (Ctrl + S)', 'Pesquisa: Formularios (Ctrl + S)');

    miMaximizedForms.Checked := vrJupiterApp.Params.Exists('Interface.Form.ModalShowMaximized');

    with TJupiterToolsModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools')) do
    begin
      if ((Params.Exists(DefineParamName('Tasks.Current.Path'))) and
          (Params.VariableById(DefineParamName('Tasks.Current.Path')).Value <> EmptyStr)) then
      begin
        miOpenCurrentTask.Enabled      := True;
        miCurrentTaskStartTime.Enabled := not StartedTime;
        miCurrentTaskEndTime.Enabled   := StartedTime;
      end
      else
      begin
        miOpenCurrentTask.Enabled      := False;
        miCurrentTaskStartTime.Enabled := False;
        miCurrentTaskEndTime.Enabled   := False;
      end;
    end;

    FormResize(Self);

    tvMenu.Font.Size := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);

    miEditorSQL.Enabled := TJupiterDatabaseModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Database')).IsActive;
    JupiterFormTab1.Font.Size := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);

    edSearch.Font.Size := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);
    cbNavigation.Font.Size := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);

    if Assigned(vrJupiterApp.CurrentForm) then
      Self.Internal_CurrentFormTitleSetValue('', vrJupiterApp.CurrentForm.Caption);

    cbNavigation.Enabled := JupiterFormTab1.Visible;

    miNotepadPlugin.Enabled := vrEnviroment.Exists(vrEnviroment.FullPath('jupiter_notepad_plugin.dll'));

    miStartAutoUpdaterCMD.Enabled := TJupiterAppModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Application')).UpdateAvaliable;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFMain.Internal_PrepareForm;
var
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Internal_PrepareForm;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    if TJupiterStandardModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Standard')).TabMode then
    begin
      pnBody.BevelInner := bvNone;

      vrJupiterApp.JupiterFormTab := JupiterFormTab1;
    end
    else
    begin
      JupiterFormTab1.Visible := False;

      vrJupiterApp.BodyPanel := pnBody;
    end;

    vrJupiterApp.PopupNotifier    := ppNotifier;
    vrJupiterApp.OnBeforeNavigate := @Internal_OnBeforeNavigate;
    vrJupiterApp.OnAfterNavigate  := @Internal_OnAfterNavigate;
    vrJupiterApp.OnShowPopup      := @Internal_OnShowPopup;

    if vrJupiterApp.Params.Exists(vrJupiterApp.AppID + '.Messages.Count') then
    begin
      vrJupiterApp.Params.VariableById(vrJupiterApp.AppID + '.Messages.Count').OnChangeValue := @Internal_MessagesCountSetValue;

      Internal_MessagesCountSetValue(EmptyStr, IntToStr(vrJupiterApp.Messages.Size));
    end;

    if vrJupiterApp.Params.Exists(vrJupiterApp.AppID + '.State.Current') then
      vrJupiterApp.Params.VariableById(vrJupiterApp.AppID + '.State.Current').OnChangeValue := @Internal_CurrentStatusSetValue;

    if vrJupiterApp.Params.Exists(vrJupiterApp.AppID + '.State.Message') then
      vrJupiterApp.Params.VariableById(vrJupiterApp.AppID + '.State.Message').OnChangeValue := @Internal_CurrentMessageStatusSetValue;

    if vrJupiterApp.Params.Exists('Interface.CurrentForm.Title') then
      vrJupiterApp.Params.VariableById('Interface.CurrentForm.Title').OnChangeValue := @Internal_CurrentFormTitleSetValue;

    Self.Internal_ListMenuItens;
    Self.Internal_GetExternalIcons;
    Self.Internal_PrepareMainMenu;
    Self.Internal_PrepareShortCuts;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFMain.Internal_LoadDirectoryStructure(prDirectory: String; prAfterThan: TMenuItem);
var
  vrEnviroment : TJupiterEnviroment;
  vrDirectory : TJupiterDirectoryDataProvider;
  vrVez : Integer;
  vrMenuItem : TMenuItem;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  vrDirectory  := TJupiterDirectoryDataProvider.Create;
  try
    vrDirectory.Path := vrEnviroment.FullPath(prDirectory);
    vrDirectory.SubFolders := False;
    vrDirectory.ProvideData;

    for vrVez := 0 to vrDirectory.Count - 1 do
    begin
      vrMenuItem            := TMenuItem.Create(mmMainMenu);
      vrMenuItem.Caption    := vrDirectory.GetRowByIndex(vrVez).Fields.VariableById('Folder').Value;
      vrMenuItem.ImageIndex := ICON_OPEN;
      vrMenuItem.OnClick    := @menuFolderClick;

      if prDirectory = EmptyStr then
        Self.Internal_LoadDirectoryStructure(vrDirectory.GetRowByIndex(vrVez).Fields.VariableById('Folder').Value + GetDirectorySeparator, vrMenuItem)
      else
        Self.Internal_LoadDirectoryStructure(prDirectory + GetDirectorySeparator + vrDirectory.GetRowByIndex(vrVez).Fields.VariableById('Folder').Value + GetDirectorySeparator, vrMenuItem);

      vrMenuItem.Hint := vrDirectory.GetRowByIndex(vrVez).Fields.VariableById('Path').Value;

      prAfterThan.Add(vrMenuItem);
    end;
  finally
    FreeAndNil(vrEnviroment);
    FreeAndNil(vrDirectory);
  end;
end;

procedure TFMain.Internal_PrepareMainMenu;
begin
  Self.MenuRouteList.Add(TJupiterMenuRoute.Create(MENU_FILE_PATH, miFormParams));
  Self.MenuRouteList.Add(TJupiterMenuRoute.Create(MENU_FILE_NEW_PATH, miNew));
  Self.MenuRouteList.Add(TJupiterMenuRoute.Create(MENU_FILE_OPEN_PATH, miOpen));
  Self.MenuRouteList.Add(TJupiterMenuRoute.Create(MENU_EDIT_PATH, MenuItem2));
  Self.MenuRouteList.Add(TJupiterMenuRoute.Create(MENU_TOOLS_PATH, MenuItem3));
  Self.MenuRouteList.Add(TJupiterMenuRoute.Create(MENU_FOLDERS_PATH, MenuItem4));
  Self.MenuRouteList.Add(TJupiterMenuRoute.Create(MENU_HELP_PATH, MenuItem5));
  Self.MenuRouteList.OnClick := @MenuRouteListClick;

  Self.MenuRouteList.Render;
end;

procedure TFMain.Internal_PrepareShortCuts;
var
  vrVez      : Integer;
  vrShortCut : TAction;
begin
  for vrVez := 0 to vrJupiterApp.Shortcuts.Count - 1 do
    with vrJupiterApp.Shortcuts.GetShortcutByIndex(vrVez) do
    begin
      vrShortCut := TAction.Create(acShortcuts);
      vrShortCut.ActionList := acShortcuts;
      vrShortCut.Caption    := Description;
      vrShortCut.Hint       := ShortCutStr;
      vrShortCut.ShortCut   := GetShortCut;
      vrShortCut.Tag        := vrVez;
      vrShortCut.OnExecute  := @acCustomShortcut;
    end;
end;

function TFMain.Internal_ListAllShortcuts: String;
begin
  Result := inherited Internal_ListAllShortcuts;

  Result := Result + Internal_ListShortcuts(acShortcuts);
end;

procedure TFMain.Internal_Activate;
begin
  inherited Internal_Activate;

  if not Self.FPrepared then
    Exit;

  if Assigned(vrJupiterApp.CurrentForm) then
    vrJupiterApp.CurrentForm.Activate;
end;

end.

