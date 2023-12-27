unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Menus, PopupNotifier, Buttons, JupiterApp, JupiterRoute,
  JupiterConsts, JupiterObject, JupiterForm, JupiterAction, JupiterEnviroment,
  JupiterRunnable, jupiterformutils, JupiterFileDataProvider, jupiterScript,
  JupiterDirectoryDataProvider, JupiterToolsModule, uPSComponent_Default,
  LMessages, PairSplitter, ActnList;

type

  { TFMain }

  TFMain = class(TFJupiterForm)
    acPrompt: TAction;
    acShortcuts: TActionList;
    acMenu: TAction;
    acChangeSearchAction: TAction;
    ApplicationProperties1: TApplicationProperties;
    cbNavigationMenu: TCoolBar;
    edSearch: TEdit;
    ilIconFamily: TImageList;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
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
    pnBody: TPanel;
    pnMenu: TPanel;
    spSplitter: TSplitter;
    tbHome: TToolButton;
    tbMenu: TToolButton;
    tbOptions: TToolBar;
    TbSystemBar: TToolButton;
    tmInternalThread: TTimer;
    tmSearch: TTimer;
    tbSearch: TToolBar;
    tbSystemButtons: TToolBar;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    tbMessage: TToolButton;
    ToolButton2: TToolButton;
    tbPrompt: TToolButton;
    tvMenu: TTreeView;
    procedure acChangeSearchActionExecute(Sender: TObject);
    procedure acMenuExecute(Sender: TObject);
    procedure acPromptExecute(Sender: TObject);
    procedure ApplicationProperties1Activate(Sender: TObject);
    procedure ApplicationProperties1Restore(Sender: TObject);
    procedure cbNavigationMenuChange(Sender: TObject);
    procedure edSearchChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
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
    procedure miUpdateClick(Sender: TObject);
    procedure pnBodyClick(Sender: TObject);
    procedure tbHomeClick(Sender: TObject);
    procedure tbMenuClick(Sender: TObject);
    procedure tbMessageClick(Sender: TObject);
    procedure tbPromptClick(Sender: TObject);
    procedure tmInternalThreadTimer(Sender: TObject);
    procedure tmSearchTimer(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure tvMenuChange(Sender: TObject; Node: TTreeNode);
    procedure tvMenuClick(Sender: TObject);
    procedure menuFolderClick(Sender: TObject);
  private
    FSearchMode : TJupiterSearchMode;

    procedure Internal_ListMenuItens;
    procedure Internal_GetExternalIcons;

    procedure Internal_ShowRoute(prRoute : TJupiterRoute; prList : TJupiterObjectList; prNode : TTreeNode);
    procedure Internal_MessagesCountSetValue(prID, prNewValue : String);
    procedure Internal_CurrentFormTitleSetValue(prID, prNewValue : String);
    function Internal_GoToPageItem(prItemRouteForm : String) : Boolean;

    procedure Internal_OnBeforeNavigate(Sender: TObject);
    procedure Internal_OnAfterNavigate(Sender: TObject);
    procedure Internal_CallFormAction(prActionIndex : Integer);
  protected
    procedure Internal_UpdateComponents; override;
    procedure Internal_PrepareForm; override;
    procedure Internal_LoadDirectoryStructure(prDirectory : String; prAfterThan : TMenuItem);
  public

  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}

uses LCLType, JupiterDialogForm, StrUtils, JupiterStandardModule;

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
  if miAutoUpdate.Checked then
  begin
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

procedure TFMain.edSearchChange(Sender: TObject);
begin
  tmSearch.Enabled := False;
  tmSearch.Enabled := True;
end;

procedure TFMain.FormActivate(Sender: TObject);
begin
  inherited;

  FormResize(Sender);

  ApplicationProperties1Restore(Sender);
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FSearchMode := jsmForm;

  Self.Internal_LoadDirectoryStructure(EmptyStr, MenuItem4);
end;

procedure TFMain.FormResize(Sender: TObject);
begin
  cbNavigationMenu.Bands[1].Width := (Self.Width - ( (FORM_MARGIN_LEFT * 4) +
                                                    cbNavigationMenu.Bands[0].Width +
                                                    cbNavigationMenu.Bands[2].Width));

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

  if TJupiterStandardModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Standard')).HideMenuTree then
    tbMenu.Click;

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

procedure TFMain.MenuItem1Click(Sender: TObject);
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

      Route.Params.AddVariable('title', 'Arquivo: ' + ExtractFileName(vrFile), 'Título');
      Route.Params.AddVariable('filename', vrFile, 'Arquivo');

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

procedure TFMain.miUpdateClick(Sender: TObject);
begin
  Self.IsModal := True;

  Self.UpdateForm;
end;

procedure TFMain.pnBodyClick(Sender: TObject);
begin

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
  sbStatus.Panels[0].Text := 'Mensagens: ' + prNewValue;

  tbMessage.Hint := sbStatus.Panels[0].Text;
  miMessage.Hint := sbStatus.Panels[0].Text;
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

      tvMenuChange(Self, tvMenu.Items[vrVez]);
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

procedure TFMain.Internal_CallFormAction(prActionIndex: Integer);
begin
  //
end;

procedure TFMain.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

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
end;

procedure TFMain.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  vrJupiterApp.BodyPanel        := pnBody;
  vrJupiterApp.PopupNotifier    := ppNotifier;
  vrJupiterApp.OnBeforeNavigate := @Internal_OnBeforeNavigate;
  vrJupiterApp.OnAfterNavigate  := @Internal_OnAfterNavigate;

  if vrJupiterApp.Params.Exists(vrJupiterApp.AppID + '.Messages.Count') then
  begin
    vrJupiterApp.Params.VariableById(vrJupiterApp.AppID + '.Messages.Count').OnChangeValue := @Internal_MessagesCountSetValue;

    Internal_MessagesCountSetValue(EmptyStr, IntToStr(vrJupiterApp.Messages.Size));
  end;

  if vrJupiterApp.Params.Exists('Interface.CurrentForm.Title') then
    vrJupiterApp.Params.VariableById('Interface.CurrentForm.Title').OnChangeValue := @Internal_CurrentFormTitleSetValue;

  Self.Internal_ListMenuItens;
  Self.Internal_GetExternalIcons;
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

end.

