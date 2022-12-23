unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Menus, PopupNotifier, Buttons, JupiterApp, JupiterRoute,
  JupiterConsts, JupiterObject, JupiterForm, JupiterAction, JupiterEnviroment,
  JupiterRunnable, jupiterformutils, JupiterToolsModule, uPSComponent_Default,
  LMessages, PairSplitter;

type

  { TFMain }

  TFMain = class(TFJupiterForm)
    cbNavigationMenu: TCoolBar;
    edSearch: TEdit;
    ilIconFamily: TImageList;
    miMaximizedForms: TMenuItem;
    miPastasJupiter: TMenuItem;
    miPastasModules: TMenuItem;
    miPastasDatasets: TMenuItem;
    miPastasTemp: TMenuItem;
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
    tmSearch: TTimer;
    ToolBar1: TToolBar;
    tbSearch: TToolBar;
    tbSystemButtons: TToolBar;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    tbMessage: TToolButton;
    ToolButton2: TToolButton;
    tvMenu: TTreeView;
    procedure cbNavigationMenuChange(Sender: TObject);
    procedure edSearchChange(Sender: TObject);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure miDecFontSizeClick(Sender: TObject);
    procedure miIncFontSizeClick(Sender: TObject);
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
    procedure miOpenCSVClick(Sender: TObject);
    procedure miOpenCurrentTaskClick(Sender: TObject);
    procedure miUpdateClick(Sender: TObject);
    procedure pnBodyClick(Sender: TObject);
    procedure tbHomeClick(Sender: TObject);
    procedure tbMenuClick(Sender: TObject);
    procedure tbMessageClick(Sender: TObject);
    procedure tmSearchTimer(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure tvMenuClick(Sender: TObject);
  private
    procedure Internal_ListMenuItens;

    procedure Internal_ShowRoute(prRoute : TJupiterRoute; prList : TJupiterObjectList; prNode : TTreeNode);
    procedure Internal_MessagesCountSetValue(prID, prNewValue : String);
    procedure Internal_CurrentFormTitleSetValue(prID, prNewValue : String);
  protected
    procedure Internal_UpdateComponents; override;
    procedure Internal_PrepareForm; override;
  public

  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}

uses LCLType;

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

procedure TFMain.tmSearchTimer(Sender: TObject);
begin
  if Assigned(vrJupiterApp.CurrentForm) then
    vrJupiterApp.CurrentForm.Search(edSearch.Text);

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

procedure TFMain.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
begin

end;

procedure TFMain.cbNavigationMenuChange(Sender: TObject);
begin

end;

procedure TFMain.edSearchChange(Sender: TObject);
begin
  tmSearch.Enabled := False;
  tmSearch.Enabled := True;
end;

procedure TFMain.FormShow(Sender: TObject);
begin
  inherited;

  vrJupiterApp.MainIcons := ilIconFamily;
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(ROOT_FORM_PATH), False);
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

procedure TFMain.miOpenCSVClick(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
  vrFile       : String;
  vrRoute      : TJupiterRoute;
begin
  vrEnviroment := TJupiterEnviroment.Create();
  try
    vrFile := vrEnviroment.OpenFile('*.csv');

    if vrFile <> EmptyStr then
    begin
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

procedure TFMain.Internal_ListMenuItens;
var
  vrMenuList : TJupiterObjectList;
begin
  vrMenuList := vrJupiterApp.GetActions(TJupiterRoute.Create(ROOT_PATH));

  ShowRouteOnTreeView(tvMenu, TJupiterRoute.Create(ROOT_PATH), vrMenuList, nil);

  tvMenu.FullExpand;
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

procedure TFMain.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if Assigned(vrJupiterApp.CurrentForm) then
    vrJupiterApp.CurrentForm.UpdateForm;

  if pnMenu.Visible then
  begin
    tbMenu.ImageIndex := ICON_LEFT;

    tbMenu.Hint := 'Esconder menu';
  end
  else
  begin
    tbMenu.ImageIndex := ICON_RIGHT;

    tbMenu.Hint := 'Exibir menu';
  end;

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
end;

procedure TFMain.Internal_PrepareForm;
var
  vr : TDateTime;
begin
  inherited Internal_PrepareForm;

  vrJupiterApp.BodyPanel := pnBody;

  if vrJupiterApp.Params.Exists(vrJupiterApp.AppID + '.Messages.Count') then
  begin
    vrJupiterApp.Params.VariableById(vrJupiterApp.AppID + '.Messages.Count').OnChangeValue := @Internal_MessagesCountSetValue;

    Internal_MessagesCountSetValue(EmptyStr, IntToStr(vrJupiterApp.Messages.Size));
  end;

  if vrJupiterApp.Params.Exists('Interface.CurrentForm.Title') then
    vrJupiterApp.Params.VariableById('Interface.CurrentForm.Title').OnChangeValue := @Internal_CurrentFormTitleSetValue;

  Self.Internal_ListMenuItens;
end;

end.

