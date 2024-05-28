unit JupiterStandardModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterModule, JupiterRoute, JupiterObject, JupiterAction,
  JupiterConsts, JupiterEnviroment, SysUtils;

type
  { TJupiterStandardModule }

  TJupiterStandardModule = class(TJupiterModule)
  protected
    procedure Internal_GenerateDefaultScript;
    procedure Internal_Prepare; override;
    function Internal_GetModuleID : String; override;
    function Internal_GetModuleTitle : String; override;
  public
    function CompactMode : Boolean;
    function ShowActionsInForm : Boolean;
    function HideMenuTree : Boolean;
    procedure SetUserPreferences(prPreferencesTag : String);
    procedure SetUserStartRoute(prRoute : String);
    function GetUserStartRoute : String;
    function TabMode : Boolean;

    function GetActions(prRoute : TJupiterRoute) : TJupiterObjectList; override;
  end;

implementation

{ TJupiterStandardModule }

procedure TJupiterStandardModule.Internal_GenerateDefaultScript;
var
  vrEnviroment : TJupiterEnviroment;
  vrStr : TStrings;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  vrStr := TStringList.Create;
  try
    vrStr.Clear;

    vrEnviroment.CreatePath('modules/jpas/');

    vrStr.Add('program promptCommand;');
    vrStr.Add('begin');
    vrStr.Add('  ' + JPAS_FLAG_USERCOMMAND);
    vrStr.Add('end.');

    vrEnviroment.CreateFile('modules/jpas/promptCommand.jpas', vrStr.Text);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);

    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterStandardModule.Internal_Prepare;
begin
  inherited Internal_Prepare;

    if not Self.Params.Exists(Self.DefineParamName('Triggers.OnBeforeStart')) then
      Self.Params.AddConfig(Self.DefineParamName('Triggers.OnBeforeStart'), EmptyStr, 'Gatilhos: Antes de iniciar a aplicação');

  if not Self.Params.Exists(Self.DefineParamName('Triggers.OnStart')) then
    Self.Params.AddConfig(Self.DefineParamName('Triggers.OnStart'), EmptyStr, 'Gatilhos: Ao iniciar a aplicação');

  if not Self.Params.Exists(Self.DefineParamName('Triggers.OnUpdate')) then
    Self.Params.AddConfig(Self.DefineParamName('Triggers.OnUpdate'), EmptyStr, 'Gatilhos: Ao atualizar a aplicação');

  if not Self.Params.Exists(Self.DefineParamName('Triggers.OnChangeRoute')) then
    Self.Params.AddConfig(Self.DefineParamName('Triggers.OnChangeRoute'), EmptyStr, 'Gatilhos: Ao alterar a rota');

  if not Self.Params.Exists(Self.DefineParamName('Triggers.OnChangeTask')) then
    Self.Params.AddConfig(Self.DefineParamName('Triggers.OnChangeTask'), EmptyStr, 'Gatilhos: Ao alterar a tarefa atual');

  if not Self.Params.Exists(Self.DefineParamName('Triggers.OnExecuteCurrentThread')) then
    Self.Params.AddConfig(Self.DefineParamName('Triggers.OnExecuteCurrentThread'), EmptyStr, 'Gatilhos: Thread executada a cada X tempo na aplicação');

  if not Self.Params.Exists(Self.DefineParamName('Triggers.OnExecuteCurrentThread.Time')) then
    Self.Params.AddConfig(Self.DefineParamName('Triggers.OnExecuteCurrentThread.Time'), '5000', 'Gatilhos: Intervalo de execução da Thread executada a cada X tempo na aplicação');

  if not Self.Params.Exists(Self.DefineParamName('UI.UserPreferences')) then
    Self.Params.AddConfig(Self.DefineParamName('UI.UserPreferences'), EmptyStr, 'Preferências do usuário');

  if not Self.Params.Exists(Self.DefineParamName('UI.UserPreferences.StartRoute')) then
    Self.Params.AddConfig(Self.DefineParamName('UI.UserPreferences.StartRoute'), EmptyStr, 'Rota inicial');

  if not Self.Params.Exists(Self.DefineParamName('UI.ShowPopup')) then
    Self.Params.AddConfig(Self.DefineParamName('UI.ShowPopup'), '1', 'Exibir Popup (1 - Sim, 2 - Não)');

  if not Self.Params.Exists(Self.DefineParamName('UI.DebugMode')) then
    Self.Params.AddConfig(Self.DefineParamName('UI.DebugMode'), '0', 'Modo Debug (1 - Sim, 2 - Não)');

  Self.Internal_GenerateDefaultScript;
end;

function TJupiterStandardModule.Internal_GetModuleID: String;
begin
  Result := 'Jupiter.Standard';
end;

function TJupiterStandardModule.Internal_GetModuleTitle: String;
begin
  Result := 'Básico';
end;

function TJupiterStandardModule.CompactMode: Boolean;
begin
  Result := Pos('#COMPACTMODE', AnsiUpperCase(Self.Params.VariableById(Self.DefineParamName('UI.UserPreferences')).Value)) <> 0;
end;

function TJupiterStandardModule.ShowActionsInForm: Boolean;
begin
  Result := Pos('#SHOWACTIONSINFORM', AnsiUpperCase(Self.Params.VariableById(Self.DefineParamName('UI.UserPreferences')).Value)) <> 0;
end;

function TJupiterStandardModule.HideMenuTree: Boolean;
begin
  Result := Pos('#HIDEMENUTREE', AnsiUpperCase(Self.Params.VariableById(Self.DefineParamName('UI.UserPreferences')).Value)) <> 0;
end;

procedure TJupiterStandardModule.SetUserPreferences(prPreferencesTag: String);
begin
  Self.Params.VariableById(Self.DefineParamName('UI.UserPreferences')).Value := prPreferencesTag;
end;

procedure TJupiterStandardModule.SetUserStartRoute(prRoute: String);
begin
  Self.Params.VariableById(Self.DefineParamName('UI.UserPreferences.StartRoute')).Value := prRoute;
end;

function TJupiterStandardModule.GetUserStartRoute: String;
begin
  Result := Self.Params.VariableById(Self.DefineParamName('UI.UserPreferences.StartRoute')).Value;
end;

function TJupiterStandardModule.TabMode: Boolean;
begin
  Result := Pos('#TABNAVIGATION', AnsiUpperCase(Self.Params.VariableById(Self.DefineParamName('UI.UserPreferences')).Value)) <> 0;
end;

function TJupiterStandardModule.GetActions(prRoute: TJupiterRoute): TJupiterObjectList;
begin
  Result := inherited GetActions(prRoute);

  Result.Add(TJupiterAction.Create('Cadastros', TJupiterRoute.Create('/records/'), TJupiterRoute.Create('/')));

  with TJupiterAction(Result.GetLastObject) do
    Icon := ICON_RECORDS;

  Result.Add(TJupiterAction.Create('Ferramentas', TJupiterRoute.Create('/tools/'), TJupiterRoute.Create('/')));

  with TJupiterAction(Result.GetLastObject) do
    Icon := ICON_TOOLS;
end;

end.

