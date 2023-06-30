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

  if not Self.Params.Exists(Self.DefineParamName('Triggers.OnStart')) then
    Self.Params.AddConfig(Self.DefineParamName('Triggers.OnStart'), EmptyStr, 'Gatilhos: Ao iniciar a aplicação');

  if not Self.Params.Exists(Self.DefineParamName('Triggers.OnUpdate')) then
    Self.Params.AddConfig(Self.DefineParamName('Triggers.OnUpdate'), EmptyStr, 'Gatilhos: Ao atualizar a aplicação');

  if not Self.Params.Exists(Self.DefineParamName('Triggers.OnChangeRoute')) then
    Self.Params.AddConfig(Self.DefineParamName('Triggers.OnChangeRoute'), EmptyStr, 'Gatilhos: Ao alterar a rota');

  if not Self.Params.Exists(Self.DefineParamName('Triggers.OnChangeTask')) then
    Self.Params.AddConfig(Self.DefineParamName('Triggers.OnChangeTask'), EmptyStr, 'Gatilhos: Ao alterar a tarefa atual');

  if not Self.Params.Exists(Self.DefineParamName('Triggers.OnExecuteCurrentThread')) then
    Self.Params.AddConfig(Self.DefineParamName('Triggers.OnExecuteCurrentThread'), EmptyStr, 'Gatilhos: Thread executada a cada 5 segundos na aplicação');

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

