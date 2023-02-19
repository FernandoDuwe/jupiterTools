unit JupiterStandardModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterModule, JupiterRoute, JupiterObject, JupiterAction,
  JupiterConsts, SysUtils;

type
  { TJupiterStandardModule }

  TJupiterStandardModule = class(TJupiterModule)
  protected
    procedure Internal_Prepare; override;
    function Internal_GetModuleID : String; override;
    function Internal_GetModuleTitle : String; override;
  public
    function GetActions(prRoute : TJupiterRoute) : TJupiterObjectList; override;
  end;

implementation

{ TJupiterStandardModule }

procedure TJupiterStandardModule.Internal_Prepare;
begin
  inherited Internal_Prepare;

  if not Self.Params.Exists(Self.DefineParamName('Triggers.OnStart')) then
    Self.Params.AddConfig(Self.DefineParamName('Triggers.OnStart'), EmptyStr, 'Gatilhos: Ao iniciar a aplicação');

  if not Self.Params.Exists(Self.DefineParamName('Triggers.OnUpdate')) then
    Self.Params.AddConfig(Self.DefineParamName('Triggers.OnUpdate'), EmptyStr, 'Gatilhos: Ao atualizar a aplicação');
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

