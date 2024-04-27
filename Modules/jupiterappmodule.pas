unit jupiterappmodule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterModule, JupiterConsts, JupiterObject,
  JupiterEnviroment;

type

  { TJupiterAppModule }

  TJupiterAppModule = class(TJupiterModule)
  protected
    procedure Internal_Prepare; override;
    function Internal_GetModuleID : String; override;
    function Internal_GetModuleTitle : String; override;
  public
    function UpdateAvaliable : Boolean;
  end;

implementation

{ TJupiterAppModule }

procedure TJupiterAppModule.Internal_Prepare;
var
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Internal_Prepare;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.CreatePath('modules/app');

    if not Self.Params.Exists(Self.DefineParamName('AutoUpdater.KeyFile.xml')) then
       Self.Params.AddConfig(Self.DefineParamName('AutoUpdater.KeyFile.xml'), '', 'Arquivo base do atualizador automático (.xml)');
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

function TJupiterAppModule.Internal_GetModuleID: String;
begin
  Result := 'Jupiter.Application';
end;

function TJupiterAppModule.Internal_GetModuleTitle: String;
begin
  Result := 'Aplicação';
end;

function TJupiterAppModule.UpdateAvaliable: Boolean;
begin
  Result := Self.Params.VariableById(Self.DefineParamName('AutoUpdater.KeyFile.xml')).Value <> EmptyStr;
end;

end.

