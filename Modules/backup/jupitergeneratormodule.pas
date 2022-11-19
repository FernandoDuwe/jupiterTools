unit JupiterGeneratorModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterModule, JupiterRoute, JupiterObject, JupiterAction,
  JupiterConsts, SysUtils;

type
  { TJupiterGeneratorModule }

  TJupiterGeneratorModule = class(TJupiterModule)
  protected
    procedure Internal_Prepare; override;
    function Internal_GetModuleID : String; override;
    function Internal_GetModuleTitle : String; override;
  public
    function GetActions(prRoute : TJupiterRoute) : TJupiterObjectList; override;
  end;

implementation

{ TJupiterGeneratorModule }

procedure TJupiterGeneratorModule.Internal_Prepare;
var
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Internal_Prepare;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.CreatePath('modules/generator');
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

function TJupiterGeneratorModule.Internal_GetModuleID: String;
begin
  Result := 'Jupiter.Generator';
end;

function TJupiterGeneratorModule.Internal_GetModuleTitle: String;
begin
  Result := 'Generator';
end;

function TJupiterGeneratorModule.GetActions(prRoute: TJupiterRoute): TJupiterObjectList;
begin
  Result := inherited GetActions(prRoute);
end;

end.

