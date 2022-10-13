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
    function Internal_GetModuleID : String; override;
    function Internal_GetModuleTitle : String; override;
  public
    function GetActions(prRoute : TJupiterRoute) : TJupiterObjectList; override;
  end;

implementation

{ TJupiterGeneratorModule }

function TJupiterGeneratorModule.Internal_GetModuleID: String;
begin
  Result := 'Jupiter.Generator';
end;

function TJupiterGeneratorModule.GetActions(prRoute: TJupiterRoute): TJupiterObjectList;
begin
  Result := inherited GetActions(prRoute);

  {
  Result.Add(TJupiterAction.Create('Generator', TJupiterRoute.Create('/tools/generator/'), TJupiterRoute.Create('/tools/')));

  Result.Add(TJupiterAction.Create('Variáveis', TJupiterRoute.Create('/tools/generator/variables/'), TJupiterRoute.Create('/tools/generator/')));

  Result.Add(TJupiterAction.Create('Menus', TJupiterRoute.Create('/tools/generator/menus/'), TJupiterRoute.Create('/tools/generator/')));

  Result.Add(TJupiterAction.Create('Rotinas', TJupiterRoute.Create('/tools/generator/functions/'), TJupiterRoute.Create('/tools/generator/')));
  }
end;

end.

