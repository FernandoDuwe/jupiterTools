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
    function Internal_GetModuleID : String; override;
    function Internal_GetModuleTitle : String; override;
  public
    function GetActions(prRoute : TJupiterRoute) : TJupiterObjectList; override;
  end;

implementation

{ TJupiterStandardModule }

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

