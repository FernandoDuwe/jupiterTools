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

  Result.Add(TJupiterAction.Create('Favoritos', TJupiterRoute.Create('/tools/favorites/'), TJupiterRoute.Create('/tools/')));

  with TJupiterAction(Result.GetLastObject) do
    Icon := ICON_FAVORITE;

  Result.Add(TJupiterAction.Create('Arquivos', TJupiterRoute.Create('/tools/favorites/apps/'), TJupiterRoute.Create('/tools/favorites/')));

  with TJupiterAction(Result.GetLastObject) do
    Icon := ICON_APPLICATION;

  Result.Add(TJupiterAction.Create('Diretórios', TJupiterRoute.Create('/tools/favorites/folders/'), TJupiterRoute.Create('/tools/favorites/')));

  with TJupiterAction(Result.GetLastObject) do
    Icon := ICON_OPEN;
end;

end.

