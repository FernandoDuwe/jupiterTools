unit JupiterRoute;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterConsts, JupiterObject, JupiterVariable,
  Menus, SysUtils;

type

  TJupiterRouteData = class(TJupiterObject)
  private
    FID       : Integer;
    FTitle    : String;
    FRoute    : String;
    FIcon     : Integer;
    FShortcut : String;
    FParams   : String;
    FDestiny  : Integer;
    FZIndex   : Integer;
  published
    property ID       : Integer read FID       write FID;
    property Title    : String  read FTitle    write FTitle;
    property Route    : String  read FRoute    write FRoute;
    property Icon     : Integer read FIcon     write FIcon;
    property Shortcut : String  read FShortcut write FShortcut;
    property Params   : String  read FParams   write FParams;
    property Destiny  : Integer read FDestiny  write FDestiny;
    property ZIndex   : Integer read FZIndex   write FZIndex;
  public

  end;

  { TJupiterRoute }

  TJupiterRoute = class(TJupiterObject)
  private
    FPath   : String;
    FParams : TJupiterVariableList;
  published
    property Path : String read FPath write FPath;

    property Params : TJupiterVariableList read FParams write FParams;
  public
    function DestinyPath : String;

    function SameRoute(prRoutePath : String) : Boolean;

    constructor Create(prPath : String);
    destructor Destroy; override;
  end;

  { TJupiterFormRoute }

  TJupiterFormRoute = class(TJupiterRoute)
  private
    FFormClass : TComponentClass;
  published
    property FormClass : TComponentClass read FFormClass write FFormClass;

  public
    constructor Create(prPath : String; prFormClass : TComponentClass);
  end;

  { TJupiterRouteList }

  TJupiterRouteList = class(TJupiterObjectList)
  public
    function ListDir(prDir : String) : TJupiterRouteList;
  end;

  { TJupiterFormRouteGroup }

  TJupiterFormRouteGroup = class(TJupiterObject)
  private
    FList : TList;

    function Internal_GetFormRouteCount : Integer;
  published
    property FormRouteCount : Integer read Internal_GetFormRouteCount;
  public
    function NavigateTo(prRoute : TJupiterRoute) : Boolean;

    constructor Create;
    destructor Destroy; override;
  end;

  { TJupiterMenuRoute }

  TJupiterMenuRoute = class(TJupiterRoute)
  private
    FMenuItem : TMenuItem;
  published
    property MenuItem : TMenuItem read FMenuItem write FMenuItem;
  public
    constructor Create(prPath : String; prMenuItem : TMenuItem);
  end;


implementation

uses
  JupiterApp, JupiterEnviroment;

{ TJupiterMenuRoute }

constructor TJupiterMenuRoute.Create(prPath: String; prMenuItem: TMenuItem);
begin
  Self.FMenuItem := prMenuItem;

  inherited Create(prPath);
end;

{ TJupiterRouteList }

function TJupiterRouteList.ListDir(prDir: String): TJupiterRouteList;
var
  vrVez : Integer;
begin
  Result := TJupiterRouteList.Create;

  for vrVez := 0 to Self.Size - 1 do
    if TJupiterRoute(Self.GetAtIndex(vrVez)).Path = prDir then
      Result.Add(TJupiterRoute(Self.GetAtIndex(vrVez)));
end;

{ TJupiterFormRouteGroup }

function TJupiterFormRouteGroup.Internal_GetFormRouteCount: Integer;
begin
  Result := Self.FList.Count;
end;

function TJupiterFormRouteGroup.NavigateTo(prRoute: TJupiterRoute): Boolean;
begin
  Result := False;
end;

constructor TJupiterFormRouteGroup.Create;
begin
  Self.FList := TList.Create;
  Self.FList.Clear;
end;

destructor TJupiterFormRouteGroup.Destroy;
begin
  FreeAndNil(Self.FList);

  inherited Destroy;
end;

{ TJupiterFormRoute }

constructor TJupiterFormRoute.Create(prPath: String; prFormClass: TComponentClass);
begin
  Self.FFormClass := prFormClass;

  inherited Create(prPath);
end;

{ TJupiterRoute }

function TJupiterRoute.DestinyPath: String;
begin
  Result := Self.Path;

  if Self.Params.Exists('destinyPath') then
    Result := Self.Params.VariableById('destinyPath').Value;
end;

function TJupiterRoute.SameRoute(prRoutePath: String): Boolean;
begin
  Result := Self.Path = prRoutePath;
end;

constructor TJupiterRoute.Create(prPath: String);
begin
  Self.Path := prPath;

  Self.Params := TJupiterVariableList.Create;
end;

destructor TJupiterRoute.Destroy;
begin
  FreeAndNil(Self.FParams);

  inherited Destroy;
end;

end.

