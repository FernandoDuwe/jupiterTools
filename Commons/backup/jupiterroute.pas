unit JupiterRoute;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterConsts, JupiterObject, JupiterVariable, SysUtils;

type

  { TJupiterRoute }

  TJupiterRoute = class(TJupiterObject)
  private
    FPath   : String;
    FParams : TJupiterVariableList;
  published
    property Path : String read FPath write FPath;

    property Params : TJupiterVariableList read FParams write FParams;
  public
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

  { TJupiterFormRouteGroup }

  { TJupiterRouteList }

  TJupiterRouteList = class(TJupiterObjectList)
  public
    function ListDir(prDir : String) : TJupiterRouteList;
  end;

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

implementation

{ TJupiterRouteList }

function TJupiterRouteList.ListDir(prDir: String): TJupiterRouteList;
var
  vrVez : Integer;
begin
  Result := TJupiterRouteList.Create;

  for vrVez := 0 to Self.Size - 1 do

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

