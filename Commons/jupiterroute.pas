unit JupiterRoute;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterConsts, JupiterObject, JupiterVariable,
  JupiterXMLDataProvider, Menus, SysUtils;

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

  { TJupiterMenuRouteList }

  TJupiterMenuRouteList = class(TJupiterObjectList)
  protected
    FMainMenu : TMainMenu;
    FOnClick  : TNotifyEvent;

    procedure Internal_LoadFromFile;
    procedure Internal_SaveToFile;
  published
    property MainMenu : TMainMenu    read FMainMenu write FMainMenu;
    property OnClick  : TNotifyEvent read FOnClick  write FOnClick;
  public
    function MenuRouteAtIndex(prIndex : Integer) : TJupiterMenuRoute;
    function MenuRouteByPath(prPath : String) : TJupiterMenuRoute;
    function MenuRouteByMenuItem(prMenuItem : TMenuItem) : TJupiterMenuRoute;

    procedure Render;

    constructor Create; override;
  end;

implementation

uses
  JupiterEnviroment;

{ TJupiterMenuRouteList }

procedure TJupiterMenuRouteList.Internal_LoadFromFile;
var
  vrEnviroment : TJupiterEnviroment;
  vrXML : TJupiterXMLDataProvider;
  vrVez : Integer;
  vrRoute : TJupiterMenuRoute;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  vrXML        := TJupiterXMLDataProvider.Create;
  try
    vrXML.SearchNode := 'route';
    vrXML.Filename   := vrEnviroment.FullPath('/modules/generator/data/main_menu_list.xml');
    vrXML.ProvideData;

    for vrVez := 0 to vrXML.Count - 1 do
      with vrXML.GetRowByIndex(vrVez) do
      begin
        vrRoute := TJupiterMenuRoute.Create(Fields.VariableById('path').Value, nil);
        vrRoute.Params.CopyValues(Fields);

        Self.Add(vrRoute);
      end;
  finally
    FreeAndNil(vrEnviroment);
    FreeAndNil(vrXML);
  end;
end;

procedure TJupiterMenuRouteList.Internal_SaveToFile;
var
  vrEnviroment : TJupiterEnviroment;
  vrFile      : TStringList;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  vrFile := TStringList.Create;
  try
    vrFile.Clear;
    vrFile.Add('<?xml version="1.0" encoding="UTF-8"?>');
    vrFile.Add('<content>');
    vrFile.Add('  <routes>');
    vrFile.Add('  </routes>');
    vrFile.Add('</content>');

    vrEnviroment.CreateFile('/modules/generator/data/main_menu_list.xml', vrFile.Text);
  finally
    vrFile.Clear;
    FreeAndNil(vrFile);

    FreeAndNil(vrEnviroment);
  end;
end;

function TJupiterMenuRouteList.MenuRouteAtIndex(prIndex: Integer): TJupiterMenuRoute;
begin
  Result := TJupiterMenuRoute(Self.GetAtIndex(prIndex));
end;

function TJupiterMenuRouteList.MenuRouteByPath(prPath: String): TJupiterMenuRoute;
var
  vrVez : Integer;
begin
  Result := nil;

  for vrVez := 0 to Self.Count - 1 do
    if Self.MenuRouteAtIndex(vrVez).Path = prPath then
      Result := Self.MenuRouteAtIndex(vrVez);
end;

function TJupiterMenuRouteList.MenuRouteByMenuItem(prMenuItem: TMenuItem): TJupiterMenuRoute;
var
  vrVez : Integer;
begin
  Result := nil;

  for vrVez := 0 to Self.Count - 1 do
    if Self.MenuRouteAtIndex(vrVez).MenuItem = prMenuItem then
      Result := Self.MenuRouteAtIndex(vrVez);
end;

procedure TJupiterMenuRouteList.Render;
var
  vrVez      : Integer;
  vrRoute    : TJupiterMenuRoute;
  vrMenuItem : TMenuItem;
begin
  for vrVez := 0 to Self.Count - 1 do
  begin
    if Assigned(Self.MenuRouteAtIndex(vrVez).MenuItem) then
      Continue;

    vrRoute := Self.MenuRouteByPath(Self.MenuRouteAtIndex(vrVez).Params.VariableById('owner').Value);

    if ((not Assigned(vrRoute)) or (not Assigned(vrRoute.MenuItem))) then
    begin
      vrMenuItem := TMenuItem.Create(Self.MainMenu);
      vrMenuItem.Caption := Self.MenuRouteAtIndex(vrVez).Params.VariableById('title').Value;
      vrMenuItem.ImageIndex := StrToIntDef(Self.MenuRouteAtIndex(vrVez).Params.VariableById('icon').Value, -1);
      vrMenuItem.OnClick := Self.OnClick;

      Self.MainMenu.Items.Insert(Self.MainMenu.Items.Count - 1, vrMenuItem);

      Self.MenuRouteAtIndex(vrVez).MenuItem := vrMenuItem;
    end
    else
    begin
      vrMenuItem := TMenuItem.Create(Self.MainMenu);
      vrMenuItem.Caption := Self.MenuRouteAtIndex(vrVez).Params.VariableById('title').Value;
      vrMenuItem.ImageIndex := StrToIntDef(Self.MenuRouteAtIndex(vrVez).Params.VariableById('icon').Value, -1);
      vrMenuItem.OnClick := Self.OnClick;

      vrRoute.MenuItem.Add(vrMenuItem);

      Self.MenuRouteAtIndex(vrVez).MenuItem := vrMenuItem;
    end;
  end;
end;

constructor TJupiterMenuRouteList.Create;
var
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Create;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    if not vrEnviroment.Exists(vrEnviroment.FullPath('/modules/generator/data/')) then
      vrEnviroment.CreatePath('/modules/generator/data/');

    if not vrEnviroment.Exists(vrEnviroment.FullPath('/modules/generator/data/main_menu_list.xml')) then
      Self.Internal_SaveToFile;

    Self.Internal_LoadFromFile;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

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

