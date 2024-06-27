unit JupiterModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterObject, JupiterVariable, jupiterDatabaseWizard, JupiterConsts,
  SysUtils;

type

  { TJupiterModule }

  TJupiterModule = class(TJupiterObject)
  private
    FParams : TJupiterVariableList;
  protected
    function Internal_GetModuleID : String; virtual;
    function Internal_GetModuleTitle : String; virtual;
    procedure Internal_Prepare; virtual;

    procedure Internal_CreateRouteIfDontExists(prTitle, prRoute, prDestiny : String; prIcon, prZIndex : Integer);
  published
    property ModuleID    : String               read Internal_GetModuleID;
    property ModuleTitle : String               read Internal_GetModuleTitle;
    property Params      : TJupiterVariableList read FParams write FParams;
  public
    function DefineParamName(prName : String) : String;
    procedure ExecuteCommand(prParamList : TStrings); virtual;

    constructor Create;
    destructor Destroy; override;
  end;


  { TJupiterModuleList }

  TJupiterModuleList = class(TJupiterObjectList)
  public
    function GetModuleByIndex(prIndex : Integer) : TJupiterModule;
    function GetModuleById(prID : String) : TJupiterModule;
  end;

implementation

uses JupiterApp;

{ TJupiterModuleList }

function TJupiterModuleList.GetModuleByIndex(prIndex: Integer): TJupiterModule;
begin
  Result := TJupiterModule(Self.GetAtIndex(prIndex));
end;

function TJupiterModuleList.GetModuleById(prID: String): TJupiterModule;
var
  vrVez : Integer;
begin
  Result := nil;

  for vrVez := 0 to Self.Size - 1 do
    if TJupiterModule(Self.GetAtIndex(vrVez)).ModuleID = prID then
      Result := TJupiterModule(Self.GetAtIndex(vrVez));
end;

{ TJupiterModule }

function TJupiterModule.Internal_GetModuleID: String;
begin
  Result := EmptyStr;
end;

function TJupiterModule.Internal_GetModuleTitle: String;
begin
  Result := EmptyStr;
end;

procedure TJupiterModule.Internal_Prepare;
var
  vrWizard : TJupiterDatabaseWizard;
begin
  Self.Params.FileName := 'datasets/' + StringReplace(Self.ModuleID, '.', EmptyStr, [rfIgnoreCase, rfReplaceAll]) + '.csv';

  vrWizard := vrJupiterApp.NewWizard;
  try
    if not vrWizard.Exists('MODULES', ' MODULEID = "' + Self.ModuleID + '" ') then
      vrWizard.ExecuteScript(CreateStringList(' INSERT INTO MODULES (NAME, MODULEID) VALUES ("' + Self.ModuleTitle + '", "' + Self.ModuleID + '") '));
  finally
    FreeAndNil(vrWizard);
  end;
end;

procedure TJupiterModule.Internal_CreateRouteIfDontExists(prTitle, prRoute, prDestiny: String; prIcon, prZIndex : Integer);
var
  vrWizard : TJupiterDatabaseWizard;
  vrIcon   : String;
  vrZIndex : String;
begin
  vrWizard := vrJupiterApp.NewWizard;
  try
    if vrWizard.Exists('ROUTES', ' ROUTE = "' + prRoute + '" ') then
      Exit;

    if prIcon = NULL_KEY then
      vrIcon := 'NULL'
    else
      vrIcon := IntToStr(prIcon);

    if prZIndex = NULL_KEY then
      vrZIndex := 'NULL'
    else
      vrZIndex := IntToStr(prZIndex);

    if prDestiny = EmptyStr then
      vrWizard.ExecuteScript(CreateStringList(' INSERT INTO ROUTES (TITLE, ROUTE, ICON, ZINDEX) VALUES ("' + prTitle + '", "' + prRoute + '", ' + vrIcon + ', ' + vrZIndex + ') '))
    else
      vrWizard.ExecuteScript(CreateStringList(' INSERT INTO ROUTES (TITLE, ROUTE, ICON, ZINDEX, DESTINY) VALUES ("' + prTitle + '", "' + prRoute + '", ' + vrIcon + ', ' + vrZIndex + ', "' + prDestiny + '") '))
  finally
    FreeAndNil(vrWizard);
  end;
end;

function TJupiterModule.DefineParamName(prName: String): String;
begin
  Result := Self.ModuleID + '.' + prName;
end;

procedure TJupiterModule.ExecuteCommand(prParamList: TStrings);
begin
  //
end;

constructor TJupiterModule.Create;
begin
  inherited Create;

  Self.FParams := TJupiterVariableList.Create;

  Self.Internal_Prepare;
end;

destructor TJupiterModule.Destroy;
begin
  FreeAndNil(Self.FParams);

  inherited Destroy;
end;

end.

