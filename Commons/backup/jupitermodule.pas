unit JupiterModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterObject, JupiterRoute, JupiterVariable, SysUtils;

type

  { TJupiterModule }

  TJupiterModule = class(TJupiterObject)
  private
    FParams : TJupiterVariableList;
  protected
    function Internal_GetModuleID : String; virtual;
    function Internal_GetModuleTitle : String; virtual;
    procedure Internal_Prepare; virtual;
  published
    property ModuleID    : String               read Internal_GetModuleID;
    property ModuleTitle : String               read Internal_GetModuleTitle;
    property Params      : TJupiterVariableList read FParams write FParams;
  public
    function GetActions(prRoute : TJupiterRoute) : TJupiterObjectList; virtual;
    function DefineParamName(prName : String) : String;

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

{ TJupiterModuleList }

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
begin
  Self.Params.FileName := 'datasets/' + StringReplace(Self.ModuleID, '.', EmptyStr, [rfIgnoreCase, rfReplaceAll]) + '.csv';
end;

function TJupiterModule.GetActions(prRoute: TJupiterRoute): TJupiterObjectList;
begin
  Result := TJupiterRouteList.Create;
end;

function TJupiterModule.DefineParamName(prName: String): String;
begin
  Result := Self.ModuleID + '.' + prName;
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

