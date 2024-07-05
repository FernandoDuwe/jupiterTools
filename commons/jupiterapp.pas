unit JupiterApp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, JupiterObject, JupiterModule, JupiterEnviroment,
  JupiterVariable, jupiterDatabaseWizard, jupiterScript, SQLite3Conn;

type

  { TJupiterApp }

  TJupiterApp = class(TJupiterObject)
  private
    FAppID            : String;
    FAppName          : String;
    FModules          : TJupiterModuleList;
    FParams           : TJupiterVariableList;
    FInternalDatabase : TSQLite3Connection;

  protected
    procedure Internal_Prepare; virtual;
    procedure Internal_AddScriptLibraries(var prScript : TJupiterScript); virtual;
  published
    property AppID   : String read FAppID;
    property AppName : String read FAppName;

    property ModulesList : TJupiterModuleList   read FModules write FModules;
    property Params      : TJupiterVariableList read FParams  write FParams;

    property InternalDatabase : TSQLite3Connection read FInternalDatabase write FInternalDatabase;
  public
    procedure AddModule(prModule : TJupiterModule);
    procedure Prepare;

    function GetVersion : String;
    function ConsoleMode : Boolean;

    function NewWizard : TJupiterDatabaseWizard;
    function NewScript : TJupiterScript;
    procedure SetInternalWizardData(prWizard : TJupiterDatabaseWizard);

    procedure RunMacro(prId : Integer);

    constructor Create(prAppID, prAppName : String); virtual;
    destructor Destroy; override;
  end;

var
  vrJupiterApp : TJupiterApp;

implementation

uses FileInfo, SysUtils, SQLDB, uJupiterAppScript;

{ TJupiterApp }

procedure TJupiterApp.Internal_AddScriptLibraries(var prScript : TJupiterScript);
begin
  prScript.LibraryList.Add(TJupiterAppScript.Create);
end;

procedure TJupiterApp.Internal_Prepare;
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.CreatePath('/datasets/');

    Self.FParams.FileName := vrEnviroment.FullPath('/datasets/config.csv');

    if not Self.FParams.Exists('database.local') then
      Self.FParams.AddConfig('database.local', '/datasets/' + Self.AppID + '.db', 'Base de dados local');
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterApp.AddModule(prModule: TJupiterModule);
begin
  Self.ModulesList.Add(prModule);
end;

procedure TJupiterApp.Prepare;
var
  vrVez : Integer;
begin
  for vrVez := 0 to Self.ModulesList.Count - 1 do
    with Self.ModulesList.GetModuleByIndex(vrVez) do
         Prepare;
end;

function TJupiterApp.GetVersion: String;
var
  vrVersionInfo : TVersionInfo;
begin
  Result := EmptyStr;

  vrVersionInfo := TVersionInfo.Create;
  try
    vrVersionInfo.Load(HINSTANCE);

    Result := Format('%0:d.%1:d.%2:d.%3:d', [vrVersionInfo.FixedInfo.FileVersion[0], vrVersionInfo.FixedInfo.FileVersion[1], vrVersionInfo.FixedInfo.FileVersion[2], vrVersionInfo.FixedInfo.FileVersion[3]]);
  finally
    if Assigned(vrVersionInfo) then
      vrVersionInfo.Free;
  end;
end;

function TJupiterApp.ConsoleMode: Boolean;
begin
  {$IFNDEF JUPITERCLI}
  Result := False;
  {$ENDIF}

  {$IFDEF JUPITERCLI}
  Result := True;
  {$ENDIF}
end;

function TJupiterApp.NewWizard: TJupiterDatabaseWizard;
begin
  Result := TJupiterDatabaseWizard.Create(Self.InternalDatabase);
end;

function TJupiterApp.NewScript: TJupiterScript;
begin
  Result := TJupiterScript.Create;

  Self.Internal_AddScriptLibraries(Result);
end;

procedure TJupiterApp.SetInternalWizardData(prWizard: TJupiterDatabaseWizard);
begin
  prWizard.Connection  := Self.InternalDatabase;
  prWizard.Transaction := Self.InternalDatabase.Transaction;
end;

procedure TJupiterApp.RunMacro(prId: Integer);
var
  vrScript : TJupiterScript;
  vrQry    : TSQLQuery;
begin
  vrScript := Self.NewScript;
  vrQry    := Self.NewWizard.NewQuery;
  try
    vrQry.SQL.Add(' SELECT ID, MACRO FROM MACROS WHERE ID = :PRID ');
    vrQry.ParamByName('PRID').AsInteger := prId;
    vrQry.Open;

    vrScript.Script.Add(vrQry.FieldByName('MACRO').AsString);

    vrScript.Execute;
  finally
    FreeAndNil(vrScript);
  end;
end;

constructor TJupiterApp.Create(prAppID, prAppName: String);
begin
  Self.FAppID   := prAppID;
  Self.FAppName := prAppName;

  Self.FParams  := TJupiterVariableList.Create;
  Self.FModules := TJupiterModuleList.Create;

  Self.Internal_Prepare;
end;

destructor TJupiterApp.Destroy;
begin
  FreeAndNil(Self.FParams);
  FreeAndNil(Self.FModules);

  inherited Destroy;
end;

end.

