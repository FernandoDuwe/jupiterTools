unit JupiterApp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, JupiterObject, JupiterModule, JupiterEnviroment,
  JupiterVariable, jupiterDatabaseWizard, jupiterScript, jupiterStringUtils,
  JupiterConsts, uJupiterEnviromentScript, uJupiterStringUtilsScript,
  uJupiterRunnableScript, SQLite3Conn;

type

  { TJupiterApp }

  TJupiterApp = class(TJupiterObject)
  private
    FAppID            : String;
    FAppName          : String;
    FModules          : TJupiterModuleList;
    FParams           : TJupiterVariableList;
    FScripts          : TJupiterVariableList;
    FInternalDatabase : TSQLite3Connection;

  protected
    procedure Internal_Prepare; virtual;
    procedure Internal_AddScriptLibraries(var prScript : TJupiterScript); virtual;
  published
    property AppID   : String read FAppID;
    property AppName : String read FAppName;

    property ModulesList : TJupiterModuleList   read FModules write FModules;
    property Params      : TJupiterVariableList read FParams  write FParams;
    property Scripts     : TJupiterVariableList read FScripts write FScripts;

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
    procedure RunMacro(prMacroId : String);
    procedure RunAction(prId : Integer; prParams : TJupiterVariableList);
    function RunAcitonEnabled(prId : Integer; prParams : TJupiterVariableList) : Boolean;
    function RunAcitonVisible(prId : Integer; prParams : TJupiterVariableList) : Boolean;

    function GetScriptById(prScriptID : String) : TJupiterScript;
    procedure DeleteScriptById(prScriptID : String);

    constructor Create(prAppID, prAppName : String); virtual;
    destructor Destroy; override;
  end;

var
  vrJupiterApp : TJupiterApp;

implementation

uses FileInfo, SysUtils, SQLDB, uJupiterAppScript, uJupiterDatabaseScript;

{ TJupiterApp }

procedure TJupiterApp.Internal_AddScriptLibraries(var prScript : TJupiterScript);
begin
  prScript.LibraryList.Add(TJupiterAppScript.Create);
  prScript.LibraryList.Add(TJupiterEnviromentcript.Create);
  prScript.LibraryList.Add(TJupiterRunnableScript.Create);
  prScript.LibraryList.Add(TJupiterStringUtilsScript.Create);
  prScript.LibraryList.Add(TuJupiterDatabaseScript.Create);
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

  Self.Params.AddChildList(prModule.Params);
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

    vrScript.Script.AddStrings(JupiterStringUtilsStringToStringList(vrQry.FieldByName('MACRO').AsString));

    vrScript.Execute;
  finally
    FreeAndNil(vrScript);
  end;
end;

procedure TJupiterApp.RunMacro(prMacroId: String);
var
  vrScript : TJupiterScript;
  vrQry    : TSQLQuery;
begin
  vrScript := Self.NewScript;
  vrQry    := Self.NewWizard.NewQuery;
  try
    vrQry.SQL.Add(' SELECT ID, MACRO FROM MACROS WHERE MACROID = :PRID ');
    vrQry.ParamByName('PRID').AsString := prMacroId;
    vrQry.Open;

    vrScript.Script.AddStrings(JupiterStringUtilsStringToStringList(vrQry.FieldByName('MACRO').AsString));

    vrScript.Execute;
  finally
    FreeAndNil(vrScript);
  end;
end;

procedure TJupiterApp.RunAction(prId: Integer; prParams: TJupiterVariableList);
var
  vrScript : TJupiterScript;
  vrQry    : TSQLQuery;
begin
  vrScript := Self.NewScript;
  vrQry    := Self.NewWizard.NewQuery;
  try
    vrQry.SQL.Add(' SELECT A1.ID, M1.MACRO ');
    vrQry.SQL.Add(' FROM ACTIONS A1 ');
    vrQry.SQL.Add('   INNER JOIN MACROS M1 ON (A1.MACRO = M1.ID) ');
    vrQry.SQL.Add(' WHERE A1.ID = :PRID ');
    vrQry.ParamByName('PRID').AsInteger := prId;
    vrQry.Open;

    if vrQry.EOF then
      Exit;

    if vrQry.Fields[1].IsNull then
      Exit;

    vrScript.Script.AddStrings(JupiterStringUtilsStringToStringList(vrQry.FieldByName('MACRO').AsString));
    vrScript.Params.AddChildList(prParams);
    vrScript.Execute;
  finally
    FreeAndNil(vrScript);
  end;
end;

function TJupiterApp.RunAcitonEnabled(prId: Integer; prParams: TJupiterVariableList): Boolean;
var
  vrScript : TJupiterScript;
  vrQry    : TSQLQuery;
begin
  Result := True;

  vrScript := Self.NewScript;
  vrQry    := Self.NewWizard.NewQuery;
  try
    vrQry.SQL.Add(' SELECT A1.ID, M1.MACRO ');
    vrQry.SQL.Add(' FROM ACTIONS A1 ');
    vrQry.SQL.Add('   INNER JOIN MACROS M1 ON (A1.MACRO_ENABLE = M1.ID) ');
    vrQry.SQL.Add(' WHERE A1.ID = :PRID ');
    vrQry.ParamByName('PRID').AsInteger := prId;
    vrQry.Open;

    if vrQry.EOF then
      Exit;

    if vrQry.Fields[1].IsNull then
      Exit;

    vrScript.Params.AddVariable('ScriptID', vrScript.ScriptID, 'ScriptID');
    vrScript.Params.AddVariable('Result', BOOL_FALSE_STR, 'Result');

    vrScript.Script.AddStrings(JupiterStringUtilsStringToStringList(vrQry.FieldByName('MACRO').AsString));
    vrScript.Params.AddChildList(prParams);
    vrScript.Execute;

    Result := False;

    if vrScript.Params.Exists('Result') then
      Result := vrScript.Params.VariableById('Result').AsBool;
  finally
    FreeAndNil(vrScript);
  end;
end;

function TJupiterApp.RunAcitonVisible(prId: Integer; prParams: TJupiterVariableList): Boolean;
var
  vrScript : TJupiterScript;
  vrQry    : TSQLQuery;
begin
  Result := True;

  vrScript := Self.NewScript;
  vrQry    := Self.NewWizard.NewQuery;
  try
    vrQry.SQL.Add(' SELECT A1.ID, M1.MACRO ');
    vrQry.SQL.Add(' FROM ACTIONS A1 ');
    vrQry.SQL.Add('   INNER JOIN MACROS M1 ON (A1.MACRO_VISIBLE = M1.ID) ');
    vrQry.SQL.Add(' WHERE A1.ID = :PRID ');
    vrQry.ParamByName('PRID').AsInteger := prId;
    vrQry.Open;

    if vrQry.EOF then
      Exit;

    if vrQry.Fields[1].IsNull then
      Exit;

    vrScript.Script.AddStrings(JupiterStringUtilsStringToStringList(vrQry.FieldByName('MACRO').AsString));
    vrScript.Params.AddChildList(prParams);
    vrScript.Execute;

    Result := False;

    if vrScript.Params.Exists('Result') then
      Result := vrScript.Params.VariableById('Result').AsBool;
  finally
    FreeAndNil(vrScript);
  end;
end;

function TJupiterApp.GetScriptById(prScriptID: String): TJupiterScript;
var
  vrVez : Integer;
begin
  Result := nil;

  for vrVez := 0 to Self.Scripts.Count - 1 do
    with TJupiterScript(Self.Scripts.GetAtIndex(vrVez)) do
      if ScriptID = prScriptID then
      begin
        Result := TJupiterScript(Self.Scripts.GetAtIndex(vrVez));
        Exit;
      end;
end;

procedure TJupiterApp.DeleteScriptById(prScriptID: String);
var
  vrVez : Integer;
begin
  for vrVez := 0 to Self.Scripts.Count - 1 do
    with TJupiterScript(Self.Scripts.GetAtIndex(vrVez)) do
      if ScriptID = prScriptID then
      begin
        Self.Scripts.DeleteListItem(vrVez);

        Exit;
      end;
end;

constructor TJupiterApp.Create(prAppID, prAppName: String);
begin
  Self.FAppID   := prAppID;
  Self.FAppName := prAppName;

  Self.FParams  := TJupiterVariableList.Create;
  Self.FModules := TJupiterModuleList.Create;
  Self.FScripts := TJupiterVariableList.Create;

  Self.Internal_Prepare;
end;

destructor TJupiterApp.Destroy;
begin
  FreeAndNil(Self.FParams);
  FreeAndNil(Self.FModules);
  FreeAndNil(Self.FScripts);

  inherited Destroy;
end;

end.

