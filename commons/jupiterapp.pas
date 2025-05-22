unit JupiterApp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterObject, JupiterModule, JupiterEnviroment, jupiterScriptList,
  JupiterVariable, jupiterDatabaseWizard, jupiterScript, jupiterStringUtils,
  JupiterConsts, uJupiterEnviromentScript, uJupiterStringUtilsScript,
  uJupiterRunnableScript, uJupiterDataProviderScript, uJupiterDateUtilsScript,
  uJupiterCheckListUtilsScript, SQLite3Conn, JupiterDataProvider, jupiterthread;

type

  { TJupiterApp }

  TJupiterApp = class(TJupiterObject)
  private
    FAppReady         : Boolean;
    FAppID            : String;
    FAppName          : String;
    FModules          : TJupiterModuleList;
    FParams           : TJupiterVariableList;
    FScripts          : TJupiterVariableList;
    FInternalDatabase : TSQLite3Connection;
    FScriptList       : TJupiterDataProvider;
    FMessageList      : TJupiterDataProvider;
    FThreadList       : TJupiterThreadList;
    FScriptLineList   : TJupiterScriptList;

    procedure Internal_OnExecute(prScript, prMessages, prRunMessages : TStrings; prExecuted : Boolean);
    procedure Internal_SetVariableValue(prID, prNewValue : String);
  protected
    procedure Internal_Prepare; virtual;
    procedure Internal_AddScriptLibraries(var prScript : TJupiterScript); virtual;
  published
    property AppReady : Boolean read FAppReady;
    property AppID    : String  read FAppID;
    property AppName  : String  read FAppName;

    property ModulesList    : TJupiterModuleList   read FModules        write FModules;
    property Params         : TJupiterVariableList read FParams         write FParams;
    property Scripts        : TJupiterVariableList read FScripts        write FScripts;
    property ScriptLineList : TJupiterScriptList   read FScriptLineList write FScriptLineList;
    property ThreadList     : TJupiterThreadList   read FThreadList     write FThreadList;

    property InternalDatabase : TSQLite3Connection read FInternalDatabase write FInternalDatabase;
    property ScriptList : TJupiterDataProvider read FScriptList write FScriptList;
    property MessageList : TJupiterDataProvider read FMessageList write FMessageList;
  public
    DataProviders : TJupiterObjectList;
    GlobalReferences : TJupiterObjectList;

    procedure AddModule(prModule : TJupiterModule);
    procedure AddMessage(prTitle, prMessage, prOrigin : String);
    procedure LoadOtherVariables;
    procedure Prepare;

    function GetVersion : String;
    function ConsoleMode : Boolean;

    function NewWizard : TJupiterDatabaseWizard;
    function NewScript : TJupiterScript;
    procedure SetInternalWizardData(prWizard : TJupiterDatabaseWizard);

    function ExistsMacro(prMacroId : String) : Boolean;

    procedure RunMacro(prId : Integer; prParams : TJupiterVariableList);
    procedure RunMacro(prMacroId : String; prParams : TJupiterVariableList);
    procedure RunMacroNoMessage(prMacroId : String; prParams : TJupiterVariableList);
    procedure RunMacroFromFile(prMacroFile : String; prParams : TJupiterVariableList);
    procedure RunMacroFromFileInThread(prMacroFile : String; prParams : TJupiterVariableList);
    procedure RunMacroInThread(prMacroId : String; prParams : TJupiterVariableList);
    procedure RunScript(prMacro : TStrings; prParams : TJupiterVariableList);
    procedure RunAction(prId : Integer; prParams : TJupiterVariableList);
    function RunAcitonEnabled(prId : Integer; prParams : TJupiterVariableList) : Boolean;
    function RunAcitonVisible(prId : Integer; prParams : TJupiterVariableList) : Boolean;
    function RunMacroAsResult(prMacroId : String; prParams : TJupiterVariableList) : String;

    function GetScriptById(prScriptID : String) : TJupiterScript;
    procedure DeleteScriptById(prScriptID : String);

    function GetDataProviderById(prDataProviderID : String) : TJupiterDataProvider;
    procedure DeleteDataProviderById(prDataProviderID : String);

    // Global References
    procedure AddGlobalReference(prReference : TJupiterDatabaseReference);
    function  GlobalReferenceExists(prTableName : String) : Boolean;
    function  GetGlobalReference(prTablename : String) : TJupiterDatabaseReference;
    procedure RemoveReference(prTableName : String; prID : Integer);

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
  prScript.LibraryList.Add(TJupiterDateUtilsScript.Create);
  prScript.LibraryList.Add(TuJupiterDatabaseScript.Create);
  prScript.LibraryList.Add(TJupiterDataProviderScript.Create);
  prScript.LibraryList.Add(TJupiterCheckListUtilsScript.Create);
end;

procedure TJupiterApp.Internal_OnExecute(prScript, prMessages, prRunMessages: TStrings; prExecuted: Boolean);
begin
  Self.ScriptList.AddRow;

  with Self.ScriptList.GetLastRow do
  begin
    Fields.AddVariable('script', prScript.Text);
    Fields.AddVariable('messages', prMessages.Text);
    Fields.AddVariable('runMessages', prRunMessages.Text);
    Fields.AddVariable('executed', JupiterStringUtilsBoolToStr(prExecuted));
  end;
end;

procedure TJupiterApp.Internal_SetVariableValue(prID, prNewValue: String);
var
  vrQry    : TSQLQuery;
  vrWizard : TJupiterDatabaseWizard;
begin
  vrWizard := Self.NewWizard;
  vrQry    := vrWizard.NewQuery;
  try
    vrQry.SQL.Add(' UPDATE VARIABLES ');
    vrQry.SQL.Add(' SET VALUE = :PRVALUE ');
    vrQry.SQL.Add(' WHERE NAME = :PRNAME ');
    vrQry.ParamByName('PRNAME').AsString  := prID;
    vrQry.ParamByName('PRVALUE').AsString := prNewValue;

    vrWizard.StartTransaction;

    try
      vrQry.ExecSQL;

      vrWizard.Commit;
    except
      vrWizard.Rollback;
      raise;
    end;
  finally
    FreeAndNil(vrQry);
    FreeAndNil(vrWizard);
  end;
end;

procedure TJupiterApp.Internal_Prepare;
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.CreatePath('/assets/');
    vrEnviroment.CreatePath('/checklists/');
    vrEnviroment.CreatePath('/datasets/');
    vrEnviroment.CreatePath('/temp/');
    vrEnviroment.CreatePath('/out/');

    Self.FParams.FileName := vrEnviroment.FullPath('/datasets/config.csv');

    if not Self.FParams.Exists('database.local') then
      Self.FParams.AddConfig('database.local', '/datasets/' + Self.AppID + '.db', 'Base de dados local');

    Self.FAppReady := True;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterApp.AddModule(prModule: TJupiterModule);
begin
  Self.ModulesList.Add(prModule);

  Self.Params.AddChildList(prModule.Params);
end;

procedure TJupiterApp.AddMessage(prTitle, prMessage, prOrigin: String);
begin
  Self.MessageList.AddRow;

  with Self.MessageList.GetLastRow do
  begin
    Fields.AddVariable('title', prTitle);
    Fields.AddVariable('message', prMessage);
    Fields.AddVariable('origin', prOrigin);
    Fields.AddVariable('dateTime', FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));
  end;
end;

procedure TJupiterApp.LoadOtherVariables;
var
  vrQry : TSQLQuery;
begin
  vrQry := Self.NewWizard.NewQuery;
  try
    vrQry.SQL.Add(' SELECT * FROM VARIABLES ');
    vrQry.Open;

    while not vrQry.EOF do
    begin
      if Self.Params.Exists(vrQry.FieldByName('NAME').AsString) then
      begin
        vrQry.Next;
        Continue;
      end;

      Self.Params.AddVariable(vrQry.FieldByName('NAME').AsString,
                              vrQry.FieldByName('VALUE').AsString,
                              vrQry.FieldByName('DESCRIPTION').AsString);

      Self.Params.VariableById(vrQry.FieldByName('NAME').AsString).OnChangeValue := @Internal_SetVariableValue;

      vrQry.Next;
    end;
  finally
    vrQry.Close;
    FreeAndNil(vrQry);
  end;
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
  Result.OnExecute := @Internal_OnExecute;

  Self.Internal_AddScriptLibraries(Result);
end;

procedure TJupiterApp.SetInternalWizardData(prWizard: TJupiterDatabaseWizard);
begin
  prWizard.Connection  := Self.InternalDatabase;
  prWizard.Transaction := Self.InternalDatabase.Transaction;
end;

function TJupiterApp.ExistsMacro(prMacroId: String): Boolean;
begin
  Result := Self.NewWizard.Exists('MACROS', '  MACROID = "' + prMacroId + '" ');
end;

procedure TJupiterApp.RunMacro(prId: Integer; prParams : TJupiterVariableList);
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

    if vrQry.EOF then
      raise Exception.Create('A macro com ID ' + IntToStr(prId) + ' não foi encontrada.');

    vrScript.Script.AddStrings(JupiterStringUtilsStringToStringList(vrQry.FieldByName('MACRO').AsString));
    vrScript.Params.CopyValues(prParams);

    vrScript.Execute;
  finally
    FreeAndNil(vrScript);
    FreeAndNil(vrQry);

    FreeAndNil(prParams);
  end;
end;

procedure TJupiterApp.RunMacro(prMacroId: String; prParams : TJupiterVariableList);
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

    if vrQry.EOF then
      raise Exception.Create('A macro ' + prMacroId + ' não foi encontrada.');

    vrScript.Script.AddStrings(JupiterStringUtilsStringToStringList(vrQry.FieldByName('MACRO').AsString));
    vrScript.Params.CopyValues(prParams);

    vrScript.Execute;
  finally
    FreeAndNil(vrScript);
    FreeAndNil(vrQry);
    FreeAndNil(prParams);
  end;
end;

procedure TJupiterApp.RunMacroNoMessage(prMacroId: String; prParams: TJupiterVariableList);
var
  vrScript : TJupiterScript;
  vrQry    : TSQLQuery;
begin
  vrScript := Self.NewScript;
  vrQry    := Self.NewWizard.NewQuery;
  try
    if not Self.Params.VariableById(DEBUG_MODE).AsBool then
      vrScript.OnExecute := nil;

    vrQry.SQL.Add(' SELECT ID, MACRO FROM MACROS WHERE MACROID = :PRID ');
    vrQry.ParamByName('PRID').AsString := prMacroId;
    vrQry.Open;

    if vrQry.EOF then
      raise Exception.Create('A macro ' + prMacroId + ' não foi encontrada.');

    vrScript.Script.AddStrings(JupiterStringUtilsStringToStringList(vrQry.FieldByName('MACRO').AsString));
    vrScript.Params.CopyValues(prParams);

    vrScript.Execute;
  finally
    FreeAndNil(vrScript);
    FreeAndNil(vrQry);
    FreeAndNil(prParams);
  end;
end;

procedure TJupiterApp.RunMacroFromFile(prMacroFile: String; prParams: TJupiterVariableList);
var
  vrScript : TJupiterScript;
begin
  vrScript := Self.NewScript;
  try
    vrScript.LoadFromFile(prMacroFile);
    vrScript.Params.CopyValues(prParams);

    vrScript.Execute;
  finally
    FreeAndNil(vrScript);
    FreeAndNil(prParams);
  end;
end;

procedure TJupiterApp.RunMacroFromFileInThread(prMacroFile: String; prParams: TJupiterVariableList);
var
  vrScript : TJupiterScript;
begin
  vrScript := Self.NewScript;
  vrScript.LoadFromFile(prMacroFile);
  vrScript.Params.CopyValues(prParams);

  Self.ThreadList.NewThread('Macro: ' + prMacroFile, vrScript);
end;

procedure TJupiterApp.RunMacroInThread(prMacroId: String; prParams: TJupiterVariableList);
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

    if vrQry.EOF then
      raise Exception.Create('A macro ' + prMacroId + ' não foi encontrada.');

    vrScript.Script.AddStrings(JupiterStringUtilsStringToStringList(vrQry.FieldByName('MACRO').AsString));
    vrScript.Params.CopyValues(prParams);

    Self.ThreadList.NewThread('Macro: ' + prMacroId, vrScript);
  finally
    FreeAndNil(vrQry);
  end;
end;

procedure TJupiterApp.RunScript(prMacro: TStrings; prParams: TJupiterVariableList);
var
  vrScript : TJupiterScript;
begin
  vrScript := Self.NewScript;
  try
    vrScript.Script.AddStrings(prMacro);
    vrScript.Params.CopyValues(prParams);

    vrScript.Execute;
  finally
    FreeAndNil(vrScript);
    FreeAndNil(prParams);
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
    vrScript.Params.CopyValues(prParams);

    vrScript.Execute;
  finally
    FreeAndNil(vrQry);
    FreeAndNil(vrScript);
    FreeAndNil(prParams);
  end;
end;

function TJupiterApp.RunAcitonEnabled(prId: Integer; prParams: TJupiterVariableList): Boolean;
var
  vrScript : TJupiterScript;
  vrQry    : TSQLQuery;
  vrWizard : TJupiterDatabaseWizard;
begin
  Result := True;

  vrScript := Self.NewScript;
  vrWizard := Self.NewWizard;
  vrQry    := vrWizard.NewQuery;
  try
    if not Self.Params.VariableById(DEBUG_MODE).AsBool then
      vrScript.OnExecute := nil;

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
    vrScript.Params.CopyValues(prParams);
    vrScript.Execute;

    Result := False;

    if vrScript.Params.Exists('Result') then
      Result := vrScript.Params.VariableById('Result').AsBool;
  finally
    FreeAndNil(vrWizard);
    FreeAndNil(vrScript);
    FreeAndNil(prParams);
    FreeAndNil(vrQry);
  end;
end;

function TJupiterApp.RunAcitonVisible(prId: Integer; prParams: TJupiterVariableList): Boolean;
var
  vrScript : TJupiterScript;
  vrQry    : TSQLQuery;
  vrWizard : TJupiterDatabaseWizard;
begin
  Result := True;

  vrScript := Self.NewScript;
  vrWizard := Self.NewWizard;
  vrQry    := vrWizard.NewQuery;
  try
    if not Self.Params.VariableById(DEBUG_MODE).AsBool then
      vrScript.OnExecute := nil;

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
    vrScript.Params.CopyValues(prParams);
    vrScript.Execute;

    Result := False;

    if vrScript.Params.Exists('Result') then
      Result := vrScript.Params.VariableById('Result').AsBool;
  finally
    FreeAndNil(vrWizard);
    FreeAndNil(vrScript);
    FreeAndNil(vrQry);
    FreeAndNil(prParams);
  end;
end;

function TJupiterApp.RunMacroAsResult(prMacroId : String; prParams: TJupiterVariableList): String;
var
  vrScript : TJupiterScript;
  vrQry    : TSQLQuery;
  vrWizard : TJupiterDatabaseWizard;
begin
  Result := EmptyStr;

  vrScript := Self.NewScript;
  vrWizard := Self.NewWizard;
  vrQry    := vrWizard.NewQuery;
  try
    if not Self.Params.VariableById(DEBUG_MODE).AsBool then
      vrScript.OnExecute := nil;

    vrQry.SQL.Add(' SELECT ID, MACRO FROM MACROS WHERE MACROID = :PRID ');
    vrQry.ParamByName('PRID').AsString := prMacroId;
    vrQry.Open;

    if vrQry.EOF then
      Exit;

    if vrQry.Fields[1].IsNull then
      Exit;

    vrScript.Script.AddStrings(JupiterStringUtilsStringToStringList(vrQry.FieldByName('MACRO').AsString));
    vrScript.Params.CopyValues(prParams);
    vrScript.Execute;

    Result := EmptyStr;

    if vrScript.Params.Exists('Result') then
      Result := vrScript.Params.VariableById('Result').Value;
  finally
    FreeAndNil(vrWizard);
    FreeAndNil(vrScript);
    FreeAndNil(prParams);
    FreeAndNil(vrQry);
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

function TJupiterApp.GetDataProviderById(prDataProviderID: String): TJupiterDataProvider;
var
  vrVez : Integer;
begin
  Result := nil;

  if not Assigned(DataProviders) then Exit;

  for vrVez := 0 to Self.DataProviders.Count - 1 do
    with TJupiterDataProvider(Self.DataProviders.GetAtIndex(vrVez)) do
      if ProviderID = prDataProviderID then
      begin
        Result := TJupiterDataProvider(Self.DataProviders.GetAtIndex(vrVez));
        Exit;
      end;
end;

procedure TJupiterApp.DeleteDataProviderById(prDataProviderID: String);
var
  vrVez : Integer;
begin
  if not Assigned(DataProviders) then Exit;

  for vrVez := 0 to Self.DataProviders.Count - 1 do
    with TJupiterDataProvider(Self.DataProviders.GetAtIndex(vrVez)) do
      if ProviderID = prDataProviderID then
      begin
        Self.DataProviders.DeleteListItem(vrVez);

        Exit;
      end;
end;

procedure TJupiterApp.AddGlobalReference(prReference: TJupiterDatabaseReference);
begin
  Self.GlobalReferences.Add(prReference);
end;

function TJupiterApp.GlobalReferenceExists(prTableName: String): Boolean;
var
  vrVez : Integer;
begin
  Result := False;

  for vrVez := 0 to Self.GlobalReferences.Count - 1 do
    if TJupiterDatabaseReference(Self.GlobalReferences.GetAtIndex(vrVez)).TableName = prTableName then
    begin
      Result := True;
      Exit;
    end;
end;

function TJupiterApp.GetGlobalReference(prTablename: String): TJupiterDatabaseReference;
var
  vrVez : Integer;
begin
  Result := nil;

  for vrVez := 0 to Self.GlobalReferences.Count - 1 do
    if TJupiterDatabaseReference(Self.GlobalReferences.GetAtIndex(vrVez)).TableName = prTableName then
    begin
      Result := TJupiterDatabaseReference(Self.GlobalReferences.GetAtIndex(vrVez));
      Exit;
    end;
end;

procedure TJupiterApp.RemoveReference(prTableName: String; prID: Integer);
var
  vrVez : Integer;
begin
  for vrVez := 0 to Self.GlobalReferences.Count - 1 do
    if ((TJupiterDatabaseReference(Self.GlobalReferences.GetAtIndex(vrVez)).TableName = prTableName) and (TJupiterDatabaseReference(Self.GlobalReferences.GetAtIndex(vrVez)).ID = prID)) then
    begin
      Self.GlobalReferences.DeleteAtIndex(vrVez);
      Exit;
    end;
end;

constructor TJupiterApp.Create(prAppID, prAppName: String);
begin
  try
    Self.FAppReady := False;

    Self.FAppID   := prAppID;
    Self.FAppName := prAppName;

    Self.FParams          := TJupiterVariableList.Create;
    Self.FModules         := TJupiterModuleList.Create;
    Self.FScripts         := TJupiterVariableList.Create;
    Self.DataProviders    := TJupiterObjectList.Create;
    Self.GlobalReferences := TJupiterObjectList.Create;

    Self.ScriptList := TJupiterDataProvider.Create;
    Self.MessageList := TJupiterDataProvider.Create;
    Self.ThreadList := TJupiterThreadList.Create;

    Self.ScriptLineList := TJupiterScriptList.Create;

    Self.Internal_Prepare;
  finally
    Self.AddMessage('Iniciando', 'Iniciando sistema', Self.ClassName);
  end;
end;

destructor TJupiterApp.Destroy;
begin
  FreeAndNil(Self.FMessageList);
  FreeAndNil(Self.FScriptList);
  FreeAndNil(Self.FParams);
  FreeAndNil(Self.FModules);
  FreeAndNil(Self.FScripts);
  FreeAndNil(Self.DataProviders);
  FreeAndNil(Self.FThreadList);
  FreeAndNil(Self.GlobalReferences);
  FreeAndNil(Self.FScriptLineList);

  inherited Destroy;
end;

end.

