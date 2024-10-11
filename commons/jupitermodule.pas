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
    procedure Internal_SetVariableValue(prID, prNewValue : String);

    function Internal_CreateRouteIfDontExists(prTitle, prRoute : String; prDestiny, prIcon, prZIndex : Integer) : Boolean;
    function Internal_CreateMacroIfDontExists(prID, prTitle : String; prMacro : TStrings) : Boolean;
    function Internal_CreateVariablIfDontExists(prId, prName, prValue : String) : Boolean;
    function Internal_CreateActionIfDontExists(prId, prTitle, prTable : String; prIcon, prZIndex : Integer; prMacro, prMacroEnabled, prMacroVisible : TStrings) : Boolean;
    function Internal_CreateActionIfDontExists(prId, prTitle, prTable : String; prIcon, prZIndex : Integer; prMacro, prMacroEnabled, prMacroVisible : Integer) : Boolean;

    procedure Internal_AddConfigFromDatabase(prId : String);

    function Internal_GetMacroById(prMacroID : String): Integer;
  published
    property ModuleID    : String               read Internal_GetModuleID;
    property ModuleTitle : String               read Internal_GetModuleTitle;
    property Params      : TJupiterVariableList read FParams write FParams;
  public
    function DefineParamName(prName : String) : String;
    procedure ExecuteCommand(prParamList : TStrings); virtual;
    function ModuleDatabaseID : Integer;

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

uses JupiterApp, SQLDB;

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
 // Self.Params.FileName := 'datasets/' + StringReplace(Self.ModuleID, '.', EmptyStr, [rfIgnoreCase, rfReplaceAll]) + '.csv';

  vrWizard := vrJupiterApp.NewWizard;
  try
    if not vrWizard.Exists('MODULES', ' MODULEID = "' + Self.ModuleID + '" ') then
      vrWizard.ExecuteScript(CreateStringList(' INSERT INTO MODULES (NAME, MODULEID) VALUES ("' + Self.ModuleTitle + '", "' + Self.ModuleID + '") '));
  finally
    FreeAndNil(vrWizard);
  end;
end;

procedure TJupiterModule.Internal_SetVariableValue(prID, prNewValue: String);
var
  vrQry    : TSQLQuery;
  vrWizard : TJupiterDatabaseWizard;
begin
  vrWizard := vrJupiterApp.NewWizard;
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

function TJupiterModule.Internal_CreateRouteIfDontExists(prTitle, prRoute: String; prDestiny, prIcon, prZIndex: Integer): Boolean;
var
  vrWizard  : TJupiterDatabaseWizard;
  vrIcon    : String;
  vrZIndex  : String;
  vrDestiny : String;
begin
  Result := False;

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

    if prDestiny = NULL_KEY then
      vrDestiny := 'NULL'
    else
      vrDestiny := IntToStr(prDestiny);

    vrWizard.ExecuteScript(CreateStringList(' INSERT INTO ROUTES (TITLE, ROUTE, ICON, ZINDEX, DESTINY) VALUES ("' + prTitle + '", "' + prRoute + '", ' + vrIcon + ', ' + vrZIndex + ', ' + vrDestiny + ') '));

    Result := True;
  finally
    FreeAndNil(vrWizard);
  end;
end;

function TJupiterModule.Internal_CreateMacroIfDontExists(prID, prTitle: String; prMacro: TStrings): Boolean;
var
  vrWizard : TJupiterDatabaseWizard;
  vrQry : TSQLQuery;
begin
  Result := False;

  vrWizard := vrJupiterApp.NewWizard;
  try
    vrQry := vrWizard.NewQuery;

    if vrWizard.Exists('MACROS', Format(' MACROID = "%0:s" ', [prID])) then
      Exit;

    vrQry.Close;
    vrQry.SQL.Clear;
    vrQry.SQL.Add(' SELECT * FROM MACROS WHERE 1 = 2 ');
    vrQry.InsertSQL.Add(' INSERT INTO MACROS (MACROID, NAME, MACRO) VALUES (:MACROID, :NAME, :MACRO) ');
    vrQry.Open;

    if not vrWizard.Transaction.Active then
      vrWizard.Transaction.StartTransaction;

    try
      vrQry.Insert;
      vrQry.FieldByName('MACROID').AsString := prID;
      vrQry.FieldByName('NAME').AsString    := prTitle;
      vrQry.FieldByName('MACRO').AsString   := prMacro.Text;
      vrQry.Post;
      vrQry.ApplyUpdates(-1);

      vrWizard.Transaction.CommitRetaining;

      Result := True;
    except
      vrWizard.Transaction.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(vrWizard);
    FreeAndNil(vrQry);
  end;
end;

function TJupiterModule.Internal_CreateVariablIfDontExists(prId, prName, prValue : String): Boolean;
var
  vrWizard : TJupiterDatabaseWizard;
  vrQry : TSQLQuery;
begin
  Result := False;

  vrWizard := vrJupiterApp.NewWizard;
  try
    vrQry := vrWizard.NewQuery;

    if vrWizard.Exists('VARIABLES', Format(' NAME = "%0:s" ', [prID])) then
    begin
      Self.Internal_AddConfigFromDatabase(prId);

      Exit;
    end;

    vrQry.Close;
    vrQry.SQL.Clear;
    vrQry.SQL.Add(' SELECT * FROM VARIABLES WHERE 1 = 2 ');
    vrQry.InsertSQL.Add(' INSERT INTO VARIABLES (NAME, DESCRIPTION, VALUE, MODULE) VALUES (:NAME, :DESCRIPTION, :VALUE, :MODULE) ');
    vrQry.Open;

    if not vrWizard.Transaction.Active then
      vrWizard.Transaction.StartTransaction;

    try
      vrQry.Insert;
      vrQry.FieldByName('NAME').AsString        := prId;
      vrQry.FieldByName('DESCRIPTION').AsString := prName;
      vrQry.FieldByName('VALUE').AsString       := prValue;
      vrQry.FieldByName('MODULE').AsInteger     := Self.ModuleDatabaseID;
      vrQry.Post;
      vrQry.ApplyUpdates(-1);

      vrWizard.Transaction.CommitRetaining;

      Self.Params.AddConfig(prId, prValue, prName);

      Self.Params.VariableById(vrQry.FieldByName('NAME').AsString).OnChangeValue := @Internal_SetVariableValue;

      Result := True;
    except
      vrWizard.Transaction.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(vrWizard);
    FreeAndNil(vrQry);
  end;
end;

function TJupiterModule.Internal_CreateActionIfDontExists(prId, prTitle, prTable : String; prIcon, prZIndex : Integer; prMacro, prMacroEnabled, prMacroVisible : TStrings): Boolean;
var
  vrWizard : TJupiterDatabaseWizard;
  vrQry : TSQLQuery;
  vrMacro : Integer;
  vrMacroEnable : Integer;
  vrMacroVisible : Integer;
begin
  Result := False;

  vrWizard := vrJupiterApp.NewWizard;
  try
    vrQry := vrWizard.NewQuery;

    if vrWizard.Exists('ACTIONS', Format(' TABLENAME = "%0:s" AND NAME = "%1:s" ', [prTable, prId])) then
      Exit;

    vrQry.Close;
    vrQry.SQL.Clear;
    vrQry.SQL.Add(' SELECT * FROM ACTIONS WHERE 1 = 2 ');
    vrQry.InsertSQL.Add(' INSERT INTO ACTIONS (NAME, TITLE, TABLENAME, ICON, ZINDEX, MACRO, MACRO_ENABLE, MACRO_VISIBLE) VALUES (:NAME, :TITLE, :TABLENAME, :ICON, :ZINDEX, :MACRO, :MACRO_ENABLE, :MACRO_VISIBLE) ');
    vrQry.Open;

    if not vrWizard.Transaction.Active then
      vrWizard.Transaction.StartTransaction;

    Self.Internal_CreateMacroIfDontExists(prId + '.OnClick', prId + '.OnClick', prMacro);

    vrMacro := vrWizard.GetLastID('MACROS');

    Self.Internal_CreateMacroIfDontExists(prId + '.Enabled', prId + '.Enabled', prMacroEnabled);

    vrMacroEnable := vrWizard.GetLastID('MACROS');

    Self.Internal_CreateMacroIfDontExists(prId + '.Visible', prId + '.Visible', prMacroVisible);

    vrMacroVisible := vrWizard.GetLastID('MACROS');

    try
      vrQry.Insert;
      vrQry.FieldByName('NAME').AsString := prId;
      vrQry.FieldByName('TITLE').AsString := prTitle;
      vrQry.FieldByName('TABLENAME').AsString := prTable;
      vrQry.FieldByName('ICON').AsInteger := prIcon;
      vrQry.FieldByName('ZINDEX').AsInteger := prZIndex;
      vrQry.FieldByName('MACRO').AsInteger := vrMacro;
      vrQry.FieldByName('MACRO_ENABLE').AsInteger := vrMacroEnable;
      vrQry.FieldByName('MACRO_VISIBLE').AsInteger := vrMacroVisible;
      vrQry.Post;
      vrQry.ApplyUpdates(-1);

      vrWizard.Transaction.CommitRetaining;

      Result := True;
    except
      vrWizard.Transaction.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(vrWizard);
    FreeAndNil(vrQry);
  end;
end;

function TJupiterModule.Internal_CreateActionIfDontExists(prId, prTitle, prTable: String; prIcon, prZIndex: Integer; prMacro, prMacroEnabled, prMacroVisible: Integer): Boolean;
var
  vrWizard : TJupiterDatabaseWizard;
  vrQry : TSQLQuery;
begin
  Result := False;

  vrWizard := vrJupiterApp.NewWizard;
  try
    vrQry := vrWizard.NewQuery;

    if vrWizard.Exists('ACTIONS', Format(' TABLENAME = "%0:s" AND NAME = "%1:s" ', [prTable, prId])) then
      Exit;

    vrQry.Close;
    vrQry.SQL.Clear;
    vrQry.SQL.Add(' SELECT * FROM ACTIONS WHERE 1 = 2 ');
    vrQry.InsertSQL.Add(' INSERT INTO ACTIONS (NAME, TITLE, TABLENAME, ICON, ZINDEX, MACRO, MACRO_ENABLE, MACRO_VISIBLE) VALUES (:NAME, :TITLE, :TABLENAME, :ICON, :ZINDEX, :MACRO, :MACRO_ENABLE, :MACRO_VISIBLE) ');
    vrQry.Open;

    if not vrWizard.Transaction.Active then
      vrWizard.Transaction.StartTransaction;

    try
      vrQry.Insert;
      vrQry.FieldByName('NAME').AsString := prId;
      vrQry.FieldByName('TITLE').AsString := prTitle;
      vrQry.FieldByName('TABLENAME').AsString := prTable;
      vrQry.FieldByName('ICON').AsInteger := prIcon;
      vrQry.FieldByName('ZINDEX').AsInteger := prZIndex;
      vrQry.FieldByName('MACRO').AsInteger := prMacro;
      vrQry.FieldByName('MACRO_ENABLE').AsInteger := prMacroEnabled;
      vrQry.FieldByName('MACRO_VISIBLE').AsInteger := prMacroVisible;
      vrQry.Post;
      vrQry.ApplyUpdates(-1);

      vrWizard.Transaction.CommitRetaining;

      Result := True;
    except
      vrWizard.Transaction.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(vrWizard);
    FreeAndNil(vrQry);
  end;
end;

procedure TJupiterModule.Internal_AddConfigFromDatabase(prId: String);
var
  vrWizard : TJupiterDatabaseWizard;
  vrQry : TSQLQuery;
begin
  vrWizard := vrJupiterApp.NewWizard;
  vrQry    := vrWizard.NewQuery;
  try
    vrQry.SQL.Add(' SELECT * FROM VARIABLES WHERE NAME = :PRNAME ');
    vrQry.ParamByName('PRNAME').AsString := prId;
    vrQry.Open;

    if  vrQry.EOF then
      Exit;

    Self.Params.AddConfig(vrQry.FieldByName('NAME').AsString, vrQry.FieldByName('VALUE').AsString, vrQry.FieldByName('DESCRIPTION').AsString);

    Self.Params.VariableById(vrQry.FieldByName('NAME').AsString).OnChangeValue := @Internal_SetVariableValue;
  finally
    FreeAndNil(vrQry);

    FreeAndNil(vrWizard);
  end;
end;

function TJupiterModule.Internal_GetMacroById(prMacroID: String): Integer;
var
  vrWizard : TJupiterDatabaseWizard;
begin
  Result := NULL_KEY;

  vrWizard := vrJupiterApp.NewWizard;
  try
    Result :=  vrWizard.GetField('MACROS', 'ID', 'MACROID = "' + prMacroID + '"');
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

function TJupiterModule.ModuleDatabaseID: Integer;
var
  vrWizard : TJupiterDatabaseWizard;
  vrQry : TSQLQuery;
begin
  Result := NULL_KEY;

  vrWizard := vrJupiterApp.NewWizard;
  try
    Result := vrWizard.GetField('MODULES', 'ID', ' MODULEID = "' + Self.ModuleID + '" ');
  finally
    FreeAndNil(vrWizard);
  end;
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

