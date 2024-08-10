unit jupiterDatabaseWizard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, JupiterConsts, SQLDB, DB;
type

  { TJupiterDatabaseWizard }

  { TJupiterDatabaseReference }

  TJupiterDatabaseReference = class(TJupiterObject)
  private
    FTableName : String;
    FID : Integer;
  published
    property TableName : String  read FTableName write FTableName;
    property ID        : Integer read FID        write FID;
  public
    constructor Create(prTableName : String; prID : Integer);
  end;

  { TJupiterDatabaseForeignKeyReference }

  TJupiterDatabaseForeignKeyReference = class(TJupiterObject)
  private
    FTableName        : String;
    FFieldName        : String;
    FTableDestinyName : String;
    FFieldDestinyName : String;
    FWizard           : TJupiterObject;
  published
    property TableName        : String read FTableName        write FTableName;
    property FieldName        : String read FFieldName        write FFieldName;
    property TableDestinyName : String read FTableDestinyName write FTableDestinyName;
    property FieldDestinyName : String read FFieldDestinyName write FFieldDestinyName;

    property Wizard           : TJupiterObject read FWizard write FWizard;
  public
    constructor Create(prTableName, prFieldName, prTableDestinyName, prFieldDestinyName : String; prWizard : TJupiterObject);
  end;

  TJupiterDatabaseWizard = class(TJupiterObject)
  private
    FConnection  : TSQLConnection;
    FTransaction : TSQLTransaction;
  published
    property Connection  : TSQLConnection  read FConnection  write FConnection;
    property Transaction : TSQLTransaction read FTransaction write FTransaction;
  public
    function TableExists(prTableName : String) : Boolean;
    function FieldExists(prTableName, prFieldName : String) : Boolean;
    function GetForeignKeyData(prTableName, prFieldName : String) : TJupiterDatabaseForeignKeyReference;
    function IsForeignKeyField(prTableName, prFieldName : String) : Boolean;
    function NewQuery : TSQLQuery;
    function NewQueryFromReference(prReference : TJupiterDatabaseReference) : TSQLQuery;
    function NewScript : TSQLScript;
    function NewDataSourceFromQuery(prQuery : TSQLQuery) : TDataSource;

    function Count(prTableName, prWhere : String) : Integer;
    function Exists(prTableName, prWhere : String) : Boolean;
    function GetLastID(prTableName : String) : Integer;
    function GetField(prTableName, prField, prWhere : String) : Variant;

    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;

    procedure GenerateQuerySQL(prTableName : String; var prStrings : TStrings);
    procedure GenerateInsertSQL(prTableName : String; var prStrings : TStrings);
    procedure GenerateUpdateSQL(prTableName : String; var prStrings : TStrings);
    procedure GenerateDeleteSQL(prTableName : String; var prStrings : TStrings);

    procedure ExecuteScript(prScript : TStrings);

    constructor Create(prConnection : TSQLConnection);
  end;

implementation

{ TJupiterDatabaseReference }

constructor TJupiterDatabaseReference.Create(prTableName: String; prID: Integer
  );
begin
  Self.TableName := prTableName;
  Self.ID        := prID;
end;

{ TJupiterDatabaseForeignKeyReference }

constructor TJupiterDatabaseForeignKeyReference.Create(prTableName, prFieldName, prTableDestinyName, prFieldDestinyName: String; prWizard : TJupiterObject);
begin
  Self.TableName := prTableName;
  Self.FieldName := prFieldName;

  Self.TableDestinyName  := prTableDestinyName;
  Self.FFieldDestinyName := prFieldDestinyName;

  Self.Wizard := prWizard;
end;

{ TJupiterDatabaseWizard }

function TJupiterDatabaseWizard.TableExists(prTableName: String): Boolean;
var
  vrStr : TStrings;
begin
  vrStr := TStringList.Create;

  Self.Connection.GetTableNames(vrStr, False);

  try
    Result := vrStr.IndexOf(prTableName) <> - 1;
  finally
    vrStr.Clear;
    vrStr.Free;
  end;
end;

function TJupiterDatabaseWizard.FieldExists(prTableName, prFieldName: String): Boolean;
var
  vrStr : TStrings;
begin
  vrStr := TStringList.Create;

  Self.Connection.GetFieldNames(prTableName, vrStr);

  try
    Result := vrStr.IndexOf(prFieldName) <> - 1;
  finally
    vrStr.Clear;
    vrStr.Free;
  end;
end;

function TJupiterDatabaseWizard.GetForeignKeyData(prTableName, prFieldName: String): TJupiterDatabaseForeignKeyReference;
var
  vrQry : TSQLQuery;
begin
  Result := nil;

  if not Self.IsForeignKeyField(prTableName, prFieldName) then
    Exit;

  vrQry := Self.NewQuery;
  try
    vrQry.SQL.Add(' SELECT * FROM PRAGMA_FOREIGN_KEY_LIST("' + prTableName + '") ');
    vrQry.Open;
    vrQry.First;

    while not vrQry.EOF do
    begin
      if vrQry.FieldByName('FROM').AsString = prFieldName then
      begin
        Result := TJupiterDatabaseForeignKeyReference.Create(prTableName,
                                                             prFieldName,
                                                             vrQry.FieldByName('TABLE').AsString,
                                                             vrQry.FieldByName('TO').AsString,
                                                             Self);
        Exit;
      end;

      vrQry.Next;
    end;
  finally
    vrQry.Close;
    FreeAndNil(vrQry);
  end;
end;

function TJupiterDatabaseWizard.IsForeignKeyField(prTableName, prFieldName: String): Boolean;
var
  vrQry : TSQLQuery;
begin
  Result := False;

  vrQry := Self.NewQuery;
  try
    vrQry.SQL.Add(' SELECT * FROM PRAGMA_FOREIGN_KEY_LIST("' + prTableName + '") ');
    vrQry.Open;
    vrQry.First;

    while not vrQry.EOF do
    begin
      if vrQry.FieldByName('FROM').AsString = prFieldName then
      begin
        Result := True;
        Exit;
      end;

      vrQry.Next;
    end;
  finally
    vrQry.Close;
    FreeAndNil(vrQry);
  end;
end;

function TJupiterDatabaseWizard.NewQuery: TSQLQuery;
begin
  Result                := TSQLQuery.Create(nil);
  Result.SQLConnection  := Self.Connection;
  Result.SQLTransaction := Self.Transaction;
  Result.Close;
  Result.SQL.Clear;
end;

function TJupiterDatabaseWizard.NewQueryFromReference(prReference: TJupiterDatabaseReference): TSQLQuery;
begin
  Result := NewQuery;

  Result.SQL.Add(String.Format(' SELECT * FROM %0:s WHERE ((ID = %1:d) OR (-1 = %1:d)) ORDER BY 2', [prReference.TableName, prReference.ID]));
end;

function TJupiterDatabaseWizard.NewScript: TSQLScript;
begin
  Result             := TSQLScript.Create(nil);
  Result.DataBase    := Self.Connection;
  Result.Transaction := Self.Transaction;

  Result.Script.Clear;
end;

function TJupiterDatabaseWizard.NewDataSourceFromQuery(prQuery: TSQLQuery): TDataSource;
begin
  Result := TDataSource.Create(prQuery);
  Result.DataSet := prQuery;
end;

function TJupiterDatabaseWizard.Count(prTableName, prWhere: String): Integer;
var
  vrQry : TSQLQuery;
begin
  vrQry := Self.NewQuery;
  try
    vrQry.SQL.Add(Format(' SELECT COUNT(ID) AS COUNTER FROM %0:s WHERE %1:s', [prTableName, prWhere]));
    vrQry.Open;

    Result := vrQry.Fields[0].AsInteger;
  finally
    vrQry.Close;

    FreeAndNil(vrQry);
  end;
end;

function TJupiterDatabaseWizard.Exists(prTableName, prWhere: String): Boolean;
begin
  Result := Self.Count(prTableName, prWhere) > 0;
end;

function TJupiterDatabaseWizard.GetLastID(prTableName: String): Integer;
var
  vrQry : TSQLQuery;
begin
  Result := NULL_KEY;

  vrQry := Self.NewQuery;
  try
    vrQry.SQL.Add(' SELECT MAX(ID) FROM ' + prTableName);
    vrQry.Open;

    Result := vrQry.Fields[0].AsInteger;
  finally
    vrQry.Close;
    FreeAndNil(vrQry);
  end;
end;

function TJupiterDatabaseWizard.GetField(prTableName, prField, prWhere: String): Variant;
var
  vrQry : TSQLQuery;
begin
  Result := Null;

  vrQry := Self.NewQuery;
  try
    vrQry.SQL.Add(Format(' SELECT %0:s FROM %1:s WHERE %2:s ', [prField, prTableName, prWhere]));
    vrQry.Open;
    vrQry.First;

    Result := vrQry.Fields[0].Value;
  finally
    vrQry.Close;
    FreeAndNil(vrQry);
  end;
end;

procedure TJupiterDatabaseWizard.StartTransaction;
begin
  if not Self.Transaction.Active then
    Self.Transaction.StartTransaction;
end;

procedure TJupiterDatabaseWizard.Commit;
begin
  Self.Transaction.CommitRetaining;
end;

procedure TJupiterDatabaseWizard.Rollback;
begin
  Self.Transaction.RollbackRetaining;
end;

procedure TJupiterDatabaseWizard.GenerateQuerySQL(prTableName: String; var prStrings: TStrings);
begin
  //
end;

procedure TJupiterDatabaseWizard.GenerateInsertSQL(prTableName: String; var prStrings: TStrings);
begin

end;

procedure TJupiterDatabaseWizard.GenerateUpdateSQL(prTableName: String; var prStrings: TStrings);
begin

end;

procedure TJupiterDatabaseWizard.GenerateDeleteSQL(prTableName: String; var prStrings: TStrings);
begin

end;

procedure TJupiterDatabaseWizard.ExecuteScript(prScript: TStrings);
var
  vrScript : TSQLScript;
begin
  vrScript := Self.NewScript;
  try
    Self.StartTransaction;

    try
      vrScript.Script.AddStrings(prScript);
      vrScript.Execute;

      Self.Commit;
    except
      Self.Rollback;

      raise;
    end;
  finally
    FreeAndNil(vrScript);
  end;
end;

constructor TJupiterDatabaseWizard.Create(prConnection: TSQLConnection);
begin
  Self.Connection  := prConnection;
  Self.Transaction := prConnection.Transaction;
end;

end.

