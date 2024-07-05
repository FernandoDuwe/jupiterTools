unit jupiterDatabaseWizard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, JupiterConsts, SQLDB;

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
    function NewQuery : TSQLQuery;
    function NewScript : TSQLScript;

    function Count(prTableName, prWhere : String) : Integer;
    function Exists(prTableName, prWhere : String) : Boolean;
    function GetLastID(prTableName : String) : Integer;
    function GetField(prTableName, prField, prWhere : String) : Variant;

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

function TJupiterDatabaseWizard.NewQuery: TSQLQuery;
begin
  Result                := TSQLQuery.Create(nil);
  Result.SQLConnection  := Self.Connection;
  Result.SQLTransaction := Self.Transaction;
  Result.Close;
  Result.SQL.Clear;
end;

function TJupiterDatabaseWizard.NewScript: TSQLScript;
begin
  Result             := TSQLScript.Create(nil);
  Result.DataBase    := Self.Connection;
  Result.Transaction := Self.Transaction;

  Result.Script.Clear;
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

procedure TJupiterDatabaseWizard.ExecuteScript(prScript: TStrings);
var
  vrScript : TSQLScript;
begin
  vrScript := Self.NewScript;
  try
    if not Self.Transaction.Active then
      Self.Transaction.StartTransaction;

    try
      vrScript.Script.AddStrings(prScript);
      vrScript.Execute;

      Self.Transaction.CommitRetaining;
    except
      Self.Transaction.RollbackRetaining;
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

