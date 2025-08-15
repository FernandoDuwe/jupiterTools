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

    procedure Internal_GenerateDataOfTable(prTable : String);
  published
    property Connection  : TSQLConnection  read FConnection  write FConnection;
    property Transaction : TSQLTransaction read FTransaction write FTransaction;
  public
    function TableExists(prTableName : String) : Boolean;
    function FieldExists(prTableName, prFieldName : String) : Boolean;
    function GetForeignKeyData(prTableName, prFieldName : String) : TJupiterDatabaseForeignKeyReference;
    function IsForeignKeyField(prTableName, prFieldName : String) : Boolean;
    function NewQuery : TSQLQuery;
    function GetSelectGridFields(prTable : String) : String;
    function NewQueryFromReference(prReference : TJupiterDatabaseReference; prWhere : String = ''; prOrderBy : String = ''; prFields : String = '*'; prLimit : String = '') : TSQLQuery;
    function NewQueryFromReferenceToComboBox(prReference : TJupiterDatabaseReference; prWhere : String = ''; prOrderBy : String = ''; prFields : String = '*'; prLimit : String = '') : TSQLQuery;
    function NewQueryFromReferenceWithSearch(prReference : TJupiterDatabaseReference; prFieldList : TStrings; prSearch : String; prWhere : String = ''; prOrderBy : String = ''; prFields : String = '*'; prLimit : String = '') : TSQLQuery;
    function NewScript : TSQLScript;
    function NewDataSourceFromQuery(prQuery : TSQLQuery) : TDataSource;
    function GetDescriptionFieldFromTable(prTableName : String) : String;

    function Count(prTableName, prWhere : String) : Integer;
    function Exists(prTableName, prWhere : String) : Boolean;
    function GetLastID(prTableName : String) : Integer;
    function GetField(prTableName, prField, prWhere : String) : Variant;
    function GetBLobField(prTableName, prField, prWhere : String) : TStrings;
    function Resolve(prTableName, prField, prWhere : String) : String;

    function GetForeignKeysFromTable(prTableName : String) : TJupiterObjectList;

    procedure UpdateBLOBField(prTableName, prField, prWhere : String; prData : TStrings);

    function GetTableDescription(prTableName : String; prId : Integer) : String;

    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;

    procedure GenerateQuerySQL(prTableName : String; var prStrings : TStrings);
    procedure GenerateInsertSQL(prTableName : String; var prStrings : TStrings);
    procedure GenerateUpdateSQL(prTableName : String; var prStrings : TStrings);
    procedure GenerateDeleteSQL(prTableName : String; var prStrings : TStrings);
    procedure GenerateDatabasStats;

    procedure ExecuteScript(prScript : TStrings; prStartTransaction : Boolean = True);

    constructor Create(prConnection : TSQLConnection); virtual;
  end;

implementation

uses JupiterApp, jupiterStringUtils;

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

procedure TJupiterDatabaseWizard.Internal_GenerateDataOfTable(prTable: String);
var
  vrVez : Integer;
  vrStr : TStrings;
begin
  if Self.Count('DATABASE_TABLETITLE', ' TABLENAME = "' + prTable + '" ') = 0 then
    Self.ExecuteScript(CreateStringList(' INSERT INTO DATABASE_TABLETITLE (TABLENAME, EXPRESSION) VALUES ("' + prTable + '", "{ID}") '), False);

  vrStr := TStringList.Create;
  try
    vrStr.Clear;

    Self.Connection.GetFieldNames(prTable, vrStr);

    if Self.Count('DATABASE_DICTIONARY', ' TABLENAME = "' + prTable + '" AND FIELDNAME IS NULL ') = 0 then
      Self.ExecuteScript(CreateStringList(' INSERT INTO DATABASE_DICTIONARY (TABLENAME, TITLE) VALUES ("' + prTable + '", "' + JupiterStringUtilsNormalizeToPresent(prTable) + '") '), False);

    for vrVez := 0 to vrStr.Count - 1 do
    begin
      if Self.Count('DATABASE_DICTIONARY', ' TABLENAME = "' + prTable + '" AND FIELDNAME = "' + vrStr[vrVez] + '" ') = 0 then
        Self.ExecuteScript(CreateStringList(' INSERT INTO DATABASE_DICTIONARY (TABLENAME, FIELDNAME, TITLE) VALUES ("' + prTable + '", "' + vrStr[vrVez] + '", "' + JupiterStringUtilsNormalizeToPresent(vrStr[vrVez]) + '") '), False);
    end;
  finally
    FreeAndNil(vrStr);
  end;
end;

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

function TJupiterDatabaseWizard.GetSelectGridFields(prTable: String): String;
var
  vrFields : TStrings;
  vrVez    : Integer;
begin
  Result := EmptyStr;

  vrFields := TStringList.Create;
  try
    vrFields.Clear;

    Self.Connection.GetFieldNames(prTable, vrFields);

    for vrVez := 0 to vrFields.Count - 1 do
    begin
      if vrVez > 0 then
        Result := Result + ', ';

      if Self.IsForeignKeyField(prTable, vrFields[vrVez]) then
      begin
        Result := Result + vrFields[vrVez] + ' AS ' + vrFields[vrVez] + '_ID';

        Result := Result + ', CAST("" AS VARCHAR(100))  AS ' + vrFields[vrVez];

        Continue;
      end;

      Result := Result + vrFields[vrVez];
    end;
  finally
    FreeAndNil(vrFields);
  end;
end;

function TJupiterDatabaseWizard.NewQueryFromReference(
  prReference: TJupiterDatabaseReference; prWhere: String; prOrderBy: String;
  prFields: String; prLimit: String): TSQLQuery;
begin
  Result := NewQuery;

  if prOrderBy = '' then
    prOrderBy := '2';

  if prWhere  <> '' then
    prWhere := ' AND ' + prWhere;

  Result.SQL.Add(String.Format(' SELECT ' + prFields + ' FROM %0:s WHERE ((ID = %1:d) OR (-1 = %1:d)) %3:s ORDER BY %2:s %4:s', [prReference.TableName, prReference.ID, prOrderBy, prWhere, prLimit]));
end;

function TJupiterDatabaseWizard.NewQueryFromReferenceToComboBox(prReference: TJupiterDatabaseReference; prWhere: String; prOrderBy: String; prFields: String; prLimit: String): TSQLQuery;
begin
  //
end;

function TJupiterDatabaseWizard.NewQueryFromReferenceWithSearch(
  prReference: TJupiterDatabaseReference; prFieldList: TStrings;
  prSearch: String; prWhere: String; prOrderBy: String; prFields: String;
  prLimit: String): TSQLQuery;
var
  vrVez : Integer;
begin
  Result := NewQuery;

  Result.SQL.Add(' SELECT ' + prFields);
  Result.SQL.Add(' FROM ' + prReference.TableName);
  Result.SQL.Add(' WHERE ( ');

  for vrVez := 0 to prFieldList.Count - 1 do
  begin
    if vrVez > 0 then
       Result.SQL.Add(' OR ');

    Result.SQL.Add(' UPPER(' + prFieldList[vrVez] + ') LIKE UPPER(''%' + prSearch + '%'') ');
  end;

  Result.SQL.Add(' ) ');

  if prWhere  <> '' then
    prWhere := ' AND ' + prWhere;

  Result.SQL.Add(prWhere);

  if prOrderBy = '' then
    prOrderBy := '2';

  Result.SQL.Add(' ORDER BY ' + prOrderBy);
  Result.SQL.Add(prLimit);
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

function TJupiterDatabaseWizard.GetDescriptionFieldFromTable(prTableName: String): String;
var
  vrQry : TSQLQuery;
  vrVez : Integer;
begin
  Result := '"#" || ID';

  vrQry := NewQuery;
  try
    vrQry.SQL.Add(' SELECT * FROM ' + prTableName + ' WHERE ID = -1 ');
    vrQry.Open;

    for vrVez := 0 to vrQry.Fields.Count - 1 do
    begin
      if vrQry.Fields[vrVez].FieldName = 'ID' then
        Continue;

      if vrQry.Fields[vrVez] is TFloatField then
        Continue;

      if vrQry.Fields[vrVez] is TBlobField then
        Continue;

      if vrQry.Fields[vrVez] is TIntegerField then
        Continue;

      if vrQry.Fields[vrVez] is TBooleanField then
        Continue;

      if vrQry.Fields[vrVez] is TFloatField then
        Continue;

      if Result <> EmptyStr then
        Result := Result + ' || ';

      Result := Result + Format(' (CASE WHEN %0:s IS NULL THEN "" ELSE " - " || %0:s END) ', [vrQry.Fields[vrVez].FieldName]);
    end;

    if Result = EmptyStr then
      Result := 'ID';
  finally
    Result := Result + ' AS DESCRIPTION';

    FreeAndNil(vrQry);
  end;
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

function TJupiterDatabaseWizard.GetBLobField(prTableName, prField, prWhere: String): TStrings;
var
  vrQry : TSQLQuery;
  BlobStream: TStream;
  StringStream: TStringStream;
begin
  Result := CreateStringList(EmptyStr);

  vrQry := Self.NewQuery;
  try
    vrQry.SQL.Add(Format(' SELECT %0:s FROM %1:s WHERE %2:s ', [prField, prTableName, prWhere]));
    vrQry.Open;
    vrQry.First;

    BlobStream := TBlobField(vrQry.Fields[0]).DataSet.CreateBlobStream(vrQry.Fields[0], bmRead);
    try
      StringStream := TStringStream.Create;
      try
        StringStream.CopyFrom(BlobStream, BlobStream.Size);
        Result := CreateStringList(StringStream.DataString);
      finally
        StringStream.Free;
      end;
    finally
      BlobStream.Free;
    end;
  finally
    vrQry.Close;
    FreeAndNil(vrQry);
  end;
end;

function TJupiterDatabaseWizard.Resolve(prTableName, prField, prWhere: String): String;
var
  vrQry : TSQLQuery;
begin
  Result := EmptyStr;

  vrQry := Self.NewQuery;
  try
    vrQry.SQL.Add(Format(' SELECT %0:s FROM %1:s WHERE %2:s ', [prField, prTableName, prWhere]));
    vrQry.Open;
    vrQry.First;

    if not vrQry.Fields[1].IsNull then
      Result := vrQry.Fields[1].AsString;
  finally
    vrQry.Close;
    FreeAndNil(vrQry);
  end;
end;

function TJupiterDatabaseWizard.GetForeignKeysFromTable(prTableName: String): TJupiterObjectList;
var
  vrTableList : TStrings;
  vrFieldList : TStrings;
  vrVez : Integer;
  vrVez2 : Integer;
  vrReference : TJupiterDatabaseForeignKeyReference;
begin
  Result := TJupiterObjectList.Create;

  vrTableList := TStringList.Create;
  vrFieldList := TStringList.Create;
  try
    Self.Connection.GetTableNames(vrTableList, False);

    for vrVez := 0 to vrTableList.Count - 1 do
    begin
      if vrTableList[vrVez] = prTableName then
        Continue;

      vrFieldList.Clear;
      Self.Connection.GetFieldNames(vrTableList[vrVez], vrFieldList);

      for vrVez2 := 0 to vrFieldList.Count - 1 do
      begin
        vrReference := Self.GetForeignKeyData(vrTableList[vrVez], vrFieldList[vrVez2]);

        if not Assigned(vrReference) then
          Continue;

       if vrReference.TableDestinyName = prTableName then
          Result.Add(vrReference);
      end;
    end;
  finally
    vrFieldList.Clear;
    vrFieldList.Free;

    vrTableList.Clear;
    vrTableList.Free;
  end;
end;

procedure TJupiterDatabaseWizard.UpdateBLOBField(prTableName, prField, prWhere: String; prData: TStrings);
var
  vrQry : TSQLQuery;
begin
  vrQry := Self.NewQuery;
  try
    vrQry.SQL.Add(Format(' UPDATE %0:s SET %1:s = :PRDATA WHERE %2:s ', [prTableName, prField, prWhere]));
    vrQry.ParamByName('PRDATA').AsString := prData.Text;

    Self.StartTransaction;

    try
      vrQry.ExecSQL;

      Self.Commit;
    except
      Self.Rollback;

      raise;
    end;
  finally
    vrQry.Close;
    FreeAndNil(vrQry);
  end;

end;

function TJupiterDatabaseWizard.GetTableDescription(prTableName: String; prId: Integer): String;
var
  vrQry : TSQLQuery;
  vrVez : Integer;
begin
  Result := prTableName + ' (#' + IntToStr(prId) + ')';

  vrQry := Self.NewQuery;
  try
    vrQry.SQL.Add(Format(' SELECT * FROM %0:s WHERE ID = %1:d ', [prTableName, prId]));
    vrQry.Open;

    if vrQry.EOF then
      Exit;

    for vrVez := 0 to vrQry.FieldCount - 1 do
    begin
       if vrQry.Fields[vrVez].IsNull then
         Continue;

       if vrQry.Fields[vrVez] is TStringField then
       begin
         if Result <> EmptyStr then
           Result := Result + ' - ';

         Result := Result + vrQry.Fields[vrVez].AsString;
       end;

       if vrQry.Fields[vrVez] is TDateField then
       begin
         if Result <> EmptyStr then
           Result := Result + ' - ';

         Result := Result + vrQry.Fields[vrVez].AsString;
       end;

       if vrQry.Fields[vrVez] is TDateTimeField then
       begin
         if Result <> EmptyStr then
           Result := Result + ' - ';

         Result := Result + vrQry.Fields[vrVez].AsString;
       end;

       if vrQry.Fields[vrVez] is TTimeField then
       begin
         if Result <> EmptyStr then
           Result := Result + ' - ';

         Result := Result + vrQry.Fields[vrVez].AsString;
       end;
    end;
  finally
  //  if Length(Result) > vrJupiterApp.Params.VariableById(FORM_DESCRIPTION_MAXSIZE).AsInteger then
  //    Result := Copy(Result, 1, vrJupiterApp.Params.VariableById(FORM_DESCRIPTION_MAXSIZE).AsInteger) + '...';

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

procedure TJupiterDatabaseWizard.GenerateDatabasStats;
var
  vrVez : Integer;
  vrTableList : TStrings;
begin
  vrTableList := TStringList.Create;
  try
    vrTableList.Clear;
    Self.Connection.GetTableNames(vrTableList, False);

    if not Self.Transaction.Active then
      Self.Transaction.StartTransaction;

    try
      for vrVez := 0 to vrTableList.Count - 1 do
      begin
        if Trim(vrTableList[vrVez]) = EmptyStr then
          Continue;

        Self.Internal_GenerateDataOfTable(vrTableList[vrVez]);
      end;

      Self.Transaction.CommitRetaining;
    except
      Self.Transaction.RollbackRetaining;
      raise;
    end;
  finally
    FreeAndNil(vrTableList);
  end;
end;

procedure TJupiterDatabaseWizard.ExecuteScript(prScript: TStrings; prStartTransaction : Boolean = True);
var
  vrScript : TSQLScript;
begin
  vrScript := Self.NewScript;
  try
    if prStartTransaction then
      Self.StartTransaction;

    try
      vrScript.Script.AddStrings(prScript);
      vrScript.Execute;

      if prStartTransaction then
        Self.Commit;
    except
      if prStartTransaction then
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

