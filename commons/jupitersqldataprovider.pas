unit jupitersqldataprovider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterDataProvider, JupiterConsts, JupiterVariable,
  JupiterApp, jupiterDatabaseWizard, SQLDB;

type

  { TJupiterSQLDataProvider }

  TJupiterSQLDataProvider = class(TJupiterDataProvider)
  private
    FQuery : String;
  published
    property Query : String read FQuery write FQuery;
  public
    procedure ProvideData; override;
    procedure ProvideFromQuery(prQry : TSQLQuery); virtual;
  end;

implementation

{ TJupiterSQLDataProvider }

procedure TJupiterSQLDataProvider.ProvideData;
var
  vrQry : TSQLQuery;
begin
  inherited ProvideData;

  vrQry := vrJupiterApp.NewWizard.NewQuery;
  try
    vrQry.SQL.Add(Self.Query);
    vrQry.Open;

    Self.ProvideFromQuery(vrQry);
  finally
    FreeAndNil(vrQry);
  end;
end;

procedure TJupiterSQLDataProvider.ProvideFromQuery(prQry : TSQLQuery);
var
  vrVez : Integer;
begin
  prQry.First;

  while not prQry.EOF do
  begin
    Self.AddRow;

    for vrVez := 0 to prQry.Fields.Count - 1 do
    begin
      if prQry.Fields[vrVez].IsNull then
        Self.GetLastRow.Fields.AddVariable(prQry.Fields[vrVez].FieldName, EmptyStr)
      else
        Self.GetLastRow.Fields.AddVariable(prQry.Fields[vrVez].FieldName, prQry.Fields[vrVez].AsString);
    end;

    prQry.Next;
  end;
end;

end.

