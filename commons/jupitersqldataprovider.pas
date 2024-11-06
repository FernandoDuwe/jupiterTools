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
  end;

implementation

{ TJupiterSQLDataProvider }

procedure TJupiterSQLDataProvider.ProvideData;
var
  vrQry : TSQLQuery;
  vrVez : Integer;
begin
  inherited ProvideData;

  vrQry := vrJupiterApp.NewWizard.NewQuery;
  try
    vrQry.SQL.Add(Self.Query);
    vrQry.Open;
    vrQry.First;

    while not vrQry.EOF do
    begin
      Self.AddRow;

      for vrVez := 0 to vrQry.Fields.Count - 1 do
      begin
        if vrQry.Fields[vrVez].IsNull then
          Self.GetLastRow.Fields.AddVariable(vrQry.Fields[vrVez].FieldName, EmptyStr)
        else
          Self.GetLastRow.Fields.AddVariable(vrQry.Fields[vrVez].FieldName, vrQry.Fields[vrVez].AsString);
      end;

      vrQry.Next;
    end;
  finally
    FreeAndNil(vrQry);
  end;
end;

end.

