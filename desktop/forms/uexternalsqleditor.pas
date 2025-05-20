unit uExternalSQLEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, Forms, Controls, Graphics, Dialogs, uSQLEditor;

type

  { TFExternalSQLEditor }

  TFExternalSQLEditor = class(TFSQLEditor)
    sqlConector: TSQLConnector;
    sqlLiteInternalTransaction: TSQLTransaction;
  private
    function Internal_GetConnection  : TSQLConnection; override;
    function Internal_GetTransaction : TSQLTransaction; override;
  public

  end;

var
  FExternalSQLEditor: TFExternalSQLEditor;

implementation

{$R *.lfm}

{ TFExternalSQLEditor }

function TFExternalSQLEditor.Internal_GetConnection: TSQLConnection;
begin
  Result := Self.sqlConector;
end;

function TFExternalSQLEditor.Internal_GetTransaction: TSQLTransaction;
begin
  Result := Self.sqlLiteInternalTransaction;
end;

end.

