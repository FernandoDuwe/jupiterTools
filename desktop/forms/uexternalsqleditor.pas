unit uExternalSQLEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, Forms, Controls, Graphics, Dialogs, uSQLEditor, MSSQLConn, odbcconn, IBConnection, oracleconnection;

type

  { TFExternalSQLEditor }

  TFExternalSQLEditor = class(TFSQLEditor)
    sqlConector: TSQLConnector;
    sqlLiteInternalTransaction: TSQLTransaction;
    procedure FormDestroy(Sender: TObject);
  private
    function Internal_GetConnection  : TSQLConnection; override;
    function Internal_GetTransaction : TSQLTransaction; override;

    function Internal_RenderFields : Boolean; override;

    function Internal_GetLastEditedFile : String; override;
  public

  end;

var
  FExternalSQLEditor: TFExternalSQLEditor;

implementation

{$R *.lfm}

{ TFExternalSQLEditor }

procedure TFExternalSQLEditor.FormDestroy(Sender: TObject);
begin
  inherited;

  sqlConector.Connected := False;
end;

function TFExternalSQLEditor.Internal_GetConnection: TSQLConnection;
begin
  Result := Self.sqlConector;
end;

function TFExternalSQLEditor.Internal_GetTransaction: TSQLTransaction;
begin
  Result := Self.sqlLiteInternalTransaction;
end;

function TFExternalSQLEditor.Internal_RenderFields: Boolean;
begin
  Result := False;
end;

function TFExternalSQLEditor.Internal_GetLastEditedFile: String;
begin
  Result := '/temp/sql externo.sql';
end;

end.

