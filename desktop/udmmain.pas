unit uDmMain;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterApp, JupiterEnviroment, SQLite3Conn, SQLDB;

type

  { TDMMain }

  TDMMain = class(TDataModule)
    sqlLiteInternalDatabaseConnection: TSQLite3Connection;
    sqlLiteInternalTransaction: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private

  public

  end;

var
  DMMain: TDMMain;

implementation

{$R *.lfm}

{ TDMMain }

procedure TDMMain.DataModuleCreate(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrJupiterApp.InternalDatabase := sqlLiteInternalDatabaseConnection;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    sqlLiteInternalDatabaseConnection.Connected    := False;
    sqlLiteInternalDatabaseConnection.DatabaseName := vrEnviroment.FullPath(vrJupiterApp.Params.VariableById('database.local').Value);

    if not vrEnviroment.Exists(sqlLiteInternalDatabaseConnection.DatabaseName) then
      sqlLiteInternalDatabaseConnection.CreateDB;

    sqlLiteInternalDatabaseConnection.Connected := True;
  finally
    if sqlLiteInternalDatabaseConnection.Connected then
      vrJupiterApp.Prepare;
  end;
end;

procedure TDMMain.DataModuleDestroy(Sender: TObject);
begin
  sqlLiteInternalDatabaseConnection.Connected := False;
end;

end.

