unit udmservice;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, SQLite3Conn;

type

  { TDMService }

  TDMService = class(TDataModule)
    sqlLiteInternalDatabaseConnection: TSQLite3Connection;
    sqlLiteInternalTransaction: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private

  public

  end;

var
  DMService: TDMService;

implementation

uses JupiterApp, JupiterEnviroment;

{$R *.lfm}

{ TDMService }

procedure TDMService.DataModuleCreate(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrJupiterApp.InternalDatabase := sqlLiteInternalDatabaseConnection;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    if not vrEnviroment.Exists('/datasets/db_params.txt') then
      vrEnviroment.CreateFile('/datasets/db_params.txt', sqlLiteInternalDatabaseConnection.Params.Text)
    else
      sqlLiteInternalDatabaseConnection.Params.LoadFromFile(vrEnviroment.FullPath('/datasets/db_params.txt'));

    sqlLiteInternalDatabaseConnection.Connected    := False;
    sqlLiteInternalDatabaseConnection.DatabaseName := vrEnviroment.FullPath(vrJupiterApp.Params.VariableById('database.local').Value);

    if not vrEnviroment.Exists(sqlLiteInternalDatabaseConnection.DatabaseName) then
      sqlLiteInternalDatabaseConnection.CreateDB;

    sqlLiteInternalDatabaseConnection.Open;
  finally
    if sqlLiteInternalDatabaseConnection.Connected then
      vrJupiterApp.Prepare;
  end;
end;

procedure TDMService.DataModuleDestroy(Sender: TObject);
begin
  sqlLiteInternalDatabaseConnection.Connected := False;
end;

end.

