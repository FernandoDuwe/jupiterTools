unit udm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterApp, JupiterSystemMessage, SQLDB, odbcconn,
  IBConnection, oracleconnection;

type

  { TDMMain }

  TDMMain = class(TDataModule)
    sqlConnector: TSQLConnector;
    sqlTransaction: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private

  public

  end;

var
  DMMain: TDMMain;

implementation

uses jupiterdatabase;

{$R *.lfm}

{ TDMMain }

procedure TDMMain.DataModuleCreate(Sender: TObject);
var
  vrSystemMesssage : TJupiterSystemMessage;
begin
  if not TJupiterDatabaseModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Database')).HasConfig then
  begin
    vrSystemMesssage := vrJupiterApp.AddMessage('Configurações de acesso a banco de dados não definidas', Self.ClassName);
    vrSystemMesssage.Details.Add('Nenhuma configuração de driver foi setada para conexão com o banco de dados. O módulo de banco de dados ficará inativo');

    Exit;
  end;

  sqlConnector.Connected     := False;
  sqlConnector.ConnectorType := TJupiterDatabaseModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Database')).GetDriverType;
  sqlConnector.HostName      := TJupiterDatabaseModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Database')).GetHost;
  sqlConnector.DatabaseName  := TJupiterDatabaseModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Database')).GetDatabase;
  sqlConnector.UserName      := TJupiterDatabaseModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Database')).GetUserName;
  sqlConnector.Password      := TJupiterDatabaseModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Database')).GetPassword;

  try
    sqlConnector.Connected := True;
    sqlTransaction.Active := True;

    vrSystemMesssage := vrJupiterApp.AddMessage('Conexão bem sucedida', Self.ClassName);
  except
    vrSystemMesssage := vrJupiterApp.AddMessage('Erro ao conectar a base de dados', Self.ClassName);
    vrSystemMesssage.Details.Add(Exception(ExceptObject).Message);
  end;
end;

procedure TDMMain.DataModuleDestroy(Sender: TObject);
begin
  if sqlTransaction.Active then
    sqlTransaction.Active := False;

  if sqlConnector.Connected then
    sqlConnector.Connected := True;
end;

end.

