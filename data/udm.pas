unit udm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterApp, JupiterSystemMessage, JupiterEnviroment, SQLDB,
  MSSQLConn, odbcconn, IBConnection, oracleconnection;

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
  vrEnviroment     : TJupiterEnviroment;
  vrStr            : TStrings;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;

    GetConnectionList(vrStr);

    vrSystemMesssage := vrJupiterApp.AddMessage('Drivers de banco de dados suportados', Self.ClassName);
    vrSystemMesssage.Details.AddStrings(vrStr);


    if not TJupiterDatabaseModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Database')).HasConfig then
    begin
      vrSystemMesssage := vrJupiterApp.AddMessage('Configurações de acesso a banco de dados não definidas', Self.ClassName);
      vrSystemMesssage.Details.Add('Nenhuma configuração de driver foi setada para conexão com o banco de dados. O módulo de banco de dados ficará inativo');

      Exit;
    end;

    vrEnviroment := TJupiterEnviroment.Create;
    try
      if vrEnviroment.Exists('/modules/database/data/params.txt') then
      begin
        sqlConnector.Params.Clear;
        sqlConnector.Params.LoadFromFile(vrEnviroment.FullPath('/modules/database/data/params.txt'));
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
    finally
      FreeAndNil(vrEnviroment);
    end;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
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

