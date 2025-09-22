unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  StdCtrls, ValEdit, MSSQLConn, odbcconn, IBConnection, oracleconnection;

type

  { TFMain }

  TFMain = class(TForm)
    gbOutput: TGroupBox;
    mmLog: TMemo;
    miDisconnect: TMenuItem;
    miConnect: TMenuItem;
    mmOptions: TMainMenu;
    sbStatus: TStatusBar;
    SQLConnector1: TSQLConnector;
    vlValues: TValueListEditor;
    procedure FormShow(Sender: TObject);
    procedure miConnectClick(Sender: TObject);
    procedure miDisconnectClick(Sender: TObject);
  private
    procedure Internal_Update;
  public

  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}

{ TFMain }

procedure TFMain.miConnectClick(Sender: TObject);
begin
  mmLog.Lines.Clear;

  try
    mmLog.Lines.Add('Iniciando');

    try
      SQLConnector1.ConnectorType := vlValues.Values['Tipo de conexão'];
      SQLConnector1.HostName      := vlValues.Values['Servidor'];
      SQLConnector1.DatabaseName  := vlValues.Values['Base de Dados'];
      SQLConnector1.UserName      := vlValues.Values['Usuário'];
      SQLConnector1.Password      := vlValues.Values['Senha'];

      mmLog.Lines.Add('Configurado. Efetuando conexão');

      SQLConnector1.Connected := True;

      mmLog.Lines.Add('Conexão bem sucedida');
    except
      mmLog.Lines.Add('Erro: ' + Exception(ExceptObject).Message);
    end;
  finally
    Self.Internal_Update;
  end;
end;

procedure TFMain.FormShow(Sender: TObject);
var
  vrStr : TStrings;
begin
  vrStr := TStringList.Create;
  try
    GetConnectionList(vrStr);

    mmLog.Lines.Clear;
    mmLog.Lines.Add('Tipos de conexões:');
    mmLog.Lines.AddStrings(vrStr);
  finally
    vrStr.Free;
    Self.Internal_Update;
  end;
end;

procedure TFMain.miDisconnectClick(Sender: TObject);
begin
  try
    SQLConnector1.Connected := False;
  finally
    Self.Internal_Update;
  end;
end;

procedure TFMain.Internal_Update;
begin
  miConnect.Enabled    := not SQLConnector1.Connected;
  miDisconnect.Enabled := SQLConnector1.Connected;
end;

end.

