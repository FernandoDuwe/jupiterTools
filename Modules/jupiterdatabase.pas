unit jupiterdatabase;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterModule, JupiterEnviroment, jupiterclicommand, udm, SysUtils;

type

  { TJupiterDatabase }

  { TJupiterDatabaseModule }

  TJupiterDatabaseModule = class(TJupiterModule)
  protected
    procedure Internal_Prepare; override;
    function Internal_GetModuleID : String; override;
    function Internal_GetModuleTitle : String; override;
  public
    function IsActive : Boolean;
    function HasConfig : Boolean;

    function GetDriverType : String;
    function GetHost : String;
    function GetDatabase : String;
    function GetUserName : String;
    function GetPassword : String;
    function GetTerminator : String;
  end;

implementation

{ TJupiterDatabase }

procedure TJupiterDatabaseModule.Internal_Prepare;
var
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Internal_Prepare;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.CreatePath('modules/database');
    vrEnviroment.CreatePath('modules/database/data');

    if not Self.Params.Exists(Self.DefineParamName('Config.Driver')) then
      Self.Params.AddConfig(Self.DefineParamName('Config.Driver'), EmptyStr, 'Driver utilizado');

    if not Self.Params.Exists(Self.DefineParamName('Config.Host')) then
      Self.Params.AddConfig(Self.DefineParamName('Config.Host'), EmptyStr, 'Host/Servidor');

    if not Self.Params.Exists(Self.DefineParamName('Config.Database')) then
      Self.Params.AddConfig(Self.DefineParamName('Config.Database'), EmptyStr, 'Base de dados/Caminho');

    if not Self.Params.Exists(Self.DefineParamName('Config.UserName')) then
      Self.Params.AddConfig(Self.DefineParamName('Config.UserName'), EmptyStr, 'Usuário');

    if not Self.Params.Exists(Self.DefineParamName('Config.Password')) then
      Self.Params.AddConfig(Self.DefineParamName('Config.Password'), EmptyStr, 'Senha');

    if not Self.Params.Exists(Self.DefineParamName('Config.Terminator')) then
      Self.Params.AddConfig(Self.DefineParamName('Config.Terminator'), ';', 'Caracter identificador de fim de instrução');
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

function TJupiterDatabaseModule.Internal_GetModuleID: String;
begin
  Result := 'Jupiter.Database';
end;

function TJupiterDatabaseModule.Internal_GetModuleTitle: String;
begin
  Result := 'Database';
end;

function TJupiterDatabaseModule.IsActive: Boolean;
begin
  Result := False;

  if not Self.HasConfig then
    Exit;

  if not DMMain.sqlConnector.Connected then
    Exit;

  Result := True;
end;

function TJupiterDatabaseModule.HasConfig: Boolean;
begin
  Result := ((Self.Params.Exists(Self.DefineParamName('Config.Driver'))) and (Self.Params.VariableById(Self.DefineParamName('Config.Driver')).Value <> ''));
end;

function TJupiterDatabaseModule.GetDriverType: String;
begin
  Result := Self.Params.VariableById(Self.DefineParamName('Config.Driver')).Value;
end;

function TJupiterDatabaseModule.GetHost: String;
begin
  Result := Self.Params.VariableById(Self.DefineParamName('Config.Host')).Value;
end;

function TJupiterDatabaseModule.GetDatabase: String;
begin
  Result := Self.Params.VariableById(Self.DefineParamName('Config.Database')).Value;
end;

function TJupiterDatabaseModule.GetUserName: String;
begin
  Result := Self.Params.VariableById(Self.DefineParamName('Config.UserName')).Value;
end;

function TJupiterDatabaseModule.GetPassword: String;
begin
  Result := Self.Params.VariableById(Self.DefineParamName('Config.Password')).Value;
end;

function TJupiterDatabaseModule.GetTerminator: String;
begin
  Result := Self.Params.VariableById(Self.DefineParamName('Config.Terminator')).Value;
end;

end.

