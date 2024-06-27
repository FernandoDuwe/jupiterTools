unit jupiterStandard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterModule, JupiterApp, jupiterDatabaseWizard,
  JupiterConsts;

type

  { TJupiterStandardModule }

  TJupiterStandardModule = class(TJupiterModule)
  protected
    function Internal_GetModuleID : String; override;
    function Internal_GetModuleTitle : String; override;
    procedure Internal_Prepare; override;
  end;

implementation

{ TJupiterStandardModule }

function TJupiterStandardModule.Internal_GetModuleID: String;
begin
  Result := 'Jupiter.Standard';
end;

function TJupiterStandardModule.Internal_GetModuleTitle: String;
begin
  Result := 'Standard: Pacote básico de execução';
end;

procedure TJupiterStandardModule.Internal_Prepare;
var
  vrWizard : TJupiterDatabaseWizard;
begin
  vrWizard := vrJupiterApp.NewWizard;
  try
    // Creating basic tables

    if not vrWizard.TableExists('MODULES') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE MODULES ( ID INTEGER PRIMARY KEY, NAME VARCHAR (100), MODULEID VARCHAR(100))'));

    if not vrWizard.TableExists('VARIABLES') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE VARIABLES ( ID INTEGER PRIMARY KEY, NAME VARCHAR (100), VALUE VARCHAR(100), MODULE INTEGER, FOREIGN KEY (MODULE) REFERENCES MODULES (ID))'));

    if not vrWizard.TableExists('ROUTES') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE ROUTES ( ID INTEGER PRIMARY KEY, TITLE VARCHAR(100), ROUTE VARCHAR (100), DESTINY BLOB, ICON SMALLINT, ZINDEX SMALLINT)'));

    // Creating basic routes
    Self.Internal_CreateRouteIfDontExists('Arquivo', '/menu/file/', EmptyStr, NULL_KEY, 100);
    Self.Internal_CreateRouteIfDontExists('Editar', '/menu/edit/', EmptyStr, NULL_KEY, 200);
    Self.Internal_CreateRouteIfDontExists('Ferramentas', '/menu/tools/', EmptyStr, NULL_KEY, 300);
    Self.Internal_CreateRouteIfDontExists('Sobre', '/menu/about/', EmptyStr, NULL_KEY, 10000);

    // Inside File Menu
    Self.Internal_CreateRouteIfDontExists('Novo', '/menu/file/new/', EmptyStr, ICON_NEW, 100);
    Self.Internal_CreateRouteIfDontExists('-', '/menu/file/separator1/', EmptyStr, NULL_KEY, 500);
    Self.Internal_CreateRouteIfDontExists('Configurações', '/menu/file/config/', EmptyStr, ICON_CONFIG, 1000);
    Self.Internal_CreateRouteIfDontExists('-', '/menu/file/separator2/', EmptyStr, NULL_KEY, 9000);
    Self.Internal_CreateRouteIfDontExists('Fechar', '/menu/file/exit/', EmptyStr, ICON_EXIT, 9000);
  finally
    FreeAndNil(vrWizard);
  end;

  inherited Internal_Prepare;
end;

end.

