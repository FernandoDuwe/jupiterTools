unit jupiterElara;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterModule, JupiterApp, jupiterDatabaseWizard, JupiterConsts,
  SysUtils;

type

  { TJupiterEModule }

  TJupiterElaraModule = class(TJupiterModule)
  protected
    function Internal_GetModuleID : String; override;
    function Internal_GetModuleTitle : String; override;
    procedure Internal_Prepare; override;
  end;

implementation

{ TJupiterEModule }

function TJupiterElaraModule.Internal_GetModuleID: String;
begin
  Result := 'Elara';
end;

function TJupiterElaraModule.Internal_GetModuleTitle: String;
begin
  Result := 'Elara: Serviço de execução do Jupiter';
end;

procedure TJupiterElaraModule.Internal_Prepare;
var
  vrWizard : TJupiterDatabaseWizard;
begin
  inherited Internal_Prepare;

  vrWizard := vrJupiterApp.NewWizard;
  try
    if not vrWizard.TableExists('SERVICES') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE SERVICES ( ID INTEGER PRIMARY KEY, NAME VARCHAR (100), MACRO INT, MACRO_ENABLED INT, FOREIGN KEY (MACRO) REFERENCES MACROS (ID), FOREIGN KEY (MACRO_ENABLED) REFERENCES MACROS (ID))'));

    if not vrWizard.TableExists('SERVICE_LOG') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE SERVICE_LOG ( ID INTEGER PRIMARY KEY, SERVICE INT, SERVICE_START TIMESTAMP, SERVICE_STOP TIMESTAMP, LOG BLOB, FOREIGN KEY (SERVICE) REFERENCES SERVICES (ID))'));

    Self.Internal_CreateVariablIfDontExists('Services.Clock.Interval', 'Clock de intervalo de execução (em milisegundos)', '1000');

    if Self.Internal_CreateMacroIfDontExists('menu.tools.services.click', 'Clique do botão Serviços', CreateStringListToMacro('OpenGridFromTable(''SERVICES'');')) then
      Self.Internal_CreateRouteIfDontExists('Serviços', '/menu/tools/services/', vrWizard.GetLastID('MACROS'), ICON_CONFIG, 15000);
  finally
    FreeAndNil(vrWizard);
  end;
end;

end.

