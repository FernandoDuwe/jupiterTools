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
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE VARIABLES ( ID INTEGER PRIMARY KEY, NAME VARCHAR (100), DESCRIPTION VARCHAR(100), VALUE VARCHAR(100), MODULE INTEGER, FOREIGN KEY (MODULE) REFERENCES MODULES (ID))'));

    if not vrWizard.TableExists('MACROS') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE MACROS ( ID INTEGER PRIMARY KEY, NAME VARCHAR (100), MACROID VARCHAR(100), MACRO BLOB)'));

    if not vrWizard.TableExists('ROUTES') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE ROUTES ( ID INTEGER PRIMARY KEY, TITLE VARCHAR(100), ROUTE VARCHAR (100), DESTINY INT, ICON SMALLINT, ZINDEX SMALLINT, FOREIGN KEY (DESTINY) REFERENCES MACROS (ID))'));

    if not vrWizard.TableExists('ACTIONS') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE ACTIONS ( ID INTEGER PRIMARY KEY, NAME VARCHAR (100), TITLE VARCHAR (100), TABLENAME VARCHAR(100), ICON SMALLINT, ZINDEX SMALLINT, MACRO INTEGER, MACRO_ENABLE INTEGER, MACRO_VISIBLE INTEGER, FOREIGN KEY (MACRO) REFERENCES MACROS (ID), FOREIGN KEY (MACRO_ENABLE) REFERENCES MACROS (ID), FOREIGN KEY (MACRO_VISIBLE) REFERENCES MACROS (ID))'));

    if Self.Internal_CreateMacroIfDontExists('menu.newTab.click', 'Clique do botão Nova aba', CreateStringList('program macro;' + #13#10 + 'begin' + #13#10 + '  OpenForm(''/forms/newTask'');' + #13#10 + 'end.')) then
      Self.Internal_CreateRouteIfDontExists(EmptyStr, '/menu/newTab/', vrWizard.GetLastID('MACROS'), ICON_ADD, 100);

    Self.Internal_CreateMacroIfDontExists(TRIGGER_ONSTART, 'Evento: Ao iniciar a aplicação', CreateStringList('program macro;' + #13#10 + 'begin' + #13#10 + '  OpenForm(''/forms/newTask'');' + #13#10 + 'end.'));

    Self.Internal_CreateMacroIfDontExists(TRIGGER_ONPROMPT, 'Evento: Ao executar comando via prompt', CreateStringList('program macro;' + #13#10 + 'begin' + #13#10 + '  OpenForm(''/forms/newTask'');' + #13#10 + 'end.'));

    // Creating basic events
    Self.Internal_CreateMacroIfDontExists(EVENT_RECORD_ONENABLE, 'Evento: Enabled de registros', CreateStringListToMacro(' if ParamExists(SCRIPTID, ''ID'') then' + STRING_NEWLINE + '  SetParam(SCRIPTID, ''Result'', ''Y'')'));
    Self.Internal_CreateMacroIfDontExists(EVENT_RECORD_ONVISIBLE, 'Evento: Visible de registros', CreateStringListToMacro(' if ParamExists(SCRIPTID, ''ID'') then' + STRING_NEWLINE + '  SetParam(SCRIPTID, ''Result'', ''Y'')'));
    Self.Internal_CreateMacroIfDontExists(EVENT_TABLE_ONENABLE, 'Evento: Enabled de tabelas', CreateStringListToMacro(' if not ParamExists(SCRIPTID, ''ID'') then' + STRING_NEWLINE + '  SetParam(SCRIPTID, ''Result'', ''Y'')'));
    Self.Internal_CreateMacroIfDontExists(EVENT_TABLE_ONVISIBLE, 'Evento: Visible de tabelas', CreateStringListToMacro(' if not ParamExists(SCRIPTID, ''ID'') then' + STRING_NEWLINE + '  SetParam(SCRIPTID, ''Result'', ''Y'')'));


    // Creating basic routes
    Self.Internal_CreateRouteIfDontExists('Arquivo', '/menu/file/', NULL_KEY, NULL_KEY, 100);
    Self.Internal_CreateRouteIfDontExists('Exibir', '/menu/show/', NULL_KEY, NULL_KEY, 200);
    Self.Internal_CreateRouteIfDontExists('Ferramentas', '/menu/tools/', NULL_KEY, NULL_KEY, 300);
    Self.Internal_CreateRouteIfDontExists('Sobre', '/menu/about/', NULL_KEY, NULL_KEY, 10000);

    Self.Internal_CreateRouteIfDontExists('Cadastros', '/main/records/', NULL_KEY, ICON_RECORDS, 100);

    Self.Internal_CreateRouteIfDontExists('Ferramentas', '/main/tools/', NULL_KEY, ICON_TOOLS, 200);

    // Inside File Menu
    Self.Internal_CreateRouteIfDontExists('Novo', '/menu/file/new/', NULL_KEY, ICON_NEW, 100);
    Self.Internal_CreateRouteIfDontExists('-', '/menu/file/separator1/', NULL_KEY, NULL_KEY, 500);

    if Self.Internal_CreateMacroIfDontExists('menu.file.config.click', 'Clique do botão Configurações', CreateStringList('program macro;' + #13#10 + 'begin' + #13#10 + '  OpenForm(''/forms/config'');' + #13#10 + 'end.')) then
       Self.Internal_CreateRouteIfDontExists('Configurações', '/menu/file/config/', vrWizard.GetLastID('MACROS'), ICON_CONFIG, 1000);

    Self.Internal_CreateRouteIfDontExists('-', '/menu/file/separator2/', NULL_KEY, NULL_KEY, 9000);

    if Self.Internal_CreateMacroIfDontExists('jupiter.close', 'Fechar Aplicação', CreateStringList('program macro;' + #13#10 + 'begin' + #13#10 + '  CloseApp();' + #13#10 + 'end.')) then
      Self.Internal_CreateRouteIfDontExists('Fechar', '/menu/file/exit/', vrWizard.GetLastID('MACROS'), ICON_EXIT, 9000);

    if Self.Internal_CreateMacroIfDontExists('menu.tools.scriptEditor.click', 'Clique do botão Editor de Scripts JPAS', CreateStringListToMacro('OpenForm(''/forms/script'');')) then
      Self.Internal_CreateRouteIfDontExists('Editor de Scripts JPAS', '/menu/tools/scriptEditor/', vrWizard.GetLastID('MACROS'), ICON_TECHFILE, 100);
  finally
    FreeAndNil(vrWizard);
  end;

  inherited Internal_Prepare;
end;

end.

