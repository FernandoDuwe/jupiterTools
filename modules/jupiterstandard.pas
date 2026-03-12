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
  vrStr : TStrings;
begin
  inherited Internal_Prepare;

  vrWizard := vrJupiterApp.NewWizard;
  vrStr := TStringList.Create;
  try
    // Creating basic tables
    if not vrWizard.TableExists('MODULES') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE MODULES ( ID INTEGER PRIMARY KEY, NAME VARCHAR (100), MODULEID VARCHAR(100))'));

    if not vrWizard.TableExists('VARIABLES') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE VARIABLES ( ID INTEGER PRIMARY KEY, NAME VARCHAR (100), DESCRIPTION VARCHAR(100), VALUE VARCHAR(100), MODULE INTEGER, FOREIGN KEY (MODULE) REFERENCES MODULES (ID))'));

    if not vrWizard.TableExists('MACROS') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE MACROS ( ID INTEGER PRIMARY KEY, NAME VARCHAR (100), MACROID VARCHAR(100), MACRO BLOB)'));

    if not vrWizard.TableExists('MACRO_LIBRARIES') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE MACRO_LIBRARIES ( ID INTEGER PRIMARY KEY, NAME VARCHAR (100), LIBRARYID VARCHAR(100), MACRO BLOB)'));

    if not vrWizard.TableExists('ROUTES') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE ROUTES ( ID INTEGER PRIMARY KEY, TITLE VARCHAR(100), ROUTE VARCHAR (100), DESTINY INT, ICON SMALLINT, ZINDEX SMALLINT, SHORTCUT VARCHAR(30), FOREIGN KEY (DESTINY) REFERENCES MACROS (ID))'));

    if not vrWizard.FieldExists('ROUTES', 'PARAMS') then
        vrWizard.ExecuteScript(CreateStringList(' ALTER TABLE ROUTES ADD PARAMS VARCHAR(100) '));

    if not vrWizard.TableExists('SEARCH_FIELD') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE SEARCH_FIELD ( ID INTEGER PRIMARY KEY, TABLENAME VARCHAR(100), FIELDNAME VARCHAR(100))'));

    if not vrWizard.TableExists('SHORTCUTS') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE SHORTCUTS ( ID INTEGER PRIMARY KEY, DESCRIPTION VARCHAR(100), SHORTCUT VARCHAR(100), DESTINY INT, FOREIGN KEY (DESTINY) REFERENCES MACROS (ID))'));

    if ((vrWizard.TableExists('SHORTCUTS')) and (not vrWizard.FieldExists('SHORTCUTS', 'ZINDEX'))) then
    begin
      vrWizard.ExecuteScript(CreateStringList(' ALTER TABLE SHORTCUTS ADD ZINDEX SMALLINT; '));

      vrWizard.ExecuteScript(CreateStringList(' UPDATE SHORTCUTS SET ZINDEX = ID * 1000 WHERE ZINDEX IS NULL '));
    end;

    if not vrWizard.TableExists('ACTIONS') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE ACTIONS ( ID INTEGER PRIMARY KEY, NAME VARCHAR (100), TITLE VARCHAR (100), TABLENAME VARCHAR(100), ICON SMALLINT, ZINDEX SMALLINT, MACRO INTEGER, MACRO_ENABLE INTEGER, MACRO_VISIBLE INTEGER, FOREIGN KEY (MACRO) REFERENCES MACROS (ID), FOREIGN KEY (MACRO_ENABLE) REFERENCES MACROS (ID), FOREIGN KEY (MACRO_VISIBLE) REFERENCES MACROS (ID))'));

    if not vrWizard.TableExists('RECORDPIN') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE RECORDPIN ( ID INTEGER PRIMARY KEY, TABLENAME VARCHAR (100), RECORDKEY INTEGER)'));

    // Database
    if not vrWizard.TableExists('DATABASE_DICTIONARY') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE DATABASE_DICTIONARY ( ID INTEGER PRIMARY KEY, TABLENAME VARCHAR(100), FIELDNAME VARCHAR(100), TITLE VARCHAR(100), HINT VARCHAR(200))'));

    if vrWizard.TableExists('DATABASE_DICTIONARY') then
      if not vrWizard.FieldExists('DATABASE_DICTIONARY', 'ACTION_EXECUTE') then
        vrWizard.ExecuteScript(CreateStringList(' ALTER TABLE DATABASE_DICTIONARY ADD ACTION_EXECUTE BOOLEAN '));

    if vrWizard.TableExists('DATABASE_DICTIONARY') then
    begin
      if not vrWizard.FieldExists('DATABASE_DICTIONARY', 'ACTION_COPY') then
        vrWizard.ExecuteScript(CreateStringList(' ALTER TABLE DATABASE_DICTIONARY ADD ACTION_COPY BOOLEAN '));

      if not vrWizard.FieldExists('DATABASE_DICTIONARY', 'HEIGHT') then
        vrWizard.ExecuteScript(CreateStringList(' ALTER TABLE DATABASE_DICTIONARY ADD HEIGHT INT '));
    end;

    if not vrWizard.TableExists('DATABASE_TABLETITLE') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE DATABASE_TABLETITLE ( ID INTEGER PRIMARY KEY, TABLENAME VARCHAR(100), EXPRESSION VARCHAR(200))'))
    else
      if not vrWizard.FieldExists('DATABASE_TABLETITLE', 'SQL_EXPRESSION') then
        vrWizard.ExecuteScript(CreateStringList(' ALTER TABLE DATABASE_TABLETITLE ADD SQL_EXPRESSION VARCHAR(200) '));

    // Tarefas
    if not vrWizard.TableExists('PERIODIC_TASK') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE PERIODIC_TASK ( ID INTEGER PRIMARY KEY, MACRO INTEGER, PARAMS VARCHAR(100), MINUTE INT, FOREIGN KEY (MACRO) REFERENCES MACROS (ID))'));

    if Self.Internal_CreateMacroIfDontExists('menu.newTab.click', 'Clique do botão Nova aba', CreateStringList('program macro;' + #13#10 + 'begin' + #13#10 + '  OpenForm(''/forms/newTask'');' + #13#10 + 'end.')) then
      Self.Internal_CreateRouteIfDontExists(EmptyStr, '/menu/newTab/', vrWizard.GetLastID('MACROS'), ICON_ADD, 100);

    if Self.Internal_CreateMacroIfDontExists('menu.contextMenu.click', 'Clique do botão de contexto', CreateStringListToMacro(' OpenForm(''/forms/context''); ')) then
    begin
      Self.Internal_CreateRouteIfDontExists(EmptyStr, '/menu/contextMenu/', vrWizard.GetLastID('MACROS'), ICON_MENU, 200, 'Ctrl+Enter');

      Self.Internal_CreateShortcutIfDontExists('Abrir o menu de contexto', 'Ctrl+Enter', vrWizard.GetLastID('MACROS'));
    end;

    if Self.Internal_CreateMacroIfDontExists('menu.workMenu.click', 'Clique do botão de menu de trabalho', CreateStringListToMacro(' OpenFormWithParams(''/forms/explorer/menus'', ''/workdir/''); ')) then
    begin
      Self.Internal_CreateRouteIfDontExists(EmptyStr, '/menu/workMenu/', vrWizard.GetLastID('MACROS'), ICON_FAVORITE, 200, 'Ctrl+Enter');

      Self.Internal_CreateShortcutIfDontExists('Abrir o menu de trabalho', 'Ctrl+W', vrWizard.GetLastID('MACROS'));
    end;

    if Self.Internal_CreateMacroIfDontExists('menu.quickJump.click', 'Clique do botão de menu QuickJump', CreateStringListToMacro(' OpenForm(''/forms/quick_jump''); ')) then
      Self.Internal_CreateShortcutIfDontExists('Abrir o menu QuickJump', 'Ctrl+Q', vrWizard.GetLastID('MACROS'));

    vrStr.Clear;
    vrStr.Add('program macro;');
    vrStr.Add('const');
    vrStr.Add('  SCRIPTID = ''@FLAG_SCRIPTID'';');
    vrStr.Add('var');
    vrStr.Add('  vrOutput : String;');
    vrStr.Add('begin');
    vrStr.Add('  if FileOrFolderExists(GetParam(SCRIPTID, ''PARAMS'')) then');
    vrStr.Add('  begin');
    vrStr.Add('    if SameExtension(GetParam(SCRIPTID, ''PARAMS''), ''.ckl'') then ');
    vrStr.Add('    begin ');
    vrStr.Add('      OpenCheckListExplorerForm(GetParam(SCRIPTID, ''PARAMS'')); ');
    vrStr.Add('      Exit; ');
    vrStr.Add('    end;');
    vrStr.Add('');
    vrStr.Add('    if SameExtension(GetParam(SCRIPTID, ''PARAMS''), ''.txt'') then ');
    vrStr.Add('    begin ');
    vrStr.Add('      OpenTextEditorForm(GetParam(SCRIPTID, ''PARAMS'')); ');
    vrStr.Add('      Exit; ');
    vrStr.Add('    end;');
    vrStr.Add('');
    vrStr.Add('    if FileExists(GetParam(SCRIPTID, ''PARAMS'')) then');
    vrStr.Add('      OpenDocument(GetParam(SCRIPTID, ''PARAMS''))');
    vrStr.Add('    else');
    vrStr.Add('      OpenFolder(GetParam(SCRIPTID, ''PARAMS''))');
    vrStr.Add('  end');
    vrStr.Add('  else');
    vrStr.Add('    CreateProcess(GetParam(SCRIPTID, ''PARAMS''), '''', vrOutput, False, True);');
    vrStr.Add('end.');

    Self.Internal_CreateMacroIfDontExists(TRIGGER_ONEXECUTE, 'Evento: Ao executar comando externo', vrStr);

    Self.Internal_CreateMacroIfDontExists(TRIGGER_ONSTART, 'Evento: Ao iniciar a aplicação', CreateStringList('program macro;' + #13#10 + 'begin' + #13#10 + '  OpenForm(''/forms/newTask'');' + #13#10 + 'end.'));

    vrStr.Clear;
    vrStr.Add('program macro;');
    vrStr.Add('const');
    vrStr.Add('  SCRIPTID = ''@FLAG_SCRIPTID'';');
    vrStr.Add('');
    vrStr.Add('  // Include libraries');
    vrStr.Add('');
    vrStr.Add('var');
    vrStr.Add('  vrStr : String;');
    vrStr.Add('  vrVez : Integer;');
    vrStr.Add('begin');
    vrStr.Add('  vrStr := ''Parâmetros ('' + IntToStr(ParamCount(SCRIPTID)) + ''): '' + #13#10 + #13#10;');
    vrStr.Add('');
    vrStr.Add('  for vrVez := 0 to ParamCount(SCRIPTID) - 1 do');
    vrStr.Add('  begin');
    vrStr.Add('    vrStr := vrStr + GetParamNameByIndex(SCRIPTID, vrVez) + #13#10;');
    vrStr.Add('    vrStr := vrStr + GetParam(SCRIPTID, GetParamNameByIndex(SCRIPTID, vrVez)) + #13#10;');
    vrStr.Add('');
    vrStr.Add('    vrStr := vrStr + #13#10;');
    vrStr.Add('  end;');
    vrStr.Add('');
    vrStr.Add('  ShowMessage(vrStr);');
    vrStr.Add('end.');

    Self.Internal_CreateMacroIfDontExists(TRIGGER_ONSHOWPARAMS, 'Evento: Ao exibir os parâmetros dos formulários', vrStr);

    Self.Internal_CreateMacroIfDontExists(TRIGGER_ONLOADDYNAMICDATA, 'Evento: Ao carregar dados dinâmicos', CreateStringListToMacro(''));

    Self.Internal_CreateMacroIfDontExists(TRIGGER_ONCHECKLISTCHANGE, 'Evento: Ao alterar uma linha de uma checklist', CreateStringListToMacro(''));

    Self.Internal_CreateMacroIfDontExists(TRIGGER_ONPROMPT, 'Evento: Ao executar comando via prompt', CreateStringList('program macro;' + #13#10 + 'begin' + #13#10 + '  OpenForm(''/forms/newTask'');' + #13#10 + 'end.'));

    // Creating basic events
    Self.Internal_CreateMacroIfDontExists(EVENT_RECORD_ONENABLE, 'Evento: Enabled de registros', CreateStringListToMacro(' if ParamExists(SCRIPTID, ''ID'') then' + STRING_NEWLINE + '  SetParam(SCRIPTID, ''Result'', ''Y'')'));
    Self.Internal_CreateMacroIfDontExists(EVENT_RECORD_ONVISIBLE, 'Evento: Visible de registros', CreateStringListToMacro(' if ParamExists(SCRIPTID, ''ID'') then' + STRING_NEWLINE + '  SetParam(SCRIPTID, ''Result'', ''Y'')'));
    Self.Internal_CreateMacroIfDontExists(EVENT_TABLE_ONENABLE, 'Evento: Enabled de tabelas', CreateStringListToMacro(' if not ParamExists(SCRIPTID, ''ID'') then' + STRING_NEWLINE + '  SetParam(SCRIPTID, ''Result'', ''Y'')'));
    Self.Internal_CreateMacroIfDontExists(EVENT_TABLE_ONVISIBLE, 'Evento: Visible de tabelas', CreateStringListToMacro(' if not ParamExists(SCRIPTID, ''ID'') then' + STRING_NEWLINE + '  SetParam(SCRIPTID, ''Result'', ''Y'')'));


    // Creating basic routes
    Self.Internal_CreateRouteIfDontExists('Arquivo', '/menu/file/', NULL_KEY, NULL_KEY, 1000);
    Self.Internal_CreateRouteIfDontExists('Exibir', '/menu/show/', NULL_KEY, NULL_KEY, 2000);
    Self.Internal_CreateRouteIfDontExists('Ferramentas', '/menu/tools/', NULL_KEY, NULL_KEY, 3000);

    if Self.Internal_CreateMacroIfDontExists('menu.about.click', 'Clique do botão Sobre', CreateStringListToMacro('ShowMessage(''Jupiter'' + #13#10 + #13#10 + ''Versão: '' + GetVersion);')) then
      Self.Internal_CreateRouteIfDontExists('Sobre', '/menu/about/', vrWizard.GetLastID('MACROS'), NULL_KEY, 100000);

    Self.Internal_CreateRouteIfDontExists('Cadastros', '/main/records/', NULL_KEY, ICON_RECORDS, 100);

    Self.Internal_CreateRouteIfDontExists('Ferramentas', '/main/tools/', NULL_KEY, ICON_TOOLS, 200);

    // Inside File Menu
    Self.Internal_CreateRouteIfDontExists('Novo', '/menu/file/new/', NULL_KEY, ICON_NEW, 100);
    Self.Internal_CreateRouteIfDontExists('Abrir', '/menu/file/open/', NULL_KEY, ICON_OPEN, 200);

    if Self.Internal_CreateMacroIfDontExists('menu.file.explorer', 'Clique do botão Explorar pasta', CreateStringListToMacro('   OpenFileExplorerForm(GetApplicationPath);')) then
      Self.Internal_CreateRouteIfDontExists('Explorar pasta', '/menu/file/explorer/', vrWizard.GetLastID('MACROS'), ICON_OPEN, 300);

    Self.Internal_CreateRouteIfDontExists('-', '/menu/file/separator1/', NULL_KEY, NULL_KEY, 500);

    if Self.Internal_CreateMacroIfDontExists('menu.file.config.click', 'Clique do botão Configurações', CreateStringList('program macro;' + #13#10 + 'begin' + #13#10 + '  OpenForm(''/forms/config'');' + #13#10 + 'end.')) then
       Self.Internal_CreateRouteIfDontExists('Configurações', '/menu/file/config/', vrWizard.GetLastID('MACROS'), ICON_CONFIG, 1000);

    Self.Internal_CreateRouteIfDontExists('-', '/menu/file/separator2/', NULL_KEY, NULL_KEY, 9000);

    if Self.Internal_CreateMacroIfDontExists('jupiter.close', 'Fechar Aplicação', CreateStringList('program macro;' + #13#10 + 'begin' + #13#10 + '  CloseApp();' + #13#10 + 'end.')) then
      Self.Internal_CreateRouteIfDontExists('Fechar', '/menu/file/exit/', vrWizard.GetLastID('MACROS'), ICON_EXIT, 9000);

    if Self.Internal_CreateMacroIfDontExists('menu.tools.scriptEditor.click', 'Clique do botão Editor de Scripts JPAS', CreateStringListToMacro('OpenForm(''/forms/script'');')) then
      Self.Internal_CreateRouteIfDontExists('Editor de Scripts JPAS', '/menu/tools/scriptEditor/', vrWizard.GetLastID('MACROS'), ICON_TECHFILE, 200, 'Shift+F9');

    if Self.Internal_CreateMacroIfDontExists('menu.tools.scriptSQL.click', 'Clique do botão Editor de Scripts JPAS', CreateStringListToMacro('OpenForm(''/forms/sqlEditor'');')) then
      Self.Internal_CreateRouteIfDontExists('Editor de Scripts SQL', '/menu/tools/scriptSQL/', vrWizard.GetLastID('MACROS'), ICON_PLAY, 300, 'Shift+F8');

    if Self.Internal_CreateMacroIfDontExists('menu.tools.systemMonitor.click', 'Clique do botão Monitor de aplicação', CreateStringListToMacro('OpenForm(''/forms/system'');')) then
      Self.Internal_CreateRouteIfDontExists('Monitor de aplicação', '/menu/tools/systemMonitor/', vrWizard.GetLastID('MACROS'), ICON_TOOLS, 100);

    if Self.Internal_CreateMacroIfDontExists('menu.tools.periodicTasks.click', 'Clique do botão Tarefas periódicas', CreateStringListToMacro('OpenGridFromTable(''PERIODIC_TASK'');')) then
      Self.Internal_CreateRouteIfDontExists('Tarefas periódicas', '/menu/tools/periodicTasks/', vrWizard.GetLastID('MACROS'), ICON_ENDTIME, 10000);

    Self.Internal_CreateMacroIfDontExists('MACROS.AbrirScript.OnClick', 'Abrir script no editor', CreateStringListToMacro('   OpenFormWithParams(''/forms/script'', GetParam(SCRIPTID, ''ID''));'));

    Self.Internal_CreateActionIfDontExists('MACROS.AbrirScript', 'Abrir script no editor', 'MACROS', ICON_EDIT, 100, Self.Internal_GetMacroById('MACROS.AbrirScript.OnClick'), Self.Internal_GetMacroById(EVENT_RECORD_ONENABLE), Self.Internal_GetMacroById(EVENT_RECORD_ONVISIBLE));

    Self.Internal_CreateVariablIfDontExists(TRIGGER_ONUPDATE, 'Nome da macro a ser executada a cada atualização de tela na página principal', '');

    Self.Internal_CreateVariablIfDontExists(PATH_WORKDIR, 'Diretório de tabalho padrão', ExtractFileDir('/'));

    Self.Internal_CreateVariablIfDontExists('TableGrid.Search.BlobFields', 'Efetuar pesquisa em campos BLOB', 'N');
  finally
    FreeAndNil(vrWizard);
    FreeAndNil(vrStr);
  end;
end;

end.

