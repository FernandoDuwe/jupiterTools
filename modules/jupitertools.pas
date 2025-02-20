unit jupiterTools;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterModule, JupiterApp, jupiterDatabaseWizard,
  JupiterConsts;

type

  { TJupiterTools }

  TJupiterTools = class(TJupiterModule)
  protected
    function Internal_GetModuleID : String; override;
    function Internal_GetModuleTitle : String; override;
    procedure Internal_Prepare; override;

    function Internal_CreateNewTaskMacro : TStrings;
    function Internal_CreateImportTaskMacro : TStrings;
  end;

implementation

{ TJupiterTools }

function TJupiterTools.Internal_GetModuleID: String;
begin
  Result := 'Jupiter.Tools';
end;

function TJupiterTools.Internal_GetModuleTitle: String;
begin
  Result := 'Tools: Pacote de utilitários';
end;

procedure TJupiterTools.Internal_Prepare;
var
  vrWizard : TJupiterDatabaseWizard;
begin
  inherited Internal_Prepare;

  vrWizard := vrJupiterApp.NewWizard;
  try
    if not vrWizard.TableExists('TAREFAS') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE TAREFAS ( ID INTEGER PRIMARY KEY, CLIENTE VARCHAR(200), NUMERO VARCHAR(200))'));

    if not vrWizard.TableExists('ANOTACOES') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE ANOTACOES ( ID INTEGER PRIMARY KEY, TITULO VARCHAR(200), DESCRICAO BLOB)'));

    if not vrWizard.TableExists('LAYOUT') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE LAYOUT ( ID INTEGER PRIMARY KEY, TITULO VARCHAR(200))'));

    if not vrWizard.TableExists('LAYOUTCAMPO') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE LAYOUTCAMPO ( ID INTEGER PRIMARY KEY, CAMPO VARCHAR(50), TIPO VARCHAR(10), TAMANHO INT, LAYOUT INT NOT NULL, FOREIGN KEY (LAYOUT) REFERENCES LAYOUT (ID)) '));

    if Self.Internal_CreateMacroIfDontExists('main.tasks.click', 'Clique do item de menu Tarefas', CreateStringListToMacro('OpenGridFromTable(''TAREFAS'');')) then
      Self.Internal_CreateRouteIfDontExists('Tarefas', '/main/tasks/', vrWizard.GetLastID('MACROS'), ICON_TASKS, 1000);

    if Self.Internal_CreateMacroIfDontExists('main.tools.notes.click', 'Clique do item de menu Anotações', CreateStringListToMacro('OpenGridFromTable(''ANOTACOES'');')) then
      Self.Internal_CreateRouteIfDontExists('Anotações', '/main/tools/notes/', vrWizard.GetLastID('MACROS'), ICON_IMPORTANT_MESSAGE, 50);

    Self.Internal_CreateVariablIfDontExists('Tools.Tasks.Path', 'Diretório de Tarefas', EmptyStr);
    Self.Internal_CreateVariablIfDontExists('Tools.Tasks.Current.Path', 'Diretório da Tarefa Atual', EmptyStr);
    Self.Internal_CreateVariablIfDontExists('Tools.Tasks.Current.ID', 'ID da Tarefa Atual', EmptyStr);

    if Self.Internal_CreateMacroIfDontExists('menu.file.new.task.click', 'Clique do item de nova tarefa', Self.Internal_CreateNewTaskMacro) then
    begin
      Self.Internal_CreateRouteIfDontExists('Tarefa', '/menu/file/new/task/', vrWizard.GetLastID('MACROS'), ICON_ADD, 100);
      Self.Internal_CreateRouteIfDontExists('Criar uma nova tarefa', '/context/new_task/', vrWizard.GetLastID('MACROS'), ICON_ADD, 100);
    end;

    if ((Self.Params.Exists('Tools.Tasks.Current.Path')) and (Self.Params.VariableById('Tools.Tasks.Current.Path').Value <> EmptyStr)) then
    begin
      if Self.Internal_CreateMacroIfDontExists('main.tasks.current.click', 'Clique do item de menu Tarefa', CreateStringListToMacro('')) then
        Self.Internal_CreateRouteIfDontExists('Tarefa', '/main/tasks/current/', vrWizard.GetLastID('MACROS'), ICON_NEW, 1000);

      if Self.Internal_CreateMacroIfDontExists('main.tasks.current.files.click', 'Clique do item de menu Tarefa: Arquivos', CreateStringListToMacro('OpenFileExplorerForm(GetGlobalParam(''Tools.Tasks.Current.Path''));')) then
        Self.Internal_CreateRouteIfDontExists('Arquivos', '/main/tasks/current/files/', vrWizard.GetLastID('MACROS'), ICON_OPEN, 1000);

    //  if Self.Internal_CreateMacroIfDontExists('main.tasks.current.checklists.click', 'Clique do item de menu Tarefa: Checklists', CreateStringListToMacro('')) then
    //    Self.Internal_CreateRouteIfDontExists('Checklists', '/main/tasks/current/checklists/', vrWizard.GetLastID('MACROS'), ICON_CHECK, 2000);

      if Self.Internal_CreateMacroIfDontExists('main.tasks.current.times.click', 'Clique do item de menu Tarefa: Tempos', CreateStringListToMacro('  if GlobalParamExists(''Tools.Tasks.Current.ID'') then ' + #13#10 +
         '    OpenGridFromTableWithWhere(''TEMPOS'', '' TAREFA = '' + GetGlobalParam(''Tools.Tasks.Current.ID'') + '' AND COALESCE(MARCADO, FALSE) = TRUE '', '''');')) then
        Self.Internal_CreateRouteIfDontExists('Tempos', '/main/tasks/current/times/', vrWizard.GetLastID('MACROS'), ICON_TIMEFILE, 3000);
    end;

    Self.Internal_CreateMacroIfDontExists('utils.tasks.import', 'Utils: Importar tarefas', Self.Internal_CreateImportTaskMacro);

    Self.Internal_CreateMacroIfDontExists('TAREFAS.MarcarComoAtualScript.OnClick', 'Marcar tarefa como atual', CreateStringListToMacro('   SetGlobalParam(''Tools.Tasks.Current.ID'', GetParam(SCRIPTID, ''ID''));' + #13#10 +
                                                                                                                                       #13#10 +
                                                                                                                                       '   SetGlobalParam(''Tools.Tasks.Current.Path'', GetGlobalParam(''Tools.Tasks.Path'') + ''/'' + GetParam(SCRIPTID, ''CLIENTE'') + ''/'' + GetParam(SCRIPTID, ''NUMERO'') + ''/'');' + #13#10 +
                                                                                                                                       #13#10 +
                                                                                                                                       '   SetAppMessage(''Tarefa definida como atual: '' + GetParam(SCRIPTID, ''NUMERO''));'
                                                                                                                                       ));

   Self.Internal_CreateMacroIfDontExists('LAYOUT.CamposScript.OnClick', 'Clique do botão Campos', CreateStringListToMacro('  OpenGridFromTableWithWhere(''LAYOUTCAMPO'', '' LAYOUT = '' + GetParam(SCRIPTID, "ID"), ''''); '));

    Self.Internal_CreateActionIfDontExists('TAREFAS.MarcarComoAtualScript',
                                           'Marcar como atual', 'TAREFAS', ICON_EDIT, 100,
                                           Self.Internal_GetMacroById('TAREFAS.MarcarComoAtualScript.OnClick'), Self.Internal_GetMacroById(EVENT_RECORD_ONENABLE), Self.Internal_GetMacroById(EVENT_RECORD_ONVISIBLE));

    Self.Internal_CreateActionIfDontExists('LAYOUT.CamposScript',
                                           'Campos', 'LAYOUT', ICON_SHEETFILE, 100,
                                           Self.Internal_GetMacroById('LAYOUT.CamposScript.OnClick'), Self.Internal_GetMacroById(EVENT_RECORD_ONENABLE), Self.Internal_GetMacroById(EVENT_RECORD_ONVISIBLE));

    if Self.Internal_CreateMacroIfDontExists('main.records.layouts.click', 'Clique do botão Layouts', CreateStringListToMacro('  OpenGridFromTable(''LAYOUT''); ')) then
      Self.Internal_CreateRouteIfDontExists('Layouts', '/main/records/layouts/', vrWizard.GetLastID('MACROS'), ICON_SHEETFILE, 100);


  finally
    FreeAndNil(vrWizard);
  end;
end;

function TJupiterTools.Internal_CreateNewTaskMacro: TStrings;
begin
  Result := TStringList.Create;

  Result.Clear;
  Result.Add('program macro;');
  Result.Add('const');
  Result.Add('  SCRIPTID = ''' + JPAS_FLAG_SCRIPTID + ''';');
  Result.Add(EmptyStr);
  Result.Add('  // Include libraries');
  Result.Add(EmptyStr);
  Result.Add('var');
  Result.Add('  formContext : String;');
  Result.Add('  clientDataProvider : String;');
  Result.Add('begin');
  Result.Add('  formContext := OpenForm(''/forms/custom/codable'');');
  Result.Add('  clientDataProvider := DataProviderNewPath(GetGlobalParam(''Tools.Tasks.Path''), False);');
  Result.Add('  try');
  Result.Add('    Form_AddLabel(formContext, ''Título'');');
  Result.Add('    Form_AddEdit(formContext, ''TITLE'', '''');');
  Result.Add(EmptyStr);
  Result.Add('    Form_AddLabel(formContext, ''Cliente'');');
  Result.Add('    Form_AddCombo(formContext, ''CLIENT'', clientDataProvider, ''Folder'');');
  Result.Add(EmptyStr);
  Result.Add('    Form_AddLabel(formContext, ''Nº Tarefa'');');
  Result.Add('    Form_AddEdit(formContext, ''NUMBER'', '''');');
  Result.Add(EmptyStr);
  Result.Add('    Form_AddLabel(formContext, ''Opções'');');
  Result.Add('    Form_AddCheckBox(formContext, ''CHECK_CURRENTTASK'', ''Marcar como tarefa atual'', True);');
  Result.Add('    Form_AddCheckBox(formContext, ''CHECK_MARKSTARTTIME'', ''Marcar tempo inicial'', True);');
  Result.Add('    Form_AddAction(formContext, ''Criar tarefa'', ''Clique aqui para criar a tarefa'', 3, ''menu.file.new.task.execute'');');
  Result.Add('  finally');
  Result.Add('    Form_SetCaption(formContext, ''Criar uma nova tarefa'');');
  Result.Add('    Form_SetHint(formContext, ''Neste tela você criará novas tarefas'');');
  Result.Add('    UpdateForms();');
  Result.Add('  end;');
  Result.Add('end.');
end;

function TJupiterTools.Internal_CreateImportTaskMacro: TStrings;
begin
  Result := TStringList.Create;

  Result.Clear;
  Result.Add('program macro;');
  Result.Add('const');
  Result.Add('  SCRIPTID = ''@FLAG_SCRIPTID''; ');
  Result.Add('');
  Result.Add('  // Include libraries');
  Result.Add('');
  Result.Add('var');
  Result.Add('  vrPathProvider : String;');
  Result.Add('  vrTaskProvider : String;');
  Result.Add('  vrVez : Integer;');
  Result.Add('  vrVez2 : Integer;');
  Result.Add('  vrClient : String;');
  Result.Add('  vrTask : String;');
  Result.Add('begin');
  Result.Add('  DBStartTransaction;');
  Result.Add('');
  Result.Add('  vrPathProvider := DataProviderNewPath(GetGlobalParam(''Tools.Tasks.Path''), False);');
  Result.Add('  try');
  Result.Add('    for vrVez := 0 to DataProviderGetCount(vrPathProvider) - 1 do');
  Result.Add('    begin');
  Result.Add('      vrClient := DataProviderGetField(vrPathProvider, ''Folder'', vrVez);');
  Result.Add('');
  Result.Add('      vrTaskProvider := DataProviderNewPath(DataProviderGetField(vrPathProvider, ''Path'', vrVez), False);');
  Result.Add('      try');
  Result.Add('        for vrVez2 := 0 to DataProviderGetCount(vrTaskProvider) - 1 do');
  Result.Add('        begin');
  Result.Add('          vrTask := DataProviderGetField(vrTaskProvider, ''Folder'', vrVez2);');
  Result.Add('');
  Result.Add('          if not DBExists(''TAREFAS'', '' CLIENTE = "'' + vrClient + ''" AND NUMERO = "'' + vrTask + ''" '') then');
  Result.Add('            DBRunScriptWithoutTransaction('' INSERT INTO TAREFAS (CLIENTE, NUMERO) VALUES ("'' + vrClient + ''", "'' + vrTask + ''") '');');
  Result.Add('        end;');
  Result.Add('      finally');
  Result.Add('        DataProviderDestroy(vrTaskProvider);');
  Result.Add('      end;');
  Result.Add('    end;');
  Result.Add('');
  Result.Add('    DBCommitTransaction;');
  Result.Add('  finally');
  Result.Add('    DataProviderDestroy(vrPathProvider);');
  Result.Add('  end;');
  Result.Add('end.');
  Result.Add('');
end;

end.

