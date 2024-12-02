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

      if Self.Internal_CreateMacroIfDontExists('main.tasks.current.checklists.click', 'Clique do item de menu Tarefa: Checklists', CreateStringListToMacro('')) then
        Self.Internal_CreateRouteIfDontExists('Checklists', '/main/tasks/current/checklists/', vrWizard.GetLastID('MACROS'), ICON_CHECK, 2000);

      if Self.Internal_CreateMacroIfDontExists('main.tasks.current.times.click', 'Clique do item de menu Tarefa: Tempos', CreateStringListToMacro('  if GlobalParamExists(''Tools.Tasks.Current.ID'') then ' + #13#10 +
         '    OpenGridFromTableWithWhere(''TEMPOS'', '' TAREFA = '' + GetGlobalParam(''Tools.Tasks.Current.ID''), '''');')) then
        Self.Internal_CreateRouteIfDontExists('Tempos', '/main/tasks/current/times/', vrWizard.GetLastID('MACROS'), ICON_TIMEFILE, 3000);
    end;
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

end.

