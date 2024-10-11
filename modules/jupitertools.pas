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

    Self.Internal_CreateRouteIfDontExists('Tarefas', '/main/tasks/', NULL_KEY, ICON_TASKS, 1000);

    if Self.Internal_CreateMacroIfDontExists('main.tools.notes.click', 'Clique do item de menu Anotações', CreateStringListToMacro('OpenGridFromTable(''ANOTACOES'');')) then
      Self.Internal_CreateRouteIfDontExists('Anotações', '/main/tools/notes/', vrWizard.GetLastID('MACROS'), ICON_IMPORTANT_MESSAGE, 50);

    Self.Internal_CreateVariablIfDontExists('Tools.Tasks.Path', 'Diretório de Tarefas', EmptyStr);
    Self.Internal_CreateVariablIfDontExists('Tools.Tasks.Current.Path', 'Diretório da Tarefa Atual', EmptyStr);
  finally
    FreeAndNil(vrWizard);
  end;
end;

end.

