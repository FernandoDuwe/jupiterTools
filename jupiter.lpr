program jupiter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, JupiterConsts, JupiterObject, JupiterRoute, JupiterVariable,
  JupiterRunnable, JupiterAction, JupiterForm, JupiterModule, JupiterApp,
  pascalscript, JupiterToolsModule, uHome, uExplorer, JupiterGeneratorModule,
  JupiterStandardModule, uNewTask, JupiterDataProvider, JupiterFileDataProvider,
  JupiterDirectoryDataProvider, JupiterCSVDataProvider, uConfig,
  JupiterEnviroment, JupiterFormGenerator, JupiterFormField,
  JupiterVariableForm, JupiterDialogForm, uCurrentTask,
  JupiterTasksDataProvider, JupiterTaskTimesDataProvider, uEditor, uMessage,
  JupiterSystemMessage, JupiterGeneratorForm, JupiterXMLDataProvider,
  uNewAction, uNewField, JupiterGeneratorMenuItem, jupiterformutils, uNewParam,
  JupiterVariableDataProvider, uNewDataSet, uMain, uGenerator, jupiterScript,
  uScriptEditor, uCustomJupiterForm, jupiterScriptFunctions, uDynamicRecord,
  jupiterThread, uProcessMonitor, jupiterStream, uPrompt;

{$R *.res}

var
  vrVez : Integer;
begin
  vrJupiterApp := TJupiterApp.Create('jupiter', 'Jupiter');

  for vrVez := 0 to ParamCount do
    vrJupiterApp.AddParam(ParamStr(vrVez));

  if not vrJupiterApp.ConsoleMode then
  begin
    RequireDerivedFormResource := True;
  Application.Title:='Jupiter';
  Application.Scaled:=True;
    Application.Initialize;
  end
  else
  begin
    {$apptype console}

    {$IFDEF WINDOWS}
    ShowWindow(GetConsoleWindow, SW_Show);
    {$ENDIF}

    Application.Initialize;
  end;

  if not vrJupiterApp.ConsoleMode then
    vrJupiterApp.AddMessage('Iniciando', Application.Title);

  vrJupiterApp.AddModule(TJupiterStandardModule.Create);
  vrJupiterApp.AddModule(TJupiterToolsModule.Create);
  vrJupiterApp.AddModule(TJupiterGeneratorModule.Create);

  // Forms
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(ROOT_FORM_PATH, TFHome));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(CONFIG_PATH, TFConfig));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(EXPLORER_FORM_PATH, TFExplorer));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(CUSTOM_FORM_PATH, TFCustomJupiterForm));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(NEWTASK_FORM_PATH, TFNewTask));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(TASK_FORM_PATH, TFCurrentTask));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(EDITOR_FORM_PATH, TFEditor));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(SCRIPTEDITOR_FORM_PATH, TFScriptEditor));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(GENERATOR_FORM_PATH, TFGenerator));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(MESSAGES_PATH, TFMessage));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(DYNAMIC_RECORD_FORM_PATH, TFDynamicRecord));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(PROCESS_MONITOR_PATH, TFProcessMonitor));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(PROMPT_FORM_PATH, TFPrompt));

  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.

