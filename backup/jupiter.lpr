program jupiter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, JupiterConsts, JupiterObject, JupiterRoute, JupiterVariable,
  JupiterRunnable, JupiterAction, JupiterForm, JupiterModule, JupiterApp,
  pascalscript, JupiterToolsModule, uHome, uExplorer, JupiterGeneratorModule,
  JupiterStandardModule, uCustomJupiterForm, uNewTask, JupiterDataProvider,
  JupiterFileDataProvider, JupiterDirectoryDataProvider, JupiterCSVDataProvider,
  uConfig, JupiterEnviroment, JupiterFormGenerator, JupiterFormField,
  JupiterVariableForm, JupiterDialogForm, uCurrentTask,
  JupiterTasksDataProvider, JupiterTaskTimesDataProvider, uEditor,
  uMessage, JupiterSystemMessage, JupiterGeneratorForm, JupiterXMLDataProvider,
  uNewAction, uNewField, JupiterGeneratorMenuItem, jupiterformutils, uNewParam,
  JupiterVariableDataProvider, uNewDataSet, uMain
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Jupiter';
  Application.Scaled:=True;
  Application.Initialize;

  vrJupiterApp := TJupiterApp.Create('jupiter', 'Jupiter');
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
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(GENERATOR_FORM_PATH, TFGenerator));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(MESSAGES_PATH, TFMessage));

  if not vrJupiterApp.ConsoleMode then
    Application.CreateForm(TFMain, FMain);

  Application.Run;
end.

