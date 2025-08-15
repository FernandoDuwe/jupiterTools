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
  Forms, pascalscript, pascalscriptfcl, datetimectrls, tachartlazaruspkg,
  uJupiterForm, uMain, uDmMain, JupiterConsts, JupiterObject, JupiterEnviroment,
  JupiterModule, JupiterVariable, JupiterCSVDataProvider, JupiterApp,
  jupiterDatabaseWizard, jupiterformutils, jupiterStandard,
  jupiterMainMenuGenerator, jupiterScript, JupiterRoute, uJupiterUserExperience,
  uConfig, uJupiterGenerator, jupiterDesktopApp, uJupiterAppScript,
  uJupiterDesktopAppScript, uNewTask, uGenerator, uJupiterAction,
  uCustomDatabaseForm, jupiterformcomponenttils, jupiterStringUtils,
  uJupiterDatabaseScript, jupitertreeviewmenugenerator, JupiterXMLDataProvider,
  JupiterFileDataProvider, JupiterTaskTimesDataProvider,
  jupiterTimeControlDataProvider, JupiterTasksDataProvider,
  JupiterVariableDataProvider, JupiterDirectoryDataProvider,
  JupiterDataProvider, jupiterutilspas, ucustomdatabasegrid, uUserPreferences,
  uScriptEditorForm, uJupiterEnviromentScript, uJupiterStringUtilsScript,
  uJupiterRunnableScript, jupiterTools, uJupiterDataProviderScript,
  uSystemMonitor, uCustomCodeForm, uJupiterFormDesktopAppScript, uFileExplorer,
  jupitersqldataprovider, jupiterthread, uContextMenu, uSQLEditor, uCheckList,
  uTextEditor, uJupiterDateUtilsScript, uChartViewer,
  uJupiterCheckListUtilsScript, uWaitForm, uCodeRunner, uFileFinder,
  uFileReaderFinder, jupiterScriptList, uconsole, uExternalSQLEditor,
  uMultiLevelTextEditor, uMenuNavigator;

{$R *.res}

begin
  RequireDerivedFormResource := True;

  Application.Scaled:=True;
  Application.Initialize;

  vrJupiterApp := TJupiterDesktopApp.Create('jupiter', 'Jupiter');

  Application.CreateForm(TDMMain, DMMain);

  vrJupiterApp.AddModule(TJupiterStandardModule.Create);
  vrJupiterApp.AddModule(TJupiterUserExperience.Create);
  vrJupiterApp.AddModule(TJupiterGenerator.Create);
  vrJupiterApp.AddModule(TJupiterTools.Create);
  vrJupiterApp.AddModule(TJupiterUtils.Create);

  vrJupiterApp.LoadOtherVariables;

  TJupiterDesktopApp(vrJupiterApp).GenerateDynamicData;

  Application.CreateForm(TFMain, FMain);

  with TJupiterDesktopApp(vrJupiterApp) do
  begin
    FormRoutes.Add(TJupiterFormRoute.Create(NEWTASKMENU_PATH, TFNewTask));
    FormRoutes.Add(TJupiterFormRoute.Create(CONFIG_PATH, TFConfig));
    FormRoutes.Add(TJupiterFormRoute.Create(GENERATOR_PATH, TFGenerator));
    FormRoutes.Add(TJupiterFormRoute.Create(CUSTOMDATABASE_PATH, TFCustomDatabaseForm));
    FormRoutes.Add(TJupiterFormRoute.Create(CUSTOMGRIDDATABASE_PATH, TFCustomDatabaseGrid));
    FormRoutes.Add(TJupiterFormRoute.Create(USERPREFERENCE_PATH, TFUserPreferences));
    FormRoutes.Add(TJupiterFormRoute.Create(SCRIPTFORM_PATH, TFScriptEditorForm));
    FormRoutes.Add(TJupiterFormRoute.Create(SYSTEM_PATH, TFSystemMonitor));
    FormRoutes.Add(TJupiterFormRoute.Create(CUSTOMCODEFORM_PATH, TFCustomCodeForm));
    FormRoutes.Add(TJupiterFormRoute.Create(FILEEXPLORER_PATH, TFFileExplorer));
    FormRoutes.Add(TJupiterFormRoute.Create(CONTEXT_PATH, TFContextMenu));
    FormRoutes.Add(TJupiterFormRoute.Create(SQLEDITOR_PATH, TFSQLEditor));
    FormRoutes.Add(TJupiterFormRoute.Create(SQLEXTEDITOR_PATH, TFExternalSQLEditor));
    FormRoutes.Add(TJupiterFormRoute.Create(CHECKLIST_PATH, TFCheckList));
    FormRoutes.Add(TJupiterFormRoute.Create(TEXTEDITOR_PATH, TFTextEditor));
    FormRoutes.Add(TJupiterFormRoute.Create(MULTILEVELTEXTEDITOR_PATH, TFMultiLevelTextEditor));
    FormRoutes.Add(TJupiterFormRoute.Create(CHARTVIEWER_PATH, TFChartViewer));
    FormRoutes.Add(TJupiterFormRoute.Create(CODERUNNER_PATH, TFCodeRunner));
    FormRoutes.Add(TJupiterFormRoute.Create(FILEFINDER_PATH, TFFileFinder));
    FormRoutes.Add(TJupiterFormRoute.Create(FILEREADERFINDER_PATH, TFFileReaderFinder));
    FormRoutes.Add(TJupiterFormRoute.Create(CONSOLE_PATH, TFConsole));
    FormRoutes.Add(TJupiterFormRoute.Create(MENUEXPLORER_PATH, TFMenuNavigator));
  end;

  Application.Run;
end.

