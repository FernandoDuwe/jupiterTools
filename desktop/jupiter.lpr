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
  Forms, pascalscript, pascalscriptfcl, datetimectrls, uJupiterForm, uMain,
  uDmMain, JupiterConsts, JupiterObject, JupiterEnviroment, JupiterModule,
  JupiterVariable, JupiterCSVDataProvider, JupiterApp, jupiterDatabaseWizard,
  jupiterformutils, jupiterStandard, jupiterMainMenuGenerator, jupiterScript,
  JupiterRoute, uJupiterUserExperience, uConfig, uJupiterGenerator,
  jupiterDesktopApp, uJupiterAppScript, uJupiterDesktopAppScript, uNewTask,
  uGenerator, uJupiterAction, uCustomDatabaseForm, jupiterformcomponenttils,
  jupiterStringUtils, uJupiterDatabaseScript, jupitertreeviewmenugenerator,
  jupiterutilspas, ucustomdatabasegrid, uUserPreferences;

{$R *.res}

begin
  RequireDerivedFormResource := True;

  Application.Scaled := True;
  Application.Initialize;

  vrJupiterApp := TJupiterDesktopApp.Create('jupiter', 'Jupiter');

  Application.CreateForm(TDMMain, DMMain);
  Application.CreateForm(TFMain, FMain);

  vrJupiterApp.AddModule(TJupiterStandardModule.Create);
  vrJupiterApp.AddModule(TJupiterUserExperience.Create);
  vrJupiterApp.AddModule(TJupiterGenerator.Create);
  vrJupiterApp.AddModule(TJupiterUtils.Create);

  with TJupiterDesktopApp(vrJupiterApp) do
  begin
    FormRoutes.Add(TJupiterFormRoute.Create(NEWTASKMENU_PATH, TFNewTask));
    FormRoutes.Add(TJupiterFormRoute.Create(CONFIG_PATH, TFConfig));
    FormRoutes.Add(TJupiterFormRoute.Create(GENERATOR_PATH, TFGenerator));
    FormRoutes.Add(TJupiterFormRoute.Create(CUSTOMDATABASE_PATH, TFCustomDatabaseForm));
    FormRoutes.Add(TJupiterFormRoute.Create(CUSTOMGRIDDATABASE_PATH, TFCustomDatabaseGrid));
    FormRoutes.Add(TJupiterFormRoute.Create(USERPREFERENCE_PATH, TFUserPreferences));
  end;

  Application.Run;
end.

