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
  Forms, uJupiterForm, uMain, uDmMain, JupiterConsts, JupiterObject,
  JupiterEnviroment, JupiterModule, JupiterVariable, JupiterCSVDataProvider,
  JupiterApp, jupiterDatabaseWizard, jupiterformutils, jupiterStandard, 
jupiterMainMenuGenerator, uJupiterUserExperience;

{$R *.res}

begin
  RequireDerivedFormResource := True;

  Application.Scaled := True;
  Application.Initialize;

  vrJupiterApp := TJupiterApp.Create('jupiter', 'Jupiter');

  Application.CreateForm(TDMMain, DMMain);
  Application.CreateForm(TFMain, FMain);

  vrJupiterApp.AddModule(TJupiterStandardModule.Create);
  vrJupiterApp.AddModule(TJupiterUserExperience.Create);

  Application.Run;
end.

