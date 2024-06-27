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
  JupiterApp, jupiterDatabaseWizard, jupiterStandard;

{$R *.res}

begin
  RequireDerivedFormResource := True;

  Application.Scaled := True;
  Application.Initialize;

  vrJupiterApp := TJupiterApp.Create('jupiter', 'Jupiter');

  Application.CreateForm(TDMMain, DMMain);
  Application.CreateForm(TFMain, FMain);

  vrJupiterApp.AddModule(TJupiterStandardModule.Create);

  Application.Run;
end.

