program jupiterTools;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMain, JupiterApp, uJupiterForm, JupiterModule, JupiterConfig, uConfig,
  JupiterTasks, uExplorer, JupiterConsts, fileUtils, uCurrentTask, uNewTask,
  jupiterRunner, uEditList
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;

  vrJupiterApp := TJupiterApp.Create();

  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.CreateForm(TFEditList, FEditList);
  Application.Run;
end.

