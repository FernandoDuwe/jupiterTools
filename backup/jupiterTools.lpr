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
  jupiterRunner, uEditList, uScriptEditor, jupiterchecklist, jupiterUtils,
  jupiterLog, uMessage, uNewFavoriteItem, jupiterGenerator, JupiterCustomAction,
  uGeneratorActionEditor, ugeneratoractioneditorfield, uCustomForm, 
ugeneratoractioneditoraction;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;

  vrJupiterApp := TJupiterApp.Create('JupiterTools');

  if not vrJupiterApp.ConsoleMode then
  begin
    Application.Initialize;
    Application.CreateForm(TFMain, FMain);
    Application.Run;
  end;
end.

