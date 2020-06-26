program jupiterTools;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads, cMem
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMain, JupiterForm, JupiterParams, JupiterRunnableItem, jupiterconsts,
  JupiterRunnableItemListDirectory, JupiterRunnableItemListFromFile
  { you can add units after this }
  ;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.

