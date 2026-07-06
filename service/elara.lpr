Program elara;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, pascalscript, mapper, jupiterthread, JupiterApp,
  uJupiterEnviromentScript, maindaemon, Interfaces, udmservice
  { add your units here };

{$R *.res}

begin
  Application.Initialize;

  vrJupiterApp := TJupiterApp.Create('Elara', 'Elara');

  Application.Run;
end.
