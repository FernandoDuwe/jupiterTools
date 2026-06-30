Program elara;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, daemonmapperunit, maindaemon
  { add your units here };

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
end.
