unit daemonmapperunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp;

type
  TElaraDaemonMapper = class(TDaemonMapper)
  private

  public

  end;

var
  ElaraDaemonMapper: TElaraDaemonMapper;

implementation

procedure RegisterMapper;
begin
  RegisterDaemonMapper(TElaraDaemonMapper)
end;

{$R *.lfm}


initialization
  RegisterMapper;

end.

