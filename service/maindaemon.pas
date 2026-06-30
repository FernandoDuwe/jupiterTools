unit maindaemon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp;

type
  TMainDaemon = class(TDaemon)
  private

  public

  end;

var
  MainDaemonObj : TMainDaemon;

implementation

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TMainDaemon)
end;

{$R *.lfm}


initialization
  RegisterDaemon;

end.

