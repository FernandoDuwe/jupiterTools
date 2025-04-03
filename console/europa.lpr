program europa;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, JupiterApp, uJupiterEnviromentScript, CustApp
  { you can add units after this };

type

  { TEuropa }

  TEuropa = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure WriteHelp; virtual;
  end;

{ TEuropa }

procedure TEuropa.DoRun;
var
  ErrorMsg: String;
begin
  vrJupiterApp := TJupiterApp.Create('Europa', 'Europa');
  try
    ErrorMsg := CheckOptions('h', 'help');

    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

  finally
    FreeAndNil(vrJupiterApp);
  end;

  Terminate;
end;

constructor TEuropa.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  StopOnException := True;
end;

destructor TEuropa.Destroy;
begin
  inherited Destroy;
end;

procedure TEuropa.WriteHelp;
begin
  WriteLn('Usage: ', ExeName, ' -h');
end;

var
  Application: TEuropa;
begin
  Application := TEuropa.Create(nil);
  Application.Title := 'Europa';
  Application.Run;
  Application.Free;
end.

