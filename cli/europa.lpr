program europa;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, JupiterConsts, JupiterObject, JupiterVariable, CustApp
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
  ErrorMsg : String;
begin
  WriteLn('Europa - CLI for Jupiter');

  try
    // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');

    if ErrorMsg <> EmptyStr then
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

    { add your program here }

    // stop program loop
  finally
    Terminate;
  end;
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
  { add your help code here }

  WriteLn('Usage: ', ExeName, ' -h');
end;

var
  Application: TEuropa;

{$R *.res}

begin
  Application:=TEuropa.Create(nil);
  Application.Title := 'Europa';
  Application.Run;
  Application.Free;
end.

