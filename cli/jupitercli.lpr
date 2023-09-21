program jupitercli;

{$mode objfpc}{$H+}
{$DEFINE JUPITERCLI}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, JupiterObject, JupiterXMLDataProvider,
  JupiterVariableDataProvider, JupiterVariable, jupiterThread,
  JupiterTaskTimesDataProvider, JupiterTasksDataProvider, JupiterSystemMessage,
  jupiterStream, jupiterScriptFunctions, jupiterScript, JupiterRunnable,
  JupiterRoute, JupiterModule, JupiterFileDataProvider,
  JupiterDirectoryDataProvider, JupiterDataProvider, JupiterCSVDataProvider,
  JupiterConsts, JupiterApp, JupiterAction, CustApp

  { you can add units after this };

type

  { TJupiterCli }

  TJupiterCli = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TJupiterCli }

procedure TJupiterCli.DoRun;
var
  ErrorMsg: String;
begin
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
  Terminate;
end;

constructor TJupiterCli.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  StopOnException := True;
end;

destructor TJupiterCli.Destroy;
begin
  inherited Destroy;
end;

procedure TJupiterCli.WriteHelp;
begin
  WriteLn('Usage: ', ExeName, ' -h');
end;

var
  Application: TJupiterCli;

{$R *.res}

begin
  Application := TJupiterCli.Create(nil);
  Application.Title := 'Jupiter Cli';
  Application.Run;
  Application.Free;
end.

