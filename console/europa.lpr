program europa;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, JupiterApp, uJupiterEnviromentScript, jupiterStandard,
  jupiterutilspas, jupiterTools, jupiterElara, CustApp, Interfaces,
  JupiterVariable, uDmMain
  { you can add units after this };

type

  { TEuropa }

  TEuropa = class(TCustomApplication)
  protected
    procedure DoRun; override;

    function Internal_GetCommand : String;
  published
      property Command : String read Internal_GetCommand;
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
  WriteLn('Europa - CLI for Jupiter - ' + vrJupiterApp.GetVersion);

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

  if ParamCount < 1 then
  begin
    WriteLn('You must inform the command to be executed. For more info, use the -h command.');
    Terminate;
    Exit;
  end;

  vrJupiterApp.RunMacroCLI(Self.Command, TJupiterVariableList.Create);

  Terminate;
end;

function TEuropa.Internal_GetCommand: String;
begin
  Result := Params[1];
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
  WriteLn('Database: ' + DMMain.sqlLiteInternalDatabaseConnection.DatabaseName);
  WriteLn(EmptyStr);
  WriteLn('Usage: ', ExeName, ' -h');
  WriteLn(EmptyStr);
  WriteLn('europa COMMAND');
end;

var
  Application: TEuropa;

{$R *.res}

begin
  vrJupiterApp := TJupiterApp.Create('jupiter', 'Europa - CLI for Jupiter');
  DMMain       := TDMMain.Create(nil);
  try
    vrJupiterApp.InternalDatabase := DMMain.sqlLiteInternalDatabaseConnection;

    vrJupiterApp.AddModule(TJupiterStandardModule.Create);
    vrJupiterApp.AddModule(TJupiterTools.Create);
    vrJupiterApp.AddModule(TJupiterUtils.Create);
    vrJupiterApp.AddModule(TJupiterElaraModule.Create);

    vrJupiterApp.LoadOtherVariables;

    Application := TEuropa.Create(nil);
    Application.Title := 'Europa';
    Application.Run;
    Application.Free;
  finally
    // vrJupiterApp.Free;
    FreeAndNil(DMMain);
  end;
end.

