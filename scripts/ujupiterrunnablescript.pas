unit uJupiterRunnableScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, jupiterScript, JupiterConsts, JupiterVariable, JupiterEnviroment,
  SysUtils, PascalScript, uPSComponent;

type

  { TJupiterRunnableScript }

  TJupiterRunnableScript = class(TJupiterScriptLibrary)
  protected
    function Internal_GetName : String; override;
  public
    procedure DoCompile(prSender: TPSScript); override;
    function AnalyseCode: TJupiterScriptAnalyserList; override;
  end;

  procedure JupiterRunnableScript_OpenFolder(prFolder: String);
  procedure JupiterRunnableScript_OpenDocument(prDocument: String);
  procedure JupiterRunnableScript_ShellExecute(prExecuteIn, prFile : String);
  procedure JupiterRunnableScript_CreateProcess(prFileName : String; prParams : String; var prOutput : String; prWaitUntilEnd : Boolean = True; prSilent : Boolean = False);
  procedure JupiterRunnableScript_ExecuteBat(prFile : String);
  procedure JupiterRunnableScript_RunCommandOnJupiter(prParams : String);
  procedure JupiterRunnableScript_RunCommand(prFile : String; var vrOutPut : String);
  procedure JupiterRunnableScript_RunCommandOnShell(prShell, prFile : String; var vrOutPut : String);
  procedure JupiterRunnableScript_RunCommandAndWait(prFile : String; var vrOutPut : String);
  procedure JupiterRunnableScript_RunCommandOnShellAndWait(prShell, prFile : String; var vrOutPut : String);

implementation

uses LCLIntf, Process {$IFDEF WINDOWS} , Windows, ShellApi {$ENDIF}, JupiterApp;

procedure JupiterRunnableScript_OpenFolder(prFolder: String);
begin
  OpenDocument(prFolder);
end;

procedure JupiterRunnableScript_OpenDocument(prDocument: String);
begin
  OpenDocument(prDocument);
end;

procedure JupiterRunnableScript_ShellExecute(prExecuteIn, prFile: String);
begin
  {$IFDEF WINDOWS}
     ShellExecute(0, nil, PAnsiChar(prExecuteIn), PAnsiChar(prFile), nil, 0);
  {$ENDIF}
end;

procedure JupiterRunnableScript_CreateProcess(prFileName: String; prParams: String; var prOutput: String; prWaitUntilEnd: Boolean; prSilent: Boolean);
var
  vrProcess : TProcess;
  vrOutput : TStrings;
begin
  vrProcess := TProcess.Create(nil);
  vrOutput  := TStringList.Create;
  try
    vrOutput.Clear;

    if prWaitUntilEnd then
    begin
      vrProcess.Options := vrProcess.Options + [poWaitOnExit];
      vrProcess.Options := vrProcess.Options + [poUsePipes];
    end;

    if prSilent then
      vrProcess.Options := vrProcess.Options + [poNoConsole];

    vrProcess.Executable := PAnsiChar(prFileName);

    if prParams <> EmptyStr then
      vrProcess.Parameters.Add(PAnsiChar(prParams));

    vrProcess.Execute;

    if prWaitUntilEnd then
    begin
      vrOutput.LoadFromStream(vrProcess.Output);

      prOutput := vrOutput.Text;
    end;
  finally
    vrProcess.Free;
    FreeAndNil(vrOutput);
  end;
end;

procedure JupiterRunnableScript_ExecuteBat(prFile: String);
{$IFDEF WINDOWS}
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  Cmd: String;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  FillChar(SI, SizeOf(TStartupInfo), 0);
  FillChar(PI, SizeOf(TProcessInformation), 0);
  SI.cb := SizeOf(TStartupInfo);
  SI.dwFlags := STARTF_USESHOWWINDOW;
  SI.wShowWindow := SW_HIDE; // Oculta a janela

  Cmd := 'cmd.exe /c "' + prFile + '"';

  if CreateProcess(nil, PChar(Cmd), nil, nil, False, CREATE_NO_WINDOW, nil, nil, SI, PI) then
  begin
    WaitForSingleObject(PI.hProcess, INFINITE);
    CloseHandle(PI.hProcess);
    CloseHandle(PI.hThread);
  end;
  {$ENDIF}
end;

procedure JupiterRunnableScript_RunCommandOnJupiter(prParams: String);
var
  vrParams : TJupiterVariableList;
begin
  vrParams := TJupiterVariableList.Create;
  vrParams.AddVariable('PARAMS', prParams);

  vrJupiterApp.RunMacro(TRIGGER_ONEXECUTE, vrParams);
end;

procedure JupiterRunnableScript_RunCommand(prFile : String; var vrOutPut : String);
begin
  RunCommand(prFile, [], vrOutPut, [poNoConsole]);
end;

procedure JupiterRunnableScript_RunCommandOnShell(prShell, prFile: String; var vrOutPut: String);
begin
  RunCommand(prShell, [prFile], vrOutPut, [poRunIdle]);
end;

procedure JupiterRunnableScript_RunCommandAndWait(prFile: String; var vrOutPut: String);
begin
  RunCommand(prFile, [], vrOutPut, [poNoConsole, poWaitOnExit]);
end;

procedure JupiterRunnableScript_RunCommandOnShellAndWait(prShell, prFile: String; var vrOutPut: String);
begin
  RunCommand(prShell, [prFile], vrOutPut, [poRunIdle, poWaitOnExit]);
end;

{ TJupiterRunnableScript }

function TJupiterRunnableScript.Internal_GetName: String;
begin
  Result := 'Jupiter.RunnableScript';
end;

procedure TJupiterRunnableScript.DoCompile(prSender: TPSScript);
begin
  inherited DoCompile(prSender);

  prSender.AddFunction(@JupiterRunnableScript_OpenFolder, 'procedure OpenFolder(prFolder: String);');
  prSender.AddFunction(@JupiterRunnableScript_OpenDocument, 'procedure OpenDocument(prDocument: String);');
  prSender.AddFunction(@JupiterRunnableScript_CreateProcess, 'procedure CreateProcess(prFileName: String; prParams: String; var prOutput: String; prWaitUntilEnd: Boolean; prSilent: Boolean);');
  prSender.AddFunction(@JupiterRunnableScript_RunCommandOnJupiter, 'procedure RunCommandOnJupiter(prParams: String);');

  prSender.AddFunction(@JupiterRunnableScript_RunCommand, 'procedure RunCommand(prFile : String; var vrOutPut : String);');
  prSender.AddFunction(@JupiterRunnableScript_RunCommandOnShell, 'procedure RunCommandOnShell(prShell, prFile : String; var vrOutPut : String);');
  prSender.AddFunction(@JupiterRunnableScript_RunCommandAndWait, 'procedure RunCommandAndWait(prFile : String; var vrOutPut : String);');
  prSender.AddFunction(@JupiterRunnableScript_RunCommandOnShellAndWait, 'procedure RunCommandOnShellAndWait(prShell, prFile : String; var vrOutPut : String);');

  {$IFDEF WINDOWS}
    prSender.AddFunction(@JupiterRunnableScript_ShellExecute, 'procedure ShellExecute(prExecuteIn, prFile: String);');
    prSender.AddFunction(@JupiterRunnableScript_ExecuteBat, 'procedure ExecuteBat(prFile: String);');
  {$ENDIF}
end;

function TJupiterRunnableScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result := inherited AnalyseCode;

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure OpenFolder(prFolder: String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure OpenDocument(prDocument: String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure CreateProcess(prFileName: String; prParams: String; var prOutput: String; prWaitUntilEnd: Boolean; prSilent: Boolean);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure RunCommandOnJupiter(prParams: String);'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure RunCommand(prFile : String; var vrOutPut : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure RunCommandOnShell(prShell, prFile : String; var vrOutPut : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure RunCommandAndWait(prFile : String; var vrOutPut : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure RunCommandOnShellAndWait(prShell, prFile : String; var vrOutPut : String);'));

  {$IFDEF WINDOWS}
    Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure ShellExecute(prExecuteIn, prFile: String);'));
    Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure ExecuteBat(prFile: String);'));
  {$ENDIF}
end;

end.

