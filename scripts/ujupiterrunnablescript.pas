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

implementation

uses LCLIntf, Process {$IFDEF WINDOWS} , ShellApi {$ENDIF};

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

    vrProcess.Executable := prFileName;

    if prParams <> EmptyStr then
      vrProcess.Parameters.Add(prParams);

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

  {$IFDEF WINDOWS}
    prSender.AddFunction(@JupiterRunnableScript_ShellExecute, 'procedure ShellExecute(prExecuteIn, prFile: String);');
  {$ENDIF}
end;

function TJupiterRunnableScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result := inherited AnalyseCode;

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure OpenFolder(prFolder: String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure OpenDocument(prDocument: String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure CreateProcess(prFileName: String; prParams: String; var prOutput: String; prWaitUntilEnd: Boolean; prSilent: Boolean);'));

  {$IFDEF WINDOWS}
    Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure ShellExecute(prExecuteIn, prFile: String);'));
  {$ENDIF}
end;

end.

