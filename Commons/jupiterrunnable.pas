unit JupiterRunnable;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, {$IFNDEF JUPITERCLI} Controls, Forms, {$ENDIF}
  JupiterObject, JupiterEnviroment,
  JupiterSystemMessage, SysUtils;

type

  { TJupiterRunnable }

  TJupiterRunnable = class(TJupiterObject)
  private
    FLogMessage : Boolean;
    FCommandLine : String;
    FOutPut : String;

    procedure Internal_CreateProcess(prFileName : String; prParams : String; var prOutput : String; prWaitUntilEnd : Boolean = True; prSilent : Boolean = False);
  published
    property CommandLine : String  read FCommandLine write FCommandLine;
    property LogMessage  : Boolean read FLogMessage  write FLogMessage;
    property OutPut      : String  read FOutPut;
  public
    procedure Execute;
    procedure OpenFolder(prFolder : String);
    procedure OpenScript(prScript : String);
    procedure OpenFile(prFile : String);
    procedure OpenJPasScript(prScriptFile : String);

    procedure RunCommandLine(var prOutput : String; prWait : Boolean = True);

    constructor Create(prCommandLine : String; prRunOnCreate : Boolean = False);
  end;

implementation

uses JupiterApp, LCLIntf, Process {$IFDEF WINDOWS} , ShellApi {$ENDIF}, jupiterScript, StrUtils;

{ TJupiterRunnable }

procedure TJupiterRunnable.Internal_CreateProcess(prFileName: String; prParams: String; var prOutput : String; prWaitUntilEnd : Boolean = True; prSilent : Boolean = False);
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

procedure TJupiterRunnable.Execute;
var
  vrCommandLine : String;
  vrExtensions : String;
  vrContent : Integer;
  vrOutput : String;
begin
  {$IFNDEF JUPITERCLI}
  if Assigned(Application.MainForm) then
    Application.MainForm.Cursor := crHourGlass;
  {$ENDIF}

  try
    if Assigned(vrJupiterApp) then
    begin
      vrCommandLine := vrJupiterApp.Params.ResolveString(Self.CommandLine);

      if DirectoryExists(vrCommandLine) then
      begin
        Self.OpenFolder(vrCommandLine);
        Exit;
      end;

      vrContent := Pos(AnsiUpperCase(ExtractFileExt(vrCommandLine)), AnsiUpperCase('.jpas'));

      if vrContent <> 0 then
      begin
        Self.OpenJPasScript(vrCommandLine);
        Exit;
      end;

      vrExtensions := vrJupiterApp.Params.VariableById('Enviroment.Run.ScriptExtensions').Value;

      vrContent := Pos(AnsiUpperCase(ExtractFileExt(vrCommandLine)), AnsiUpperCase(vrExtensions));

      if vrContent <> 0 then
        Self.OpenScript(vrCommandLine)
      else
      begin
        if FileExists(vrCommandLine) then
          Self.OpenFile(vrCommandLine)
        else
          Self.RunCommandLine(vrOutput, False);
      end;
    end
    else
    begin
      vrCommandLine := Self.CommandLine;

      if DirectoryExists(vrCommandLine) then
      begin
        Self.OpenFolder(vrCommandLine);
        Exit;
      end;

      Self.RunCommandLine(vrOutput, True);
    end;
  finally
    Self.FOutPut := vrOutput;

    {$IFNDEF JUPITERCLI}
    if Assigned(Application.MainForm) then
      Application.MainForm.Cursor := crDefault;
    {$ENDIF}
  end;
end;

procedure TJupiterRunnable.OpenFolder(prFolder: String);
begin
  try
    OpenDocument(prFolder);
  finally
    if Assigned(vrJupiterApp) then
      vrJupiterApp.AddMessage('Diretório Aberto', Self.ClassName).Details.Add('Pasta: ' + prFolder);
  end;
end;

procedure TJupiterRunnable.OpenScript(prScript: String);
var
  vrStr        : TStrings;
  vrVez        : Integer;
  vrEnviroment : TJupiterEnviroment;
  vrFile       : String;
  vrOutput     : String;
begin
  vrStr        := TStringList.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrStr.Clear;
    vrStr.LoadFromFile(prScript);

    for vrVez := 0 to vrStr.Count - 1 do
      vrStr[vrVez] := vrJupiterApp.Params.ResolveString(vrStr[vrVez]);

    vrFile := vrEnviroment.FullPath('temp/temp' + ExtractFileExt(prScript));

    vrStr.SaveToFile(vrFile);

    with vrJupiterApp.AddMessage('Preparando script', Self.ClassName) do
      Details.AddStrings(vrStr);

    if ((not vrJupiterApp.Params.Exists('Enviroment.Run.ShellScript')) or (Trim(vrJupiterApp.Params.VariableById('Enviroment.Run.ShellScript').Value) = EmptyStr)) then
      RunCommand(vrFile, [], vrOutput, [poNoConsole])
    else
      RunCommand(vrJupiterApp.Params.VariableById('Enviroment.Run.ShellScript').Value, [vrFile], vrOutput, [poRunIdle]);

    with vrJupiterApp.AddMessage('Script executado', Self.ClassName) do
      Details.Add(Trim(vrOutput));
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterRunnable.OpenFile(prFile: String);
var
  vrExtensions : String;
  vrOutput : String;
  vrContent : Integer;
  vrMetodo : String;
begin
  vrOutput := EmptyStr;
  vrMetodo := EmptyStr;

  if Assigned(vrJupiterApp) then
    vrExtensions := vrJupiterApp.Params.VariableById('Enviroment.Run.OpenInEditorPrefExtensions').Value
  else
    vrExtensions := EmptyStr;

  vrContent := Pos(AnsiUpperCase(ExtractFileExt(prFile)), AnsiUpperCase(vrExtensions));

  try
    if vrContent <> 0 then
    begin
      if vrJupiterApp.Params.VariableById('Enviroment.Run.Method').Value = 'RunCommand' then
      begin
        vrMetodo := 'RunCommand';
        RunCommand(vrJupiterApp.Params.VariableById('Enviroment.Run.EditorPref').Value, [prFile], vrOutput, [poRunIdle]);
      end
      else
      begin
        if vrJupiterApp.Params.VariableById('Enviroment.Run.Method').Value = 'ShellExecute' then
        begin
          vrMetodo := 'ShellExecute';

          {$IFDEF WINDOWS}
                    ShellExecute(0, nil, PAnsiChar(vrJupiterApp.Params.VariableById('Enviroment.Run.EditorPref').Value), PAnsiChar(prFile), nil, 0);
          {$ENDIF}
        end
        else
        begin
          vrMetodo := 'CreateProcess';
          Self.Internal_CreateProcess(PAnsiChar(vrJupiterApp.Params.VariableById('Enviroment.Run.EditorPref').Value), PAnsiChar(prFile), vrOutput, False);
        end;
      end;
    end
    else
    begin
      vrMetodo := 'CreateProcess';
      OpenDocument(prFile);
    end;
  finally
    with vrJupiterApp.AddMessage(ExtractFileName(prFile), Self.ClassName) do
    begin
      Details.Add('Arquivo: ' + prFile);
      Details.Add('Método: ' + vrMetodo);
      Details.Add(EmptyStr);
      Details.Add('Saída: ');
      Details.Add(vrOutput);
    end;
  end;
end;

procedure TJupiterRunnable.OpenJPasScript(prScriptFile: String);
var
  vrScript : TJupiterScript;
  vrMessage : TStrings;
begin
  vrScript := TJupiterScript.Create;
  try
    vrScript.LoadFromFile(prScriptFile);

    vrScript.Execute;

    vrJupiterApp.AddMessage(ExtractFileName(prScriptFile) + ': Iniciando', Self.ClassName).Details.Add('Arquivo: ' + prScriptFile);

    vrJupiterApp.AddMessage(ExtractFileName(prScriptFile) + ': Compilando', Self.ClassName).Details.AddStrings(vrScript.Messages);

    if vrScript.Compiled then
    begin
      vrJupiterApp.AddMessage(ExtractFileName(prScriptFile) + ': Executando', Self.ClassName).Details.AddStrings(vrScript.RunMessages);

      if vrScript.Runned then
        vrJupiterApp.AddMessage(ExtractFileName(prScriptFile) + ': Execução bem sucedida', Self.ClassName).Details.Add('Arquivo: ' + prScriptFile)
      else
      begin
        vrJupiterApp.AddMessage(ExtractFileName(prScriptFile) + ': Erro ao executar', Self.ClassName).Details.Add('Arquivo: ' + prScriptFile);

        vrMessage := TStringList.Create;
        vrMessage.Clear;
        vrMessage.Add('Erro ao executar o script: ' + ExtractFileName(prScriptFile));
        vrMessage.AddStrings(vrScript.Messages);

        vrJupiterApp.Popup('Erro ao executar', vrMessage);
      end;
    end
    else
    begin
      vrMessage := TStringList.Create;
      vrMessage.Clear;
      vrMessage.Add('Erro ao compilar o script: ' + ExtractFileName(prScriptFile));
      vrMessage.AddStrings(vrScript.Messages);

      vrJupiterApp.Popup('Erro ao compilar', vrMessage);
    end;
  finally
    FreeAndNil(vrScript);
  end;
end;

procedure TJupiterRunnable.RunCommandLine(var prOutput: String; prWait : Boolean = True);
var
  vrStr     : TStrings;
  vrVez     : Integer;
  vrParams  : String;
  vrMessage : TJupiterSystemMessage;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Delimiter     := ' ';

    if Assigned(vrJupiterApp) then
      vrStr.DelimitedText := vrJupiterApp.Params.ResolveString(Self.CommandLine)
    else
      vrStr.DelimitedText := Self.CommandLine;

    vrParams := EmptyStr;

    if vrStr.Count = 0 then
      Exit;

    for vrVez := 1 to vrStr.Count - 1 do
      vrParams := vrParams + ' ' + vrStr[vrVez];

    Self.Internal_CreateProcess(vrStr[0], vrParams, prOutput, prWait, True);
  finally
    if Self.LogMessage then
    begin
      vrMessage := vrJupiterApp.AddMessage('Comando executado', Self.ClassName);

      vrMessage.Details.Add('Comando: ' + vrStr[0]);
      vrMessage.Details.Add('Parâmetros: ' + vrParams);
      vrMessage.Details.Add('Saída:');
      vrMessage.Details.Add(prOutput);
    end;

    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

constructor TJupiterRunnable.Create(prCommandLine: String; prRunOnCreate: Boolean);
begin
  Self.FOutPut      := EmptyStr;
  Self.FLogMessage  := False;
  Self.FCommandLine := prCommandLine;

  if prRunOnCreate then
    Self.Execute;
end;

end.

