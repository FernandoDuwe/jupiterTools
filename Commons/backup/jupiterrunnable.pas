unit JupiterRunnable;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, Forms, JupiterObject, JupiterEnviroment, SysUtils;

type

  { TJupiterRunnable }

  TJupiterRunnable = class(TJupiterObject)
  private
    FCommandLine : String;

    procedure Internal_CreateProcess(prFileName : String; prParams : String; var prOutput : String);
  published
    property CommandLine : String read FCommandLine write FCommandLine ;
  public
    procedure Execute;
    procedure OpenFolder(prFolder : String);
    procedure OpenScript(prScript : String);
    procedure OpenFile(prFile : String);

    constructor Create(prCommandLine : String; prRunOnCreate : Boolean = False);
  end;

implementation

uses JupiterApp, LCLIntf, Process {$IFDEF WINDOWS} , ShellApi {$ENDIF};

{ TJupiterRunnable }

procedure TJupiterRunnable.Internal_CreateProcess(prFileName: String; prParams: String; var prOutput : String);
var
  vrProcess : TProcess;
  vrOutput : TStrings;
begin
  vrProcess := TProcess.Create(nil);
  vrOutput  := TStringList.Create;
  try
    vrOutput.Clear;

    vrProcess.Options := vrProcess.Options + [poWaitOnExit, poUsePipes];
    vrProcess.Executable := prFileName;

    if prParams <> EmptyStr then
      vrProcess.Parameters.Add(prParams);

    vrProcess.Execute;

    vrOutput.LoadFromStream(vrProcess.Output);

    prOutput := vrOutput.Text;
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
begin
  Application.MainForm.Cursor := crHourGlass;

  try
    vrCommandLine := vrJupiterApp.Params.ResolveString(Self.CommandLine);

    if DirectoryExists(vrCommandLine) then
    begin
      Self.OpenFolder(vrCommandLine);
      Exit;
    end;

    vrExtensions := vrJupiterApp.Params.VariableById('Enviroment.Run.ScriptExtensions').Value;

    vrContent := Pos(AnsiUpperCase(ExtractFileExt(vrCommandLine)), AnsiUpperCase(vrExtensions));

    if vrContent <> 0 then
      Self.OpenScript(vrCommandLine)
    else
      Self.OpenFile(vrCommandLine);
  finally
    Application.MainForm.Cursor := crDefault;
  end;
end;

procedure TJupiterRunnable.OpenFolder(prFolder: String);
begin
  try
    OpenDocument(prFolder);
  finally
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
  vrExtensions := vrJupiterApp.Params.VariableById('Enviroment.Run.OpenInEditorPrefExtensions').Value;

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
          Self.Internal_CreateProcess(PAnsiChar(vrJupiterApp.Params.VariableById('Enviroment.Run.EditorPref').Value), PAnsiChar(prFile), vrOutput);
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

constructor TJupiterRunnable.Create(prCommandLine: String; prRunOnCreate: Boolean);
begin
  Self.FCommandLine := prCommandLine;

  if prRunOnCreate then
    Self.Execute;
end;

end.

