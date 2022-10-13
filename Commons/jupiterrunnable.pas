unit JupiterRunnable;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterObject, JupiterEnviroment, SysUtils;

type

  { TJupiterRunnable }

  TJupiterRunnable = class(TJupiterObject)
  private
    FCommandLine : String;

    procedure Internal_CreateProcess(prFileName : String; prParams : String);
  published
    property CommandLine : String read FCommandLine;
  public
    procedure Execute;
    procedure OpenFolder(prFolder : String);
    procedure OpenScript(prScript : String);
    procedure OpenFile(prFile : String);

    constructor Create(prCommandLine : String; prRunOnCreate : Boolean = False);
  end;

implementation

uses JupiterApp, LCLIntf, Process, ShellApi;

{ TJupiterRunnable }

procedure TJupiterRunnable.Internal_CreateProcess(prFileName: String; prParams: String);
var
  vrProcess : TProcess;
begin
  vrProcess := TProcess.Create(nil);
  vrProcess.Executable := prFileName;

  if prParams <> EmptyStr then
    vrProcess.Parameters.Add(prParams);

  vrProcess.Execute;
  vrProcess.Free;
end;

procedure TJupiterRunnable.Execute;
var
  vrCommandLine : String;
  vrExtensions : String;
  vrContent : Integer;
begin
  vrCommandLine := vrJupiterApp.Params.ResolveString(Self.CommandLine);

  if DirectoryExists(vrCommandLine) then
  begin
    Self.OpenFolder(vrCommandLine);
    Exit;
  end;

  if FileExists(vrCommandLine) then
  begin
    vrExtensions := vrJupiterApp.Params.VariableById('Enviroment.Run.ScriptExtensions').Value;

    vrContent := Pos(AnsiUpperCase(ExtractFileExt(vrCommandLine)), AnsiUpperCase(vrExtensions));

    if vrContent <> 0 then
      Self.OpenScript(vrCommandLine)
    else
      Self.OpenFile(vrCommandLine);

    Exit;
  end;
end;

procedure TJupiterRunnable.OpenFolder(prFolder: String);
begin
  OpenDocument(prFolder);
end;

procedure TJupiterRunnable.OpenScript(prScript: String);
var
  vrStr        : TStrings;
  vrVez        : Integer;
  vrEnviroment : TJupiterEnviroment;
  vrFile       : String;
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

    Self.OpenFile(vrFile);
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
begin
  vrExtensions := vrJupiterApp.Params.VariableById('Enviroment.Run.OpenInEditorPrefExtensions').Value;

  vrContent := Pos(AnsiUpperCase(ExtractFileExt(prFile)), AnsiUpperCase(vrExtensions));

  if vrContent <> 0 then
  begin
    if vrJupiterApp.Params.VariableById('Enviroment.Run.Method').Value = 'RunCommand' then
      RunCommand(vrJupiterApp.Params.VariableById('Enviroment.Run.EditorPref').Value, [prFile], vrOutput, [poRunIdle])
    else
    begin
      if vrJupiterApp.Params.VariableById('Enviroment.Run.Method').Value = 'ShellExecute' then
        ShellExecute(0, nil, PAnsiChar(vrJupiterApp.Params.VariableById('Enviroment.Run.EditorPref').Value), PAnsiChar(prFile), nil, 0)
      else
        Self.Internal_CreateProcess(PAnsiChar(vrJupiterApp.Params.VariableById('Enviroment.Run.EditorPref').Value), PAnsiChar(prFile));
    end;
  end
  else
    OpenDocument(prFile);
end;

constructor TJupiterRunnable.Create(prCommandLine: String; prRunOnCreate: Boolean);
begin
  Self.FCommandLine := prCommandLine;

  if prRunOnCreate then
    Self.Execute;
end;

end.

