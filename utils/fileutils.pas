unit fileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLIntf, JupiterApp, SysUtils;

  procedure ListDirectories(prRoot : String; var prOut : TStrings);
  procedure ListFiles(prRoot : String; var prOut : TStrings);
  function GetDirectorySeparator : String;

  procedure OpenFolder(prFolderPath : String);
  procedure OpenFile(prFileName : String);
  function  TratarCaminho(prCaminho : String) : String;
  procedure CreateProcess(prFileName : String; prParams : String);

implementation

uses Process, ShellApi;

procedure ListDirectories(prRoot: String; var prOut: TStrings);
var
  vrInfo : TSearchRec;
begin
  prOut.Clear;

  if FindFirst(prRoot + '*', faDirectory, vrInfo) = 0 then
    repeat
      if (((vrInfo.Name = '.') or (vrInfo.Name = '..')) or (vrInfo.Name = EmptyStr)) then
        Continue;

      if not DirectoryExists(prRoot + vrInfo.Name) then
        Continue;

      prOut.Add(Format('%0:s%1:s%2:s', [prRoot, vrInfo.Name, GetDirectorySeparator]));
    until FindNext(vrInfo) <> 0;

  FindClose(vrInfo);
end;

procedure ListFiles(prRoot: String; var prOut: TStrings);
var
  vrInfo : TSearchRec;
begin
  prOut.Clear;

  if FindFirst(prRoot + '*', faAnyFile, vrInfo) = 0 then
    repeat
      if (((vrInfo.Name = '.') or (vrInfo.Name = '..')) or (vrInfo.Name = EmptyStr)) then
        Continue;

      if not FileExists(prRoot + vrInfo.Name) then
        Continue;

      if DirectoryExists(prRoot + vrInfo.Name) then
        Continue;

      prOut.Add(Format('%0:s%1:s', [prRoot, vrInfo.Name, GetDirectorySeparator]));
    until FindNext(vrInfo) <> 0;

  FindClose(vrInfo);
end;

function GetDirectorySeparator: String;
begin
  {$IFDEF WINDOWS}
     Result := '\';
  {$ELSE}
     Result := '/';
  {$ENDIF}
end;

procedure OpenFolder(prFolderPath: String);
begin
  OpenDocument(prFolderPath);
end;

procedure OpenFile(prFileName: String);
var
  vrExtensions : String;
  vrOutput : String;
  vrContent : Integer;
begin
  vrExtensions := vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.OpenInEditorPrefExtensions').Value;

  vrContent := Pos(AnsiUpperCase(ExtractFileExt(prFileName)), AnsiUpperCase(vrExtensions));

  if vrContent <> 0 then
  begin
    if vrJupiterApp.Config.GetByID('JupiterTools.Modules.Runner.ExecMethod').Value = 'RunCommand' then
      RunCommand(vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.EditorPref').Value, [prFileName], vrOutput, [poRunIdle])
    else
    begin
      if vrJupiterApp.Config.GetByID('JupiterTools.Modules.Runner.ExecMethod').Value = 'ShellExecute' then
        ShellExecute(0, nil, PAnsiChar(vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.EditorPref').Value), PAnsiChar(prFileName), nil, 0)
      else
        CreateProcess(PAnsiChar(vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.EditorPref').Value), PAnsiChar(prFileName));
    end;
  end
  else
    OpenDocument(prFileName);
end;

function TratarCaminho(prCaminho: String): String;
begin
  Result := prCaminho;
  Result := StringReplace(Result, '\', GetDirectorySeparator, [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(Result, '/', GetDirectorySeparator, [rfIgnoreCase, rfReplaceAll]);
end;

procedure CreateProcess(prFileName: String; prParams: String);
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

end.

