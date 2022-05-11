unit fileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLIntf, SysUtils;

  procedure ListDirectories(prRoot : String; var prOut : TStrings);
  procedure ListFiles(prRoot : String; var prOut : TStrings);
  function GetDirectorySeparator : String;

  procedure OpenFolder(prFolderPath : String);

implementation

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

end.

