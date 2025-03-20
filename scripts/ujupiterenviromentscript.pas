unit uJupiterEnviromentScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, jupiterScript, JupiterConsts, JupiterVariable, JupiterEnviroment,
  SysUtils, PascalScript, uPSComponent;

type

  { TJupiterEnviromentcript }

  TJupiterEnviromentcript = class(TJupiterScriptLibrary)
  protected
    function Internal_GetName : String; override;
  public
    procedure DoCompile(prSender: TPSScript); override;
    function AnalyseCode: TJupiterScriptAnalyserList; override;
  end;

  function JupiterEnviromentScript_FileOrFolderExists(prPath : String) : Boolean;
  function JupiterEnviromentScript_FileExists(prPath : String) : Boolean;
  function JupiterEnviromentScript_FolderExists(prPath : String) : Boolean;
  procedure JupiterEnviromentScript_CopyFileTo(prOrigin, prDestiny : String);
  procedure JupiterEnviromentScript_DeleteFile(prFileName : String);
  function JupiterEnviromentScript_SameExtension(prFileName, prExtension : String) : Boolean;

  function JupiterEnviromentScript_ExtractFileName(prFileName : String) : String;
  function JupiterEnviromentScript_ExtractFileDir(prFileName : String) : String;
  function JupiterEnviromentScript_ExtractFileExt(prFileName : String) : String;

  function JupiterEnviromentScript_CreatePath(prPath : String) : String;
  function JupiterEnviromentScript_CreateFile(prPath, prContent : String) : String;
  function JupiterEnviromentScript_CreateExternalFile(prPath, prContent : String) : String;

  function JupiterEnviromentScript_LoadFromFile(prFileName : String) : String;
  procedure JupiterEnviromentScript_SaveToFile(prFileName, prData : String);
  procedure JupiterEnviromentScript_CopyTextToClipboard(prText : String);

  function JupiterEnviromentScript_GetApplicationPath : String;

implementation

uses Clipbrd;

function JupiterEnviromentScript_FileOrFolderExists(prPath: String): Boolean;
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    Result := vrEnviroment.Exists(prPath);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

function JupiterEnviromentScript_FileExists(prPath: String): Boolean;
begin
  Result := FileExists(prPath);
end;

function JupiterEnviromentScript_FolderExists(prPath: String): Boolean;
begin
  Result := DirectoryExists(prPath);
end;

procedure JupiterEnviromentScript_CopyFileTo(prOrigin, prDestiny: String);
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.CopyFileTo(prOrigin, prDestiny);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure JupiterEnviromentScript_DeleteFile(prFileName: String);
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.DeleteFileOnDisk(prFileName);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

function JupiterEnviromentScript_SameExtension(prFileName, prExtension: String) : Boolean;
begin
  Result := AnsiUpperCase(ExtractFileExt(prFileName)) = AnsiUpperCase(prExtension);
end;

function JupiterEnviromentScript_ExtractFileName(prFileName: String): String;
begin
  Result := ExtractFileName(prFileName);
end;

function JupiterEnviromentScript_ExtractFileDir(prFileName: String): String;
begin
  Result := ExtractFileDir(prFileName);
end;

function JupiterEnviromentScript_ExtractFileExt(prFileName: String): String;
begin
  Result := ExtractFileExt(prFileName);
end;

function JupiterEnviromentScript_CreatePath(prPath: String): String;
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    Result := vrEnviroment.CreatePath(prPath);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

function JupiterEnviromentScript_CreateFile(prPath, prContent: String): String;
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    Result := vrEnviroment.CreateFile(prPath, prContent);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

function JupiterEnviromentScript_CreateExternalFile(prPath, prContent: String
  ): String;
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    Result := vrEnviroment.CreateExternalFile(prPath, prContent);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

function JupiterEnviromentScript_LoadFromFile(prFileName: String): String;
var
  vrStr : TStrings;
begin
  Result := EmptyStr;

  if not FileExists(prFileName) then
    Exit;

  vrStr := TStringList.Create;
  try
    vrStr.LoadFromFile(prFileName);

    Result := vrStr.Text;
  finally
    FreeAndNil(vrStr);
  end;
end;

procedure JupiterEnviromentScript_SaveToFile(prFileName, prData: String);
var
  vrStr : TStrings;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add(prData);

    vrStr.SaveToFile(prFileName);
  finally
    FreeAndNil(vrStr);
  end;
end;

procedure JupiterEnviromentScript_CopyTextToClipboard(prText: String);
begin
  Clipboard.AsText := prText;
end;

function JupiterEnviromentScript_GetApplicationPath: String;
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    Result := vrEnviroment.BasePath;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

{ TJupiterEnviromentcript }

function TJupiterEnviromentcript.Internal_GetName: String;
begin
  Result := 'Jupiter.EnviromentScript';
end;

procedure TJupiterEnviromentcript.DoCompile(prSender: TPSScript);
begin
  inherited DoCompile(prSender);

  prSender.AddFunction(@JupiterEnviromentScript_GetApplicationPath, 'function GetApplicationPath: String;');
  prSender.AddFunction(@JupiterEnviromentScript_FileOrFolderExists, 'function FileOrFolderExists(prPath: String): Boolean;');
  prSender.AddFunction(@JupiterEnviromentScript_FileExists, 'function FileExists(prPath: String): Boolean;');
  prSender.AddFunction(@JupiterEnviromentScript_FolderExists, 'function FolderExists(prPath: String): Boolean;');

  prSender.AddFunction(@JupiterEnviromentScript_ExtractFileName, 'function ExtractFileName(prFileName: String): String;');
  prSender.AddFunction(@JupiterEnviromentScript_ExtractFileDir, 'function ExtractFileDir(prFileName: String): String;');
  prSender.AddFunction(@JupiterEnviromentScript_ExtractFileExt, 'function ExtractFileExt(prFileName: String): String;');

  prSender.AddFunction(@JupiterEnviromentScript_DeleteFile, 'procedure DeleteFileOnDisk(prFileName : String);');
  prSender.AddFunction(@JupiterEnviromentScript_CopyFileTo, 'procedure CopyFileTo(prOrigin, prDestiny: String);');
  prSender.AddFunction(@JupiterEnviromentScript_CreatePath, 'function CreatePath(prPath : String) : String;');
  prSender.AddFunction(@JupiterEnviromentScript_CreateFile, 'function CreateFile(prPath, prContent: String): String;');
  prSender.AddFunction(@JupiterEnviromentScript_CreateExternalFile, 'function CreateExternalFile(prPath, prContent: String): String;');

  prSender.AddFunction(@JupiterEnviromentScript_LoadFromFile, 'function LoadFromFile(prFileName : String): String;');
  prSender.AddFunction(@JupiterEnviromentScript_SaveToFile, 'procedure SaveToFile(prFileName, prData : String);');

  prSender.AddFunction(@JupiterEnviromentScript_CopyTextToClipboard, 'procedure CopyTextToClipboard(prText: String);');

  prSender.AddFunction(@JupiterEnviromentScript_SameExtension, 'function SameExtension(prFileName, prExtension : String) : Boolean;');
end;

function TJupiterEnviromentcript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result := inherited AnalyseCode;

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function GetApplicationPath : String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function FileOrFolderExists(prPath : String) : Boolean;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function FileExists(prPath : String) : Boolean;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function FolderExists(prPath : String) : Boolean;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function CreatePath(prPath : String) : String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function CreateFile(prPath, prContent: String): String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function CreateExternalFile(prPath, prContent: String): String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function LoadFromFile(prFileName : String): String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function SameExtension(prFileName, prExtension : String) : Boolean;'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function ExtractFileName(prFileName : String) : Boolean;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function ExtractFileDir(prFileName : String) : Boolean;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function ExtractFileExt(prFileName : String) : Boolean;'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure CopyFileTo(prOrigin, prDestiny: String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure DeleteFileOnDisk(prFileName : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure SaveToFile(prFileName, prData : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure CopyTextToClipboard(prText: String);'));
end;

end.

