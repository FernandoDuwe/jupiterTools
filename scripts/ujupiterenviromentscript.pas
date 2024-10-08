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

  function JupiterEnviromentScript_CreatePath(prPath : String) : String;
  function JupiterEnviromentScript_CreateFile(prPath, prContent : String) : String;
  function JupiterEnviromentScript_CreateExternalFile(prPath, prContent : String) : String;

implementation

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

{ TJupiterEnviromentcript }

function TJupiterEnviromentcript.Internal_GetName: String;
begin
  Result := 'Jupiter.EnviromentScript';
end;

procedure TJupiterEnviromentcript.DoCompile(prSender: TPSScript);
begin
  inherited DoCompile(prSender);

  prSender.AddFunction(@JupiterEnviromentScript_FileOrFolderExists, 'function FileOrFolderExists(prPath: String): Boolean;');
  prSender.AddFunction(@JupiterEnviromentScript_FileExists, 'function FileExists(prPath: String): Boolean;');
  prSender.AddFunction(@JupiterEnviromentScript_FolderExists, 'function FolderExists(prPath: String): Boolean;');

  prSender.AddFunction(@JupiterEnviromentScript_CopyFileTo, 'procedure CopyFileTo(prOrigin, prDestiny: String);');
  prSender.AddFunction(@JupiterEnviromentScript_CreatePath, 'function CreatePath(prPath : String) : String;');
  prSender.AddFunction(@JupiterEnviromentScript_CreateFile, 'function CreateFile(prPath, prContent: String): String;');
  prSender.AddFunction(@JupiterEnviromentScript_CreateExternalFile, 'function CreateExternalFile(prPath, prContent: String): String;');
end;

function TJupiterEnviromentcript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result := inherited AnalyseCode;

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function FileOrFolderExists(prPath : String) : Boolean;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function FileExists(prPath : String) : Boolean;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function FolderExists(prPath : String) : Boolean;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function CreatePath(prPath : String) : String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function CreateFile(prPath, prContent: String): String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function CreateExternalFile(prPath, prContent: String): String;'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure CopyFileTo(prOrigin, prDestiny: String);'));
end;

end.

