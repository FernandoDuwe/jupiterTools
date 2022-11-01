unit JupiterEnviroment;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, JupiterConsts, JupiterObject;

type

  { TJupiterEnviroment }

  TJupiterEnviroment = class(TObject)
  private
    FBasePath : String;
  protected
    function Internal_GetBasePath : String;
  published
    property BasePath : String read Internal_GetBasePath write FBasePath;
  public
    function CreatePath(prPath : String) : String;
    function CreateFile(prPath, prContent : String) : String;
    function FullPath(prPath : String) : String;
    function IconOfFile(prFileName : String) : Integer;

    function IsDocFile(prFileName : String) : Boolean;
    function IsTechFile(prFileName : String) : Boolean;
    function IsTextFile(prFileName : String) : Boolean;
    function IsSheetFile(prFileName : String) : Boolean;
    function IsPictureFile(prFileName : String) : Boolean;
    function IsCheckListFile(prFileName : String) : Boolean;
    function IsExecutableFile(prFileName : String) : Boolean;

    function CanChangeFileContent(prFileName : String) : Boolean;

    procedure CopyFileTo(prOrigin, prDestiny : String);
    function  IsOfExtension(prFileName, prGroupOfExtensions : String) : Boolean;
    function  OpenFile(prDefaultExtensions : String) : String;
  end;

implementation

uses Dialogs, JupiterApp, FileUtil;

{ TJupiterEnviroment }

function TJupiterEnviroment.Internal_GetBasePath: String;
begin
  if Trim(Self.FBasePath) <> EmptyStr then
  begin
    Result := Self.FBasePath;
    Exit;
  end;

  Result := ExtractFileDir(Application.ExeName);

  if Copy(Result, Length(Result), 1) <>  GetDirectorySeparator then
     Result := Result + GetDirectorySeparator;

  Self.FBasePath := Result;
end;

function TJupiterEnviroment.CreatePath(prPath: String) : String;
begin
  Result := Self.FullPath(prPath);

  if not DirectoryExists(Result) then
  begin
    CreateDir(Result);

    vrJupiterApp.AddMessage('Diretório criado', Self.ClassName).Details.Add('Diretório: ' + Result);
  end;

  if Copy(Result, Length(Result), 1) <>  GetDirectorySeparator then
     Result := Result + GetDirectorySeparator;
end;

function TJupiterEnviroment.CreateFile(prPath, prContent : String): String;
var
  vrStr : TStrings;
begin
  Result := Self.FullPath(prPath);

  if FileExists(Result) then
    Exit;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add(prContent);
    vrStr.SaveToFile(Result);

    vrJupiterApp.AddMessage('Arquivo criado', Self.ClassName).Details.Add('Arquivo: ' + Result);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

function TJupiterEnviroment.FullPath(prPath: String): String;
begin
  prPath := StringReplace(prPath, '\', GetDirectorySeparator, [rfIgnoreCase, rfReplaceAll]);
  prPath := StringReplace(prPath, '/', GetDirectorySeparator, [rfIgnoreCase, rfReplaceAll]);

  Result := Self.Internal_GetBasePath + prPath;
end;

function TJupiterEnviroment.IconOfFile(prFileName: String): Integer;
begin
  if DirectoryExists(prFileName) then
  begin
    Result := ICON_OPEN;
    Exit;
  end;

  Result := NULL_KEY;

  if AnsiUpperCase(ExtractFileName(prFileName)) = AnsiUpperCase('Tempos.txt') then
  begin
    Result := ICON_TIMEFILE;
    Exit;
  end;

  if Self.IsDocFile(prFileName) then
  begin
    Result := ICON_DOCFILE;
    Exit;
  end;

  if Self.IsTechFile(prFileName) then
  begin
    Result := ICON_TECHFILE;
    Exit;
  end;

  if Self.IsTextFile(prFileName) then
  begin
    Result := ICON_TXTFILE;
    Exit;
  end;

  if Self.IsSheetFile(prFileName) then
  begin
    Result := ICON_SHEETFILE;
    Exit;
  end;

  if Self.IsPictureFile(prFileName) then
  begin
    Result := ICON_PICTUREFILE;
    Exit;
  end;

  if Self.IsCheckListFile(prFileName) then
  begin
    Result := ICON_CHECK;
    Exit;
  end;

  if Self.IsExecutableFile(prFileName) then
  begin
    Result := ICON_APPLICATION;
    Exit;
  end;
end;

function TJupiterEnviroment.IsDocFile(prFileName: String): Boolean;
begin
  Result := Pos(AnsiUpperCase(ExtractFileExt(prFileName)), AnsiUpperCase('.doc .docx .rtf')) <> 0;
end;

function TJupiterEnviroment.IsTechFile(prFileName: String): Boolean;
begin
  Result := Pos(AnsiUpperCase(ExtractFileExt(prFileName)), AnsiUpperCase('.bat .cs .php .js .py .vb .json .xml .sql')) <> 0;
end;

function TJupiterEnviroment.IsTextFile(prFileName: String): Boolean;
begin
  Result := Pos(AnsiUpperCase(ExtractFileExt(prFileName)), AnsiUpperCase('.txt .md')) <> 0;
end;

function TJupiterEnviroment.IsSheetFile(prFileName: String): Boolean;
begin
  Result := Pos(AnsiUpperCase(ExtractFileExt(prFileName)), AnsiUpperCase('.csv .xls .xlsx')) <> 0;
end;

function TJupiterEnviroment.IsPictureFile(prFileName: String): Boolean;
begin
  Result := Pos(AnsiUpperCase(ExtractFileExt(prFileName)), AnsiUpperCase('.png .ico .jpg .jpeg .bmp')) <> 0;
end;

function TJupiterEnviroment.IsCheckListFile(prFileName: String): Boolean;
begin
  Result := Pos(AnsiUpperCase(ExtractFileExt(prFileName)), AnsiUpperCase('.ckl')) <> 0;
end;

function TJupiterEnviroment.IsExecutableFile(prFileName: String): Boolean;
begin
  Result := Pos(AnsiUpperCase(ExtractFileExt(prFileName)), AnsiUpperCase('.exe')) <> 0;
end;

function TJupiterEnviroment.CanChangeFileContent(prFileName: String): Boolean;
begin
  Result := Pos(AnsiUpperCase(ExtractFileExt(prFileName)), AnsiUpperCase(vrJupiterApp.Params.VariableById('Enviroment.DoNotChangeContentExtensions').Value)) = 0;
end;

procedure TJupiterEnviroment.CopyFileTo(prOrigin, prDestiny: String);
begin
  CopyFile(prOrigin, prDestiny);
end;

function TJupiterEnviroment.IsOfExtension(prFileName, prGroupOfExtensions: String): Boolean;
begin
  Result := Pos(AnsiUpperCase(ExtractFileExt(prFileName)), AnsiUpperCase(prGroupOfExtensions)) <> 0;
end;

function TJupiterEnviroment.OpenFile(prDefaultExtensions: String): String;
var
  vrDialog : TOpenDialog;
begin
  Result := EmptyStr;

  vrDialog := TOpenDialog.Create(Application.MainForm);
  try
    vrDialog.InitialDir := Self.BasePath;

    if Trim(prDefaultExtensions) <> EmptyStr then
        vrDialog.Filter := Format('Arquivos com extensão %0:s|%0:s|', [prDefaultExtensions]);

    vrDialog.Filter := vrDialog.Filter + 'Todos os arquivos|*.*';

    if ((vrDialog.Execute) and (vrDialog.FileName <> EmptyStr)) then
    begin
      Result := vrDialog.FileName;

      if not FileExists(Result) then
        Result := EmptyStr;

      if Result <> EmptyStr then
        vrJupiterApp.AddMessage('Arquivo selecionado', Self.ClassName).Details.Add('Arquivo: ' + Result);
    end;
  finally
    FreeAndNil(vrDialog);
  end;
end;

end.

