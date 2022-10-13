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
    function FullPath(prPath : String) : String;
  end;

implementation

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
     CreateDir(Result);
end;

function TJupiterEnviroment.FullPath(prPath: String): String;
begin
  prPath := StringReplace(prPath, '\', GetDirectorySeparator, [rfIgnoreCase, rfReplaceAll]);
  prPath := StringReplace(prPath, '/', GetDirectorySeparator, [rfIgnoreCase, rfReplaceAll]);

  Result := Self.Internal_GetBasePath + prPath;
end;

end.

