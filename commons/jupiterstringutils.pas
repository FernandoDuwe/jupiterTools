unit jupiterStringUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

  function JupiterStringUtilsNormalizeToPresent(prText : String) : String;

  function JupiterStringUtilsGenerateGUID : String;

  function JupiterStringUtilsStringToStringList(prScript : String) : TStrings;

implementation

function JupiterStringUtilsNormalizeToPresent(prText: String): String;
begin
  if prText = EmptyStr then
    Result := prText;

  prText    := AnsiLowerCase(prText);
  prText[1] := AnsiUpperCase(prText)[1];

  Result := prText;
end;

function JupiterStringUtilsGenerateGUID: String;
var
  vrGUID : TGuid;
begin
  Result := EmptyStr;

  if CreateGUID(vrGUID) = 0 then
    Result := GUIDToString(vrGUID);
end;

function JupiterStringUtilsStringToStringList(prScript: String): TStrings;
var
  vrVez : Integer;
  vrStrAux : String;
begin
  Result := TStringList.Create;
  Result.Clear;

  vrStrAux := EmptyStr;

  for vrVez := 1 to Length(prScript) do
  begin
    vrStrAux := vrStrAux + prScript[vrVez];

    if prScript[vrVez] = ';' then
    begin
      Result.Add(vrStrAux);

      vrStrAux := EmptyStr;
    end;
  end;

  Result.Add(vrStrAux);
end;

end.

