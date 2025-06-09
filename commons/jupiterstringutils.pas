unit jupiterStringUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterConsts, JupiterObject;

  function JupiterStringUtilsNormalizeToPresent(prText : String) : String;

  function JupiterStringUtilsGenerateGUID : String;

  function JupiterStringUtilsStringToStringList(prScript : String) : TStrings;

  function JupiterStringUtilsGetCSVColumn(prLine : String; prIndex : Integer) : String;

  function JupiterStringUtilsGetCountColumns(prLine : String; prCharacterSeparator : Char) : Integer;

  function JupiterStringUtilsBoolToStr(prValue : Boolean) : String;

  function JupiterStringUtilsStrToBool(prValue : String) : Boolean;

  function jupiterStringUtilsGetLastPathName(prPath : String) : String;

  function jupiterStringUtilsGetDatabaseTriggerName(prTrigger, prTable : String) : String;

  function jupiterStringUtilsIsValidSearch(prText, prSearchText : String) : Boolean;

  function jupiterStringUtilsIsSameRootRoute(prRoot, prRoute : String) : Boolean;

  function jupiterStringUtilsStringListToString(prStringList : TStrings; prSeparator : String = ', ') : String;

type

  { TJupiterStringReference }

  TJupiterStringReference = class(TJupiterObject)
  private
    FReference : String;
  published
    property Reference : String  read FReference write FReference;
  public
    constructor Create(prReference : String);
  end;

implementation

function JupiterStringUtilsNormalizeToPresent(prText: String): String;
begin
  if prText = EmptyStr then
    Result := prText;

  prText    := AnsiLowerCase(prText);
  prText[1] := AnsiUpperCase(prText)[1];
  prText    := StringReplace(prText, '_', ' ', [rfIgnoreCase, rfReplaceAll]);

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

function JupiterStringUtilsGetCSVColumn(prLine: String; prIndex: Integer): String;
var
  vrStr : TStrings;
begin
  Result := EmptyStr;

  if Trim(prLine) = EmptyStr then
    Exit;

  vrStr := TStringList.Create;
  try
    vrStr.Delimiter     := ';';
    vrStr.DelimitedText := StringReplace(prLine, ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]);

    if (vrStr.Count - 1) < prIndex then
      Exit;

    Result := vrStr[prIndex];
    Result := StringReplace(Result, EMPTY_SPACE_SEPARATOR, ' ', [rfIgnoreCase, rfReplaceAll]);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

function JupiterStringUtilsGetCountColumns(prLine: String; prCharacterSeparator: Char): Integer;
var
  vrStr : TStrings;
begin
  Result := 0;

  if Trim(prLine) = EmptyStr then
    Exit;

  vrStr := TStringList.Create;
  try
    vrStr.Delimiter     := prCharacterSeparator;
    vrStr.DelimitedText := StringReplace(prLine, ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]);

    Result := vrStr.Count;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

function JupiterStringUtilsBoolToStr(prValue: Boolean): String;
begin
  Result := BOOL_FALSE_STR;

  if prValue then
    Result := BOOL_TRUE_STR;
end;

function JupiterStringUtilsStrToBool(prValue: String): Boolean;
begin
  Result := False;

  if prValue = '1' then
  begin
    Result := True;
    Exit;
  end;

  if prValue = BOOL_TRUE_STR then
  begin
    Result := True;
    Exit;
  end;
end;

function jupiterStringUtilsGetLastPathName(prPath: String): String;
var
  vrTest : String;
begin
  prPath := StringReplace(prPath, '/', ';', [rfIgnoreCase, rfReplaceAll]);
  prPath := StringReplace(prPath, '\', ';', [rfIgnoreCase, rfReplaceAll]);

  if Copy(prPath, Length(prPath), 1) = ';' then
    prPath := Copy(prPath, 1, Length(prPath) - 1);

  Result := JupiterStringUtilsGetCSVColumn(prPath, JupiterStringUtilsGetCountColumns(prPath, ';') - 1);
end;

function jupiterStringUtilsGetDatabaseTriggerName(prTrigger, prTable: String): String;
begin
  Result := StringReplace(prTrigger, '{0}', prTable, [rfIgnoreCase, rfReplaceAll]);
end;

function jupiterStringUtilsIsValidSearch(prText, prSearchText: String): Boolean;
begin
  Result := Pos(AnsiUpperCase(prSearchText), AnsiUpperCase(prText)) > 0;
end;

function jupiterStringUtilsIsSameRootRoute(prRoot, prRoute: String): Boolean;
begin
  if Length(prRoot) > Length(prRoute) then
  begin
    Result := False;
    Exit;
  end;

  Result := (prRoot = Copy(prRoute, 1, Length(prRoot)));
end;

function jupiterStringUtilsStringListToString(prStringList: TStrings; prSeparator: String): String;
var
  vrVez : Integer;
begin
  Result := EmptyStr;

  for vrVez := 0 to prStringList.Count - 1 do
  begin
    if vrVez > 0 then
      Result := Result + ', ';

    Result := Result + prStringList[vrVez];
  end;
end;

{ TJupiterStringReference }

constructor TJupiterStringReference.Create(prReference: String);
begin
  Self.Reference := prReference;
end;

end.

