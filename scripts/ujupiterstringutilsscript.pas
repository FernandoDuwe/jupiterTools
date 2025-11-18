unit uJupiterStringUtilsScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, jupiterScript, JupiterConsts, JupiterVariable, jupiterStringUtils,
  SysUtils, PascalScript, uPSComponent;

type

  { TJupiterStringUtilsScript }

  TJupiterStringUtilsScript = class(TJupiterScriptLibrary)
  protected
    function Internal_GetName : String; override;
  public
    procedure DoCompile(prSender: TPSScript); override;
    function AnalyseCode: TJupiterScriptAnalyserList; override;
  end;

  function JupiterStringUtilsScript_GenerateGUID : String;
  function JupiterStringUtilsScript_GetCSVColumn(prLine : String; prIndex : Integer) : String;
  function JupiterStringUtilsScript_GetCSVColumnSeparator(prLine : String; prIndex : Integer; prSeparator : String) : String;
  function JupiterStringUtilsScript_Replace(prStr, prOldString, prNewString : String) : String;
  function JupiterStringUtilsScript_CountCharInString(prChar, prFullString : String) : Integer;
  function JupiterStringUtilsScript_GetNextWord(prWord, prText : String) : String;

implementation

function JupiterStringUtilsScript_GenerateGUID: String;
begin
  Result := JupiterStringUtilsScript_GenerateGUID;
end;

function JupiterStringUtilsScript_GetCSVColumn(prLine: String; prIndex: Integer): String;
begin
  Result := JupiterStringUtilsGetCSVColumn(prLine, prIndex);
end;

function JupiterStringUtilsScript_GetCSVColumnSeparator(prLine: String; prIndex: Integer; prSeparator : String): String;
begin
  Result := JupiterStringUtilsGetCSVColumn(JupiterStringUtilsScript_Replace(prLine, prSeparator, ';'), prIndex);
end;

function JupiterStringUtilsScript_Replace(prStr, prOldString, prNewString: String): String;
begin
  Result := StringReplace(prStr, prOldString, prNewString, [rfIgnoreCase, rfReplaceAll]);
end;

function JupiterStringUtilsScript_CountCharInString(prChar, prFullString: String): Integer;
var
  vrVez : Integer;
begin
  Result := 0;

  for vrVez := 1 to Length(prFullString) do
    if prFullString[vrVez] = prChar then
      Result := Result + 1;
end;

function JupiterStringUtilsScript_GetNextWord(prWord, prText: String): String;
var
  vrStr   : TStrings;
  vrIndex : Integer;
begin
  Result := EmptyStr;

  if prWord = prText then
    Exit;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Delimiter     := '|';
    vrStr.DelimitedText := StringReplace(prText, ' ', '|', [rfReplaceAll, rfIgnoreCase]);

    vrIndex := vrStr.IndexOf(prWord);

    if vrIndex = NULL_KEY then
      Exit;

    if vrIndex > (vrStr.Count - 1) then
      Exit;

    Result := vrStr[vrIndex + 1];
  finally
    FreeAndNil(vrStr);
  end;
end;

{ TJupiterStringUtilsScript }

function TJupiterStringUtilsScript.Internal_GetName: String;
begin
  Result := 'Jupiter.StringUtilsScript';
end;

procedure TJupiterStringUtilsScript.DoCompile(prSender: TPSScript);
begin
  inherited DoCompile(prSender);

  prSender.AddFunction(@JupiterStringUtilsScript_GenerateGUID, 'function GenerateGUID : String;');
  prSender.AddFunction(@JupiterStringUtilsScript_GetCSVColumn, 'function GetCSVColumn(prLine: String; prIndex: Integer): String;');
  prSender.AddFunction(@JupiterStringUtilsScript_GetCSVColumnSeparator, 'function GetCSVColumnSeparator(prLine: String; prIndex: Integer; prSeparator : String): String;');
  prSender.AddFunction(@JupiterStringUtilsScript_Replace, 'function Replace(prStr, prOldString, prNewString: String): String;');
end;

function TJupiterStringUtilsScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result := inherited AnalyseCode;

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function GenerateGUID : String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function GetCSVColumn(prLine: String; prIndex: Integer): String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function GetCSVColumnSeparator(prLine: String; prIndex: Integer; prSeparator : String): String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function Replace(prStr, prOldString, prNewString: String): String;'));
end;

end.

