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

implementation

function JupiterStringUtilsScript_GenerateGUID: String;
begin
  Result := JupiterStringUtilsScript_GenerateGUID;
end;

function JupiterStringUtilsScript_GetCSVColumn(prLine: String; prIndex: Integer): String;
begin
  Result := JupiterStringUtilsScript_GetCSVColumn(prLine, prIndex);
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
end;

function TJupiterStringUtilsScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result := inherited AnalyseCode;

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function GenerateGUID : String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function GetCSVColumn(prLine: String; prIndex: Integer): String;'));
end;

end.

