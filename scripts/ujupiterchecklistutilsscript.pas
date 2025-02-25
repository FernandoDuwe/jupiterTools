unit uJupiterCheckListUtilsScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, jupiterScript, JupiterConsts, JupiterVariable, jupiterStringUtils,
  uJupiterStringUtilsScript, SysUtils, PascalScript, uPSComponent, SQLDB;

type

  { TJupiterCheckListUtilsScript }

  TJupiterCheckListUtilsScript = class(TJupiterScriptLibrary)
  protected
    function Internal_GetName : String; override;
  public
    procedure DoCompile(prSender: TPSScript); override;
    function AnalyseCode: TJupiterScriptAnalyserList; override;
  end;

  function JupiterCheckListUtilsScript_CheckListCompleta(prFileName : String) : Boolean;

implementation

function JupiterCheckListUtilsScript_CheckListCompleta(prFileName: String): Boolean;
var
  vrVez : Integer;
  vrStr : TStrings;
begin
  Result := True;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.LoadFromFile(prFileName);

    for vrVez := 1 to vrStr.Count - 1 do
    begin
      if Trim(vrStr[vrVez]) = EmptyStr then
        Continue;

      if not JupiterStringUtilsStrToBool(JupiterStringUtilsGetCSVColumn(vrStr[vrVez], 1)) then
      begin
        Result := False;
        Exit;
      end;
    end;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

{ TJupiterCheckListUtilsScript }

function TJupiterCheckListUtilsScript.Internal_GetName: String;
begin
  Result := 'Jupiter.CheckListUtilsScript';
end;

procedure TJupiterCheckListUtilsScript.DoCompile(prSender: TPSScript);
begin
  inherited DoCompile(prSender);

  prSender.AddFunction(@JupiterCheckListUtilsScript_CheckListCompleta, 'function CheckListCompleta(prFileName: String): Boolean;');
end;

function TJupiterCheckListUtilsScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result := inherited AnalyseCode;

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function CheckListCompleta(prFileName: String): Boolean;'));
end;

end.

