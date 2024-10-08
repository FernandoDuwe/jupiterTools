unit uJupiterStringUtilsScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, jupiterScript, JupiterConsts, JupiterVariable, SysUtils,
  PascalScript, uPSComponent;

type

  { TJupiterStringUtilsScript }

  TJupiterStringUtilsScript = class(TJupiterScriptLibrary)
  protected
    function Internal_GetName : String; override;
  public
    procedure DoCompile(prSender: TPSScript); override;
    function AnalyseCode: TJupiterScriptAnalyserList; override;
  end;

implementation

{ TJupiterStringUtilsScript }

function TJupiterStringUtilsScript.Internal_GetName: String;
begin
  Result := 'Jupiter.StringUtilsScript';
end;

procedure TJupiterStringUtilsScript.DoCompile(prSender: TPSScript);
begin
  inherited DoCompile(prSender);
end;

function TJupiterStringUtilsScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result:=inherited AnalyseCode;
end;

end.

