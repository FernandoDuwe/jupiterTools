unit uJupiterAppScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, jupiterScript, JupiterConsts, SysUtils, PascalScript, uPSComponent;

type

  { TJupiterAppScript }

  TJupiterAppScript = class(TJupiterScriptLibrary)
  protected
    function Internal_GetName : String; override;
  public
    procedure DoCompile(prSender: TPSScript); override;
    function AnalyseCode: TJupiterScriptAnalyserList; override;
  end;

implementation

{ TJupiterAppScript }

function TJupiterAppScript.Internal_GetName: String;
begin
  Result := 'Jupiter.AppScript';
end;

procedure TJupiterAppScript.DoCompile(prSender: TPSScript);
begin
  inherited DoCompile(prSender);
end;

function TJupiterAppScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result := inherited AnalyseCode;
end;

end.

