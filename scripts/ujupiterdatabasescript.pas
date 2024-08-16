unit uJupiterDatabaseScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, jupiterScript, JupiterConsts, JupiterApp, SysUtils, PascalScript, uPSComponent, Forms, SQLDB;

type

  { TuJupiterDatabaseScript }

  TuJupiterDatabaseScript = class(TJupiterScriptLibrary)
  protected
    function Internal_GetName : String; override;
  public
    procedure DoCompile(prSender: TPSScript); override;
    function AnalyseCode: TJupiterScriptAnalyserList; override;
  end;

implementation

{ TuJupiterDatabaseScript }

function TuJupiterDatabaseScript.Internal_GetName: String;
begin
  Result := 'Jupiter.DatabaseScript';
end;

procedure TuJupiterDatabaseScript.DoCompile(prSender: TPSScript);
begin
  inherited DoCompile(prSender);
end;

function TuJupiterDatabaseScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result:=inherited AnalyseCode;
end;

end.

