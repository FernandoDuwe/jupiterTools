unit uJupiterDatabaseScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, jupiterScript, JupiterConsts, JupiterApp, jupiterDatabaseWizard,
  SysUtils, PascalScript, uPSComponent, Forms, SQLDB;

type

  { TuJupiterDatabaseScript }

  TuJupiterDatabaseScript = class(TJupiterScriptLibrary)
  protected
    function Internal_GetName : String; override;
  public
    procedure DoCompile(prSender: TPSScript); override;
    function AnalyseCode: TJupiterScriptAnalyserList; override;
  end;

  function JupiterDatabaseScript_Exists(prTable, prWhere : String) : Boolean;
  procedure JupiterDatabaseScript_RunScript(prScript : String);

implementation

function JupiterDatabaseScript_Exists(prTable, prWhere: String): Boolean;
var
  vrWizard : TJupiterDatabaseWizard;
begin
  Result := False;

  vrWizard := vrJupiterApp.NewWizard;
  try
    Result := vrWizard.Exists(prTable, prWhere);
  finally
    FreeAndNil(vrWizard);
  end;
end;

procedure JupiterDatabaseScript_RunScript(prScript: String);
var
  vrWizard : TJupiterDatabaseWizard;
begin
  vrWizard := vrJupiterApp.NewWizard;
  try
    vrWizard.ExecuteScript(CreateStringList(prScript));
  finally
    FreeAndNil(vrWizard);
  end;
end;

{ TuJupiterDatabaseScript }

function TuJupiterDatabaseScript.Internal_GetName: String;
begin
  Result := 'Jupiter.DatabaseScript';
end;

procedure TuJupiterDatabaseScript.DoCompile(prSender: TPSScript);
begin
  inherited DoCompile(prSender);

  prSender.AddFunction(@JupiterDatabaseScript_Exists, 'function DBExists(prTable, prWhere : String) : Boolean;');

  prSender.AddFunction(@JupiterDatabaseScript_RunScript, 'procedure DBRunScript(prScript : String);');
end;

function TuJupiterDatabaseScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result := inherited AnalyseCode;

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function DBExists(prTable, prWhere : String) : Boolean;'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure DBRunScript(prScript : String);'));
end;

end.

