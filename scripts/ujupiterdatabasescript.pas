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
  procedure JupiterDatabaseScript_RunScriptWithoutTransaction(prScript : String);
  procedure JupiterDatabaseScript_StartTransaction;
  procedure JupiterDatabaseScript_CommitTransaction;
  procedure JupiterDatabaseScript_RollbackTransaction;
  function JupiterDatabaseScript_Resolve(prTable, prField, prWhere : String) : String;
  function JupiterDatabaseScript_Count(prTable, prWhere : String) : Integer;

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

procedure JupiterDatabaseScript_RunScriptWithoutTransaction(prScript: String);
var
  vrWizard : TJupiterDatabaseWizard;
begin
  vrWizard := vrJupiterApp.NewWizard;
  try
    vrWizard.ExecuteScript(CreateStringList(prScript), False);
  finally
    FreeAndNil(vrWizard);
  end;
end;

procedure JupiterDatabaseScript_StartTransaction;
var
  vrWizard : TJupiterDatabaseWizard;
begin
  vrWizard := vrJupiterApp.NewWizard;
  try
    vrWizard.StartTransaction;
  finally
    FreeAndNil(vrWizard);
  end;
end;

procedure JupiterDatabaseScript_CommitTransaction;
var
  vrWizard : TJupiterDatabaseWizard;
begin
  vrWizard := vrJupiterApp.NewWizard;
  try
    vrWizard.Commit;
  finally
    FreeAndNil(vrWizard);
  end;
end;

procedure JupiterDatabaseScript_RollbackTransaction;
var
  vrWizard : TJupiterDatabaseWizard;
begin
  vrWizard := vrJupiterApp.NewWizard;
  try
    vrWizard.Rollback;
  finally
    FreeAndNil(vrWizard);
  end;
end;

function JupiterDatabaseScript_Resolve(prTable, prField, prWhere: String): String;
var
  vrWizard : TJupiterDatabaseWizard;
begin
  vrWizard := vrJupiterApp.NewWizard;
  try
    Result := vrWizard.Resolve(prTable, prField, prWhere);
  finally
    FreeAndNil(vrWizard);
  end;
end;

function JupiterDatabaseScript_Count(prTable, prWhere: String): Integer;
var
  vrWizard : TJupiterDatabaseWizard;
begin
  vrWizard := vrJupiterApp.NewWizard;
  try
    Result := vrWizard.Count(prTable, prWhere);
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

  prSender.AddFunction(@JupiterDatabaseScript_Resolve, 'function DBResolve(prTable, prField, prWhere : String) : String;');
  prSender.AddFunction(@JupiterDatabaseScript_Count, 'function DBCount(prTable, prWhere: String): Integer;');

  prSender.AddFunction(@JupiterDatabaseScript_Exists, 'function DBExists(prTable, prWhere : String) : Boolean;');

  prSender.AddFunction(@JupiterDatabaseScript_RunScript, 'procedure DBRunScript(prScript : String);');

  prSender.AddFunction(@JupiterDatabaseScript_StartTransaction, 'procedure DBStartTransaction;');
  prSender.AddFunction(@JupiterDatabaseScript_CommitTransaction, 'procedure DBCommitTransaction;');
  prSender.AddFunction(@JupiterDatabaseScript_RollbackTransaction, 'procedure DBRollbackTransaction;');

  prSender.AddFunction(@JupiterDatabaseScript_RunScriptWithoutTransaction, 'procedure DBRunScriptWithoutTransaction(prScript : String);');
end;

function TuJupiterDatabaseScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result := inherited AnalyseCode;

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function DBExists(prTable, prWhere : String) : Boolean;'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure DBRunScript(prScript : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure DBRunScriptWithoutTransaction(prScript : String);'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure DBStartTransaction;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure DBCommitTransaction;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure DBRollbackTransaction;'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function DBResolve(prTable, prField, prWhere : String) : String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function DBCount(prTable, prWhere: String): Integer;'));
end;

end.

