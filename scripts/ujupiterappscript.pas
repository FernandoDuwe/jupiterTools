unit uJupiterAppScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, jupiterScript, JupiterConsts, JupiterVariable, JupiterApp, SysUtils,
  PascalScript, uPSComponent;

type

  { TJupiterAppScript }

  TJupiterAppScript = class(TJupiterScriptLibrary)
  protected
    function Internal_GetName : String; override;
  public
    procedure DoCompile(prSender: TPSScript); override;
    function AnalyseCode: TJupiterScriptAnalyserList; override;
  end;

  function JupiterAppScript_ParamExists(prScriptId, prId : String) : Boolean;
  function JupiterAppScript_GetParam(prScriptId, prId : String) : String;
  procedure JupiterAppScript_SetParam(prScriptId, prId, prValue : String);

implementation

function JupiterAppScript_ParamExists(prScriptId, prId: String): Boolean;
begin
  Result := vrJupiterApp.GetScriptById(prScriptId).Params.Exists(prId);
end;

function JupiterAppScript_GetParam(prScriptId, prId: String): String;
begin
  Result := vrJupiterApp.GetScriptById(prScriptId).Params.VariableById(prId).Value;
end;

procedure JupiterAppScript_SetParam(prScriptId, prId, prValue: String);
begin
  if JupiterAppScript_ParamExists(prScriptId, prId) then
    vrJupiterApp.GetScriptById(prScriptId).Params.VariableById(prId).Value := prValue
  else
    vrJupiterApp.GetScriptById(prScriptId).Params.AddConfig(prId, prValue, prId);
end;

{ TJupiterAppScript }

function TJupiterAppScript.Internal_GetName: String;
begin
  Result := 'Jupiter.AppScript';
end;

procedure TJupiterAppScript.DoCompile(prSender: TPSScript);
begin
  inherited DoCompile(prSender);

  prSender.AddFunction(@JupiterAppScript_ParamExists, 'function ParamExists(prScriptId, prId : String) : Boolean;');
  prSender.AddFunction(@JupiterAppScript_GetParam, 'function GetParam(prScriptId, prId : String) : String;');
  prSender.AddFunction(@JupiterAppScript_SetParam, 'procedure SetParam(prScriptId, prId, prValue : String);');
end;

function TJupiterAppScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result := inherited AnalyseCode;

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function ParamExists(prScriptId, prId : String) : Boolean;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function GetParam(prScriptId, prId : String) : String;'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure SetParam(prScriptId, prId, prValue : String);'));
end;

end.

