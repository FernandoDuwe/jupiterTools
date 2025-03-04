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

  function JupiterAppScript_GetVersion : String;
  function JupiterAppScript_GetBasePath : String;
  function JupiterAppScript_ParamExists(prScriptId, prId : String) : Boolean;
  function JupiterAppScript_ParamCount(prScriptId : String) : Integer;
  function JupiterAppScript_GetParamNameByIndex(prScriptId : String; prIndex : Integer) : String;
  function JupiterAppScript_GetParam(prScriptId, prId : String) : String;
  procedure JupiterAppScript_SetParam(prScriptId, prId, prValue : String);
  function JupiterAppScript_GlobalParamExists(prId : String) : Boolean;
  function JupiterAppScript_GetGlobalParam(prId : String) : String;
  procedure JupiterAppScript_SetGlobalParam(prId, prValue : String);
  function JupiterAppScript_ResolveGlobal(prValue : String) : String;
  function JupiterAppScript_Resolve(prScriptId, prValue : String) : String;
  procedure JupiterAppScript_WriteLn(prMessage : String);
  procedure JupiterAppScript_WriteScriptLn(prScriptId, prMessage : String);
  procedure JupiterAppScript_RunMacroById(prMacroId : String);
  procedure JupiterAppScript_RunMacroByIdInThread(prMacroId : String);
  procedure JupiterAppScript_RunMacroByIdInThreadWithParams(prMacroId, prParams : String);
  procedure JupiterAppScript_RunMacroByIdWithParams(prMacroId, prParams : String);
  procedure JupiterAppScript_RunMacroByFile(prMacroFile : String);
  procedure JupiterAppScript_RunMacroByFileInThread(prMacroFile : String);


implementation

function JupiterAppScript_GetVersion: String;
begin
  Result := vrJupiterApp.GetVersion;
end;

function JupiterAppScript_GetBasePath: String;
begin
  Result := GetRootDirectory;
end;

function JupiterAppScript_ParamExists(prScriptId, prId: String): Boolean;
begin
  Result := vrJupiterApp.GetScriptById(prScriptId).Params.Exists(prId);
end;

function JupiterAppScript_ParamCount(prScriptId : String): Integer;
begin
  Result := vrJupiterApp.GetScriptById(prScriptId).Params.Count;
end;

function JupiterAppScript_GetParamNameByIndex(prScriptId : String; prIndex : Integer): String;
begin
  Result := vrJupiterApp.GetScriptById(prScriptId).Params.VariableByIndex(prIndex).ID;
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

function JupiterAppScript_GlobalParamExists(prId: String): Boolean;
begin
  Result := vrJupiterApp.Params.Exists(prId);
end;

function JupiterAppScript_GetGlobalParam(prId: String): String;
begin
  Result := vrJupiterApp.Params.VariableById(prId).Value;
end;

procedure JupiterAppScript_SetGlobalParam(prId, prValue: String);
begin
  if JupiterAppScript_GlobalParamExists(prId) then
    vrJupiterApp.Params.VariableById(prId).Value := prValue
  else
    vrJupiterApp.Params.AddConfig(prId, prValue, prId);
end;

function JupiterAppScript_ResolveGlobal(prValue: String): String;
begin
  Result := vrJupiterApp.Params.ResolveString(prValue);
end;

function JupiterAppScript_Resolve(prScriptId, prValue: String): String;
begin
  Result := vrJupiterApp.GetScriptById(prScriptId).Params.ResolveString(prValue);
end;

procedure JupiterAppScript_WriteLn(prMessage: String);
begin
  vrJupiterApp.AddMessage('Mensagem de script', prMessage, 'WriteLn');

  WriteLn(prMessage);
end;

procedure JupiterAppScript_WriteScriptLn(prScriptId, prMessage: String);
begin
  if vrJupiterApp.GetScriptById(prScriptId) <> nil then
    vrJupiterApp.GetScriptById(prScriptId).AddMessage(prMessage);
end;

procedure JupiterAppScript_RunMacroById(prMacroId: String);
begin
  vrJupiterApp.RunMacro(prMacroId, TJupiterVariableList.Create);
end;

procedure JupiterAppScript_RunMacroByIdInThread(prMacroId: String);
begin
  vrJupiterApp.RunMacroInThread(prMacroId, TJupiterVariableList.Create);
end;

procedure JupiterAppScript_RunMacroByIdInThreadWithParams(prMacroId, prParams: String);
var
  vrParams : TJupiterVariableList;
begin
  vrParams := TJupiterVariableList.Create;
  vrParams.AddVariable('Params', prParams);

  vrJupiterApp.RunMacroInThread(prMacroId, vrParams);
end;

procedure JupiterAppScript_RunMacroByIdWithParams(prMacroId, prParams: String);
var
  vrParams : TJupiterVariableList;
begin
  vrParams := TJupiterVariableList.Create;
  vrParams.AddVariable('Params', prParams);

  vrJupiterApp.RunMacro(prMacroId, vrParams);
end;

procedure JupiterAppScript_RunMacroByFile(prMacroFile: String);
begin
  vrJupiterApp.RunMacroFromFile(prMacroFile, TJupiterVariableList.Create);
end;

procedure JupiterAppScript_RunMacroByFileInThread(prMacroFile: String);
begin
  vrJupiterApp.RunMacroFromFileInThread(prMacroFile, TJupiterVariableList.Create);
end;

{ TJupiterAppScript }

function TJupiterAppScript.Internal_GetName: String;
begin
  Result := 'Jupiter.AppScript';
end;

procedure TJupiterAppScript.DoCompile(prSender: TPSScript);
begin
  inherited DoCompile(prSender);

  prSender.AddFunction(@JupiterAppScript_GetVersion, 'function GetVersion : String;');
  prSender.AddFunction(@JupiterAppScript_GetBasePath, 'function GetBasePath : String;');
  prSender.AddFunction(@JupiterAppScript_ParamExists, 'function ParamExists(prScriptId, prId : String) : Boolean;');
  prSender.AddFunction(@JupiterAppScript_GetParam, 'function GetParam(prScriptId, prId : String) : String;');
  prSender.AddFunction(@JupiterAppScript_SetParam, 'procedure SetParam(prScriptId, prId, prValue : String);');
  prSender.AddFunction(@JupiterAppScript_ParamCount, 'function ParamCount(prScriptId : String) : Integer;');

  prSender.AddFunction(@JupiterAppScript_GetParamNameByIndex, 'function GetParamNameByIndex(prScriptId : String; prIndex : Integer): String;');

  prSender.AddFunction(@JupiterAppScript_GlobalParamExists, 'function GlobalParamExists(prId : String) : Boolean;');
  prSender.AddFunction(@JupiterAppScript_GetGlobalParam, 'function GetGlobalParam(prId : String) : String;');
  prSender.AddFunction(@JupiterAppScript_SetGlobalParam, 'procedure SetGlobalParam(prId, prValue : String);');

  prSender.AddFunction(@JupiterAppScript_ResolveGlobal, 'function ResolveGlobal(prValue : String) : String;');
  prSender.AddFunction(@JupiterAppScript_Resolve, 'function Resolve(prScriptId, prValue : String) : String;');

  prSender.AddFunction(@JupiterAppScript_WriteLn, 'procedure Writeln(prMessage: String);');
  prSender.AddFunction(@JupiterAppScript_WriteScriptLn, 'procedure WriteScriptLn(prScriptId, prMessage: String);');

  prSender.AddFunction(@JupiterAppScript_RunMacroByIdInThread, 'procedure RunMacroByIdInThread(prMacroId: String);');
  prSender.AddFunction(@JupiterAppScript_RunMacroById, 'procedure RunMacroById(prMacroId: String);');

  prSender.AddFunction(@JupiterAppScript_RunMacroByIdWithParams, 'procedure RunMacroByIdWithParams(prMacroId, prParams : String);');
  prSender.AddFunction(@JupiterAppScript_RunMacroByIdInThreadWithParams, 'procedure RunMacroByIdInThreadWithParams(prMacroId, prParams : String);');

  prSender.AddFunction(@JupiterAppScript_RunMacroByFile, 'procedure RunMacroByFile(prMacroFile: String);');
  prSender.AddFunction(@JupiterAppScript_RunMacroByFileInThread, 'procedure RunMacroByFileInThread(prMacroFile: String);');

end;

function TJupiterAppScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result := inherited AnalyseCode;

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function GetVersion : String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function GetBasePath : String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function ParamExists(prScriptId, prId : String) : Boolean;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function GetParam(prScriptId, prId : String) : String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function GlobalParamExists(prId : String) : Boolean;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function GetGlobalParam(prId : String) : String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function ParamCount(prScriptId : String) : Integer;'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function GetParamNameByIndex(prScriptId : String; prIndex : Integer): String;'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function ResolveGlobal(prValue : String) : String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function Resolve(prScriptId, prValue : String) : String;'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure SetParam(prScriptId, prId, prValue : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure SetGlobalParam(prId, prValue : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Writeln(prMessage: String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure WriteScriptLn(prScriptId, prMessage: String);'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure RunMacroById(prMacroId: String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure RunMacroByIdWithParams(prMacroId: String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure RunMacroByIdInThread(prMacroId : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure RunMacroByIdInThreadWithParams(prMacroId, prParams : String);'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure RunMacroByFile(prMacroFile: String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure RunMacroByFileInThread(prMacroFile: String);'));
end;

end.

