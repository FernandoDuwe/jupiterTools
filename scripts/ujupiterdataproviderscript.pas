unit uJupiterDataProviderScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, jupiterScript, JupiterConsts, jupiterDatabaseWizard,
  SysUtils, PascalScript, uPSComponent, Forms, SQLDB;

type

  { TJupiterDataProviderScript }

  TJupiterDataProviderScript = class(TJupiterScriptLibrary)
  protected
    function Internal_GetName : String; override;
  public
    procedure DoCompile(prSender: TPSScript); override;
    function AnalyseCode: TJupiterScriptAnalyserList; override;
  end;

  // Construtores
  function JupiterDataProviderScript_DataProviderNewCSV(prFileName : String) : String;
  function JupiterDataProviderScript_DataProviderNewPaths(prPath : String; prSubFolders : Boolean) : String;
  function JupiterDataProviderScript_DataProviderNewFiles(prPath : String; prSubFolders : Boolean) : String;

  function JupiterDataProviderScript_DataProviderGetCount(prProviderID : String) : Integer;
  function JupiterDataProviderScript_DataProviderGetField(prProviderID, prFieldName : String; prLine : Integer) : String;
  function JupiterDataProviderScript_DataProviderSetField(prProviderID, prFieldName, prValue : String; prLine : Integer) : String;
  function JupiterDataProviderScript_DataProviderFieldExists(prProviderID, prFieldName : String; prLine : Integer) : Boolean;
  procedure JupiterDataProviderScript_DataProviderDestroy(prProviderID : String);

implementation

uses JupiterApp, JupiterDataProvider;

function JupiterDataProviderScript_DataProviderNewCSV(prFileName: String): String;
var
  vrProvider : TJupiterDataProvider;
begin
  vrProvider := FactoryDataProvider(DATAPROVIDER_TYPE_LIST_CSV, prFileName, True);

  Result := vrProvider.ProviderID;
end;

function JupiterDataProviderScript_DataProviderNewPaths(prPath: String; prSubFolders: Boolean): String;
var
  vrProvider : TJupiterDataProvider;
begin
  vrProvider := FactoryDataProvider(DATAPROVIDER_TYPE_LIST_PATHS, prPath, prSubFolders);

  Result := vrProvider.ProviderID;
end;

function JupiterDataProviderScript_DataProviderNewFiles(prPath: String; prSubFolders: Boolean): String;
var
  vrProvider : TJupiterDataProvider;
begin
  vrProvider := FactoryDataProvider(DATAPROVIDER_TYPE_LIST_FILES, prPath, prSubFolders);

  Result := vrProvider.ProviderID;
end;

function JupiterDataProviderScript_DataProviderGetCount(prProviderID: String): Integer;
begin
  Result := vrJupiterApp.GetDataProviderById(prProviderID).Count;
end;

function JupiterDataProviderScript_DataProviderGetField(prProviderID, prFieldName : String; prLine : Integer): String;
begin
  Result := vrJupiterApp.GetDataProviderById(prProviderID).GetRowByIndex(prLine).Fields.VariableById(prFieldName).Value;
end;

function JupiterDataProviderScript_DataProviderSetField(prProviderID, prFieldName, prValue: String; prLine: Integer): String;
begin
  if vrJupiterApp.GetDataProviderById(prProviderID).GetRowByIndex(prLine).Fields.Exists(prFieldName) then
    vrJupiterApp.GetDataProviderById(prProviderID).GetRowByIndex(prLine).Fields.VariableById(prFieldName).Value := prValue
  else
    vrJupiterApp.GetDataProviderById(prProviderID).GetRowByIndex(prLine).Fields.AddVariable(prFieldName, prValue);
end;

function JupiterDataProviderScript_DataProviderFieldExists(prProviderID, prFieldName: String; prLine: Integer): Boolean;
begin
  Result := vrJupiterApp.GetDataProviderById(prProviderID).GetRowByIndex(prLine).Fields.Exists(prFieldName);
end;

procedure JupiterDataProviderScript_DataProviderDestroy(prProviderID : String);
begin
  vrJupiterApp.GetDataProviderById(prProviderID).Free;
end;

{ TJupiterDataProviderScript }

function TJupiterDataProviderScript.Internal_GetName: String;
begin
  Result := 'Jupiter.DataProviderScript';
end;

procedure TJupiterDataProviderScript.DoCompile(prSender: TPSScript);
begin
  inherited DoCompile(prSender);

  prSender.AddFunction(@JupiterDataProviderScript_DataProviderNewCSV, 'function DataProviderNewCSV(prFileName : String) : String;');
  prSender.AddFunction(@JupiterDataProviderScript_DataProviderNewPaths, 'function DataProviderNewPath(prPath : String; prSubFolders : Boolean) : String;');
  prSender.AddFunction(@JupiterDataProviderScript_DataProviderNewFiles, 'function DataProviderNewFile(prPath : String; prSubFolders : Boolean) : String;');

  prSender.AddFunction(@JupiterDataProviderScript_DataProviderGetCount, 'function DataProviderGetCount(prProviderID: String) : Integer;');
  prSender.AddFunction(@JupiterDataProviderScript_DataProviderGetField, 'function DataProviderGetField(prProviderID, prFieldName : String; prLine : Integer) : String;');
  prSender.AddFunction(@JupiterDataProviderScript_DataProviderSetField, 'function DataProviderSetField(prProviderID, prFieldName, prValue: String; prLine: Integer): String;');
  prSender.AddFunction(@JupiterDataProviderScript_DataProviderFieldExists, 'function DataProviderFieldExists(prProviderID, prFieldName: String; prLine: Integer): Boolean;');

  prSender.AddFunction(@JupiterDataProviderScript_DataProviderDestroy, 'procedure DataProviderDestroy(prProviderID : String);');
end;

function TJupiterDataProviderScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result := inherited AnalyseCode;

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function DataProviderNewCSV(prFileName : String) : String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function DataProviderNewPath(prPath : String; prSubFolders : Boolean) : String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function DataProviderNewFile(prPath : String; prSubFolders : Boolean) : String;'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function DataProviderGetCount(prProviderID: String) : Integer;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function DataProviderGetField(prProviderID, prFieldName : String; prLine : Integer) : String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function DataProviderSetField(prProviderID, prFieldName, prValue: String; prLine: Integer): String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function DataProviderFieldExists(prProviderID, prFieldName: String; prLine: Integer): Boolean;'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure DataProviderDestroy(prProviderID : String);'));
end;

end.

