unit JupiterGeneratorForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, JupiterConsts, JupiterEnviroment,
  JupiterVariable, JupiterAction, JupiterXMLDataProvider, JupiterRunnable,
  JupiterVariableForm;

type

  { TJupiterGeneratorForm }

  TJupiterGeneratorForm = class(TJupiterObject)
  private
    FFormID  : String;
    FFields  : TJupiterVariableFormList;
    FActions : TJupiterActionList;

    procedure Internal_SetFormID(prFormID : String);
    procedure Internal_ReadFileVariables(prCompleteFileName : String);
    procedure Internal_ReadFileActions(prCompleteFileName : String);
    procedure Internal_ReadFile(prCompleteFileName : String);

    function Internal_GetFileName : String;
  published
    property Actions : TJupiterActionList       read FActions write FActions;
    property Fields  : TJupiterVariableFormList read FFields  write FFields;
    property FormID  : String                   read FFormID  write Internal_SetFormID;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveFile;
  end;

implementation

{ TJupiterGeneratorForm }

procedure TJupiterGeneratorForm.Internal_SetFormID(prFormID: String);
var
  vrFileName   : String;
begin
  Self.FFormID := prFormID;

  vrFileName := Self.Internal_GetFileName;

  if not FileExists(vrFileName) then
    Self.SaveFile
  else
    Self.Internal_ReadFile(vrFileName);
end;

procedure TJupiterGeneratorForm.Internal_ReadFileVariables(prCompleteFileName: String);
var
  vrXML  : TJupiterXMLDataProvider;
  vrVez  : Integer;
begin
  vrXML := TJupiterXMLDataProvider.Create;
  try
    vrXML.Filename   := prCompleteFileName;
    vrXML.SearchNode := 'field';
    vrXML.ProvideData;

    for vrVez := 0 to vrXML.Size - 1 do
    begin
      Self.Fields.AddVariable(vrXML.GetRowByIndex(vrVez).Fields.VariableById('id').Value,
                              vrXML.GetRowByIndex(vrVez).Fields.VariableById('value').Value,
                              vrXML.GetRowByIndex(vrVez).Fields.VariableById('description').Value);

      Self.Fields.VariableFormById(vrXML.GetRowByIndex(vrVez).Fields.VariableById('id').Value).Required := StrToBool(vrXML.GetRowByIndex(vrVez).Fields.VariableById('required').Value);
      Self.Fields.VariableFormById(vrXML.GetRowByIndex(vrVez).Fields.VariableById('id').Value).ReadOnly := StrToBool(vrXML.GetRowByIndex(vrVez).Fields.VariableById('readOnly').Value);
    end;
  finally
    FreeAndNil(vrXML);
  end;
end;

procedure TJupiterGeneratorForm.Internal_ReadFileActions(prCompleteFileName: String);
var
  vrXML  : TJupiterXMLDataProvider;
  vrVez  : Integer;
begin
  vrXML := TJupiterXMLDataProvider.Create;
  try
    vrXML.Filename   := prCompleteFileName;
    vrXML.SearchNode := 'action';
    vrXML.ProvideData;

    for vrVez := 0 to vrXML.Size - 1 do
    begin
      Self.Actions.Add(TJupiterAction.Create(vrXML.GetRowByIndex(vrVez).Fields.VariableById('title').Value,
                                             TJupiterRunnable.Create(vrXML.GetRowByIndex(vrVez).Fields.VariableById('file').Value)));

      TJupiterAction(Self.Actions.GetLastObject).Icon                 := StrToIntDef(vrXML.GetRowByIndex(vrVez).Fields.VariableById('icon').Value, NULL_KEY);
      TJupiterAction(Self.Actions.GetLastObject).ConfirmBeforeExecute := StrToBool(vrXML.GetRowByIndex(vrVez).Fields.VariableById('confirmBeforeExecute').Value);
    end;
  finally
    FreeAndNil(vrXML);
  end;
end;

procedure TJupiterGeneratorForm.Internal_ReadFile(prCompleteFileName: String);
begin
  Self.Internal_ReadFileActions(prCompleteFileName);
  Self.Internal_ReadFileVariables(prCompleteFileName);
end;

function TJupiterGeneratorForm.Internal_GetFileName: String;
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    Result := vrEnviroment.FullPath(Format('modules/generator/%0:s.xml', [Self.FormID]));
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

constructor TJupiterGeneratorForm.Create;
begin
  Self.FFields  := TJupiterVariableFormList.Create;
  Self.FActions := TJupiterActionList.Create;
end;

destructor TJupiterGeneratorForm.Destroy;
begin
  FreeAndNil(Self.FFields);
  FreeAndNil(Self.FActions);

  inherited Destroy;
end;

procedure TJupiterGeneratorForm.SaveFile;
var
  vrVez : Integer;
  vrStr : TStrings;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add('<?xml version="1.0" encoding="UTF-8"?>');
    vrStr.Add('<content>');
    vrStr.Add('  <actions>');

    for vrVez := 0 to Self.Actions.Size - 1 do
    begin
      vrStr.Add('    <action>');
      vrStr.Add('      <title>' + TJupiterAction(Self.Actions.GetAtIndex(vrVez)).Title + '</title>');
      vrStr.Add('      <file>' + TJupiterAction(Self.Actions.GetAtIndex(vrVez)).Runnable.CommandLine + '</file>');
      vrStr.Add('      <icon>' + IntToStr(TJupiterAction(Self.Actions.GetAtIndex(vrVez)).Icon) + '</icon>');
      vrStr.Add('      <confirmBeforeExecute>' + BoolToStr(TJupiterAction(Self.Actions.GetAtIndex(vrVez)).Icon) + '</confirmBeforeExecute>');
      vrStr.Add('    </action>');
    end;

    vrStr.Add('  </actions>');
    vrStr.Add('  <fields>');

    for vrVez := 0 to Self.Fields.Size - 1 do
    begin
      vrStr.Add('    <field>');
      vrStr.Add('      <id>' + Self.Fields.VariableFormByIndex(vrVez).ID + '</id>');
      vrStr.Add('      <value>' + Self.Fields.VariableFormByIndex(vrVez).Value + '</value>');
      vrStr.Add('      <description>' + Self.Fields.VariableFormByIndex(vrVez).Title + '</description>');
      vrStr.Add('      <required>' + BoolToStr(Self.Fields.VariableFormByIndex(vrVez).Required) + '</required>');
      vrStr.Add('      <readOnly>' + BoolToStr(Self.Fields.VariableFormByIndex(vrVez).ReadOnly) + '</readOnly>');
      vrStr.Add('    </field>');
    end;

    vrStr.Add('  </fields>');
    vrStr.Add('</content>');

    vrStr.SaveToFile(Self.Internal_GetFileName);
  finally
    FreeAndNil(vrStr);
  end;
end;

end.

