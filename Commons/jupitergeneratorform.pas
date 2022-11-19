unit JupiterGeneratorForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, JupiterConsts, JupiterEnviroment,
  JupiterVariable, JupiterAction;

type

  { TJupiterGeneratorForm }

  TJupiterGeneratorForm = class(TJupiterObject)
  private
    FFormID  : String;
    FFields  : TJupiterVariableList;
    FActions : TJupiterActionList;

    procedure Internal_SetFormID(prFormID : String);
    procedure Internal_CreateBlankFile(prCompleteFileName : String);
    procedure Internal_ReadFile(prCompleteFileName : String);
  published
    property Actions : TJupiterActionList   read FActions write FActions;
    property Fields  : TJupiterVariableList read FFields  write FFields;
    property FormID  : String               read FFormID  write Internal_SetFormID;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses XMLConf;

{ TJupiterGeneratorForm }

procedure TJupiterGeneratorForm.Internal_SetFormID(prFormID: String);
var
  vrEnviroment : TJupiterEnviroment;
  vrFileName   : String;
begin
  Self.FFormID := prFormID;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrFileName := vrEnviroment.FullPath(Format('modules/generator/%0:s.xml', [prFormID]));

    if not FileExists(vrFileName) then
      Self.Internal_CreateBlankFile(vrFileName)
    else
      Self.Internal_ReadFile(vrFileName);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterGeneratorForm.Internal_CreateBlankFile(prCompleteFileName : String);
var
  vrStr : TStrings;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add('<?xml version="1.0" encoding="UTF-8"?>');
    vrStr.Add('      <content>');
    vrStr.Add('                  <actions>');
    vrStr.Add('                  </actions>');
    vrStr.Add('                  <fields>');
    vrStr.Add('                  </fields>');
    vrStr.Add('      </content>');

    vrStr.SaveToFile(prCompleteFileName);
  finally
    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterGeneratorForm.Internal_ReadFile(prCompleteFileName: String);
var
  vrXML : TXMLConfig;
begin
  vrXML := TXMLConfig.Create(nil);
  try
    vrXML.Filename := prCompleteFileName;
  finally
    FreeAndNil(vrXML);
  end;
end;

constructor TJupiterGeneratorForm.Create;
begin
  Self.FFields  := TJupiterVariableList.Create;
  Self.FActions := TJupiterActionList.Create;
end;

destructor TJupiterGeneratorForm.Destroy;
begin
  FreeAndNil(Self.FFields);
  FreeAndNil(Self.FActions);

  inherited Destroy;
end;

end.

