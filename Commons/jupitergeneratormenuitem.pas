unit JupiterGeneratorMenuItem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterObject, JupiterConsts, JupiterVariable, JupiterAction,
  JupiterRoute, JupiterXMLDataProvider, SysUtils;

type

  { TJupiterGeneratorMenuItem }

  TJupiterGeneratorMenuItem = class(TJupiterObject)
  private
    FFileName     : String;
    FTitle        : String;
    FRoutePath    : String;
    FLocationPath : String;
    FParams       : TJupiterVariableList;

    procedure Internal_SetFileName(prFileName : String);
    procedure Internal_ReadParams(prFileName : String);
  published
    property FileName     : String read FFileName     write Internal_SetFileName;
    property Title        : String read FTitle        write FTitle;
    property RoutePath    : String read FRoutePath    write FRoutePath;
    property LocationPath : String read FLocationPath write FLocationPath;

    property Params : TJupiterVariableList read FParams write FParams;
  public
    procedure SaveFile;

    function CreateAction : TJupiterAction;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TJupiterGeneratorMenuItem }

procedure TJupiterGeneratorMenuItem.Internal_SetFileName(prFileName: String);
var
  vrXML : TJupiterXMLDataProvider;
begin
  Self.FFileName := prFileName;

  if not FileExists(Self.FFileName) then
    Exit;

  vrXML := TJupiterXMLDataProvider.Create;
  try
    vrXML.SearchNode := 'content';
    vrXML.Filename   := prFileName;
    vrXML.ProvideData;

    Self.Title        := vrXML.GetRowByIndex(0).Fields.VariableById('title').Value;
    Self.RoutePath    := vrXML.GetRowByIndex(0).Fields.VariableById('routePath').Value;
    Self.LocationPath := vrXML.GetRowByIndex(0).Fields.VariableById('locationPath').Value;

    Self.Internal_ReadParams(prFileName);
  finally
    FreeAndNil(vrXML);
  end;
end;

procedure TJupiterGeneratorMenuItem.Internal_ReadParams(prFileName: String);
var
  vrXML : TJupiterXMLDataProvider;
  vrVez : Integer;
begin
  vrXML := TJupiterXMLDataProvider.Create;
  try
    vrXML.SearchNode := 'param';
    vrXML.Filename   := prFileName;
    vrXML.ProvideData;

    for vrVez := 0 to vrXML.Size - 1 do
    begin
      Self.Params.AddConfig(vrXML.GetRowByIndex(vrVez).Fields.VariableById('id').Value,
                            vrXML.GetRowByIndex(vrVez).Fields.VariableById('value').Value,
                            vrXML.GetRowByIndex(vrVez).Fields.VariableById('title').Value);

      TJupiterVariable(Self.Params.GetLastObject).Tag := vrVez;
    end;
  finally
    FreeAndNil(vrXML);
  end;
end;

procedure TJupiterGeneratorMenuItem.SaveFile;
var
  vrStr : TStrings;
  vrVez : Integer;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add('<?xml version="1.0" encoding="UTF-8"?>');
    vrStr.Add('<content>');
    vrStr.Add('  <title>' + Self.Title + '</title>');
    vrStr.Add('  <routePath>' + Self.RoutePath + '</routePath>');
    vrStr.Add('  <locationPath>' + Self.LocationPath + '</locationPath>');
    vrStr.Add('  <params>');

    for vrVez := 0 to Self.Params.Size -1 do
    begin
      vrStr.Add('    <param>');
      vrStr.Add('      <id>' + Self.Params.VariableByIndex(vrVez).ID + '</id>');
      vrStr.Add('      <value>' + Self.Params.VariableByIndex(vrVez).Value + '</value>');
      vrStr.Add('      <title>' + Self.Params.VariableByIndex(vrVez).Title + '</title>');
      vrStr.Add('    </param>');
    end;

    vrStr.Add('  </params>');
    vrStr.Add('</content>');

    vrStr.SaveToFile(Self.FileName);
  finally
    FreeAndNil(vrStr);
  end;
end;

function TJupiterGeneratorMenuItem.CreateAction: TJupiterAction;
begin
  Result := TJupiterAction.Create(Self.Title,
                                  TJupiterRoute.Create(Self.RoutePath),
                                  TJupiterRoute.Create(Self.LocationPath));

  Result.Route.Params.CopyValues(Self.Params);
end;

constructor TJupiterGeneratorMenuItem.Create;
begin
  Self.FParams := TJupiterVariableList.Create;
end;

destructor TJupiterGeneratorMenuItem.Destroy;
begin
  FreeAndNil(Self.FParams);

  inherited Destroy;
end;

end.

