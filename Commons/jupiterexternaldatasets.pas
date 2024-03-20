unit jupiterexternaldatasets;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterConsts, JupiterObject, JupiterEnviroment,
  JupiterCSVDataProvider;

type

  { TJupiterExternalDatasetItem }

  TJupiterExternalDatasetItem = class (TJupiterObject)
  public
    Name     : String;
    FileName : String;

    constructor Create(prName, prFileName : String);
  end;

  { TJupiterExternalDatasets }

  TJupiterExternalDatasets = class(TJupiterObjectList)
  private
    FFileName : String;

    procedure Internal_SaveFile();
  published
    property FileName : String read FFileName write FFileName;
  public
    procedure AddDataset(prItem : TJupiterExternalDatasetItem);

    function GetDatasetByPath(prPath : String) : TJupiterExternalDatasetItem;

    constructor Create; override;
  end;

implementation

{ TJupiterExternalDatasets }

procedure TJupiterExternalDatasets.Internal_SaveFile;
var
  vrEnviroment : TJupiterEnviroment;
  vrStr : TStrings;
begin
  vrStr        := TStringList.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrStr.Clear;
    vrStr.Add('NAME;FILE;');

    vrEnviroment.CreateFile('modules/generator/data/JupiterExternalVariables.csv', vrStr.Text);
  finally
    FreeAndNil(vrStr);
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterExternalDatasets.AddDataset(prItem: TJupiterExternalDatasetItem);
begin
  Self.Add(prItem);
end;

function TJupiterExternalDatasets.GetDatasetByPath(prPath: String): TJupiterExternalDatasetItem;
var
  vrVez : Integer;
begin
  Result := nil;

  for vrVez := 0 to Self.Count - 1 do
    with TJupiterExternalDatasetItem(Self.GetAtIndex(vrVez)) do
      if FileName = prPath then
      begin
        Result := TJupiterExternalDatasetItem(Self.GetAtIndex(vrVez));

        Exit;
      end;
end;

constructor TJupiterExternalDatasets.Create;
var
  vrEnviroment : TJupiterEnviroment;
  vrCSV : TJupiterCSVDataProvider;
  vrVez : Integer;
begin
  inherited Create;

  vrEnviroment := TJupiterEnviroment.Create;
  vrCSV        := TJupiterCSVDataProvider.Create;
  try
    if not vrEnviroment.Exists(vrEnviroment.FullPath('modules/generator/data/JupiterExternalVariables.csv')) then
    begin
      Self.Internal_SaveFile();
      Exit;
    end;

    vrCSV.Filename := vrEnviroment.FullPath('modules/generator/data/JupiterExternalVariables.csv');
    vrCSV.ProvideData;

    for vrVez := 0 to vrCSV.Count - 1 do
      with vrCSV.GetRowByIndex(vrVez) do
        Self.AddDataset(TJupiterExternalDatasetItem.Create(Fields.VariableById('NAME').Value, Fields.VariableById('FILE').Value));
  finally
    FreeAndNil(vrEnviroment);
    FreeAndNil(vrCSV);
  end;
end;

{ TJupiterExternalDatasetItem }

constructor TJupiterExternalDatasetItem.Create(prName, prFileName: String);
begin
  Self.Name     := prName;
  Self.FileName := prFileName;
end;

end.

