unit JupiterDataProvider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, JupiterVariable, JupiterConsts,
  jupiterStringUtils;

type

  { TJupiterDataProviderRow }

  TJupiterDataProviderRow = class(TJupiterObject)
  private
    FFields : TJupiterVariableList;
  published
    property Fields : TJupiterVariableList read FFields write FFields;
  public
    function CanShowInSearch(prSearch : String) : Boolean;

    constructor Create;
    destructor Destroy; override;
  end;

  TJupiterDataProvider = class(TJupiterObjectList)
  private
    FProviderID : String;
  published
    property ProviderID : String read FProviderID;
  public
    procedure AddRow;
    function GetLastRow : TJupiterDataProviderRow;
    function GetRowByIndex(prIndex : Integer) : TJupiterDataProviderRow;
    procedure ClearRows;

    procedure ProvideData; virtual;

    class procedure GetFieldsLayout(var prList : TStrings); virtual;

    constructor Create; override;
    destructor Destroy; override;
  end;

  function FactoryDataProvider(prDataProviderType : String; prParam : String; prSubFolders : Boolean) : TJupiterDataProvider;

  function FactoryDataProviderFromString(prString : String; var prFieldToRead : String) : TJupiterDataProvider;

implementation

uses JupiterCSVDataProvider, JupiterApp;

function FactoryDataProvider(prDataProviderType: String; prParam : String; prSubFolders : Boolean): TJupiterDataProvider;
begin
  if prDataProviderType = DATAPROVIDER_TYPE_LIST_CSV then
  begin
    Result := TJupiterCSVDataProvider.Create;

    TJupiterCSVDataProvider(Result).Filename := prParam;
    TJupiterCSVDataProvider(Result).ProvideData;
    Exit;
  end;

  Result := TJupiterDataProvider.Create;
  Result.ProvideData;
end;

function FactoryDataProviderFromString(prString: String; var prFieldToRead : String): TJupiterDataProvider;
var
  vrStr : TStrings;
begin
  prString := StringReplace(prString, ')', EmptyStr, [rfReplaceAll, rfIgnoreCase]);
  prString := StringReplace(prString, '(', ',', [rfReplaceAll, rfIgnoreCase]);
  prString := StringReplace(prString, ' ', EmptyStr, [rfReplaceAll, rfIgnoreCase]);

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Delimiter     := ',';
    vrStr.DelimitedText := prString;

    if vrStr[0] = DATAPROVIDER_TYPE_LIST_CSV then
    begin
      prFieldToRead := vrStr[2];

      Result := TJupiterCSVDataProvider.Create;
      TJupiterCSVDataProvider(Result).Filename := vrStr[1];
      Result.ProvideData;
      Exit;
    end;

    prFieldToRead := vrStr[2];
    Result := TJupiterDataProvider.Create;
    Result.ProvideData;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

{ TJupiterDataProvider }

procedure TJupiterDataProvider.AddRow;
begin
  Self.Add(TJupiterDataProviderRow.Create);
end;

function TJupiterDataProvider.GetLastRow: TJupiterDataProviderRow;
begin
  Result := TJupiterDataProviderRow(Self.GetLastObject);
end;

function TJupiterDataProvider.GetRowByIndex(prIndex: Integer): TJupiterDataProviderRow;
begin
  Result := TJupiterDataProviderRow(Self.GetAtIndex(prIndex));
end;

procedure TJupiterDataProvider.ClearRows;
begin
  try
    while Self.Count > 0 do
      Self.DeleteAtIndex(0);
  finally
    Self.ClearListItens;
  end;
end;

procedure TJupiterDataProvider.ProvideData;
begin
  //
end;

class procedure TJupiterDataProvider.GetFieldsLayout(var prList: TStrings);
begin
  prList.Clear;
end;

constructor TJupiterDataProvider.Create;
begin
  inherited Create;

  Self.FProviderID := JupiterStringUtilsGenerateGUID;

  if Assigned(vrJupiterApp) then
    if vrJupiterApp.AppReady then
      vrJupiterApp.DataProviders.Add(Self);
end;

destructor TJupiterDataProvider.Destroy;
begin
  if Assigned(vrJupiterApp) then
    if vrJupiterApp.AppReady then
      vrJupiterApp.DeleteDataProviderById(Self.ProviderID);

  inherited Destroy;
end;

{ TJupiterDataProviderRow }

function TJupiterDataProviderRow.CanShowInSearch(prSearch: String): Boolean;
var
  vrVez : Integer;
begin
  Result := False;

  for vrVez := 0 to Self.Fields.Size - 1 do
    with Self.Fields.VariableByIndex(vrVez) do
      if Pos(AnsiUpperCase(prSearch), AnsiUpperCase(Self.Fields.VariableByIndex(vrVez).Value)) <> 0 then
      begin
        Result := True;
        Exit;
      end;
end;

constructor TJupiterDataProviderRow.Create;
begin
  Self.FFields := TJupiterVariableList.Create;
end;

destructor TJupiterDataProviderRow.Destroy;
begin
  FreeAndNil(Self.FFields);

  inherited Destroy;
end;

end.

