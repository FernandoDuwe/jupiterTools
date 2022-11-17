unit JupiterDataProvider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, JupiterVariable, JupiterConsts;

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
  public
    procedure AddRow;
    function GetLastRow : TJupiterDataProviderRow;
    function GetRowByIndex(prIndex : Integer) : TJupiterDataProviderRow;

    procedure ProvideData; virtual;
  end;

  function FactoryDataProvider(prDataProviderType : String; prParam : String) : TJupiterDataProvider;

implementation

uses JupiterFileDataProvider, JupiterDirectoryDataProvider, JupiterCSVDataProvider,
     JupiterTasksDataProvider;

function FactoryDataProvider(prDataProviderType: String; prParam : String): TJupiterDataProvider;
begin
  if prDataProviderType = DATAPROVIDER_TYPE_LIST_FILES then
  begin
    Result := TJupiterFileDataProvider.Create;

    TJupiterFileDataProvider(Result).Path := prParam;
    TJupiterFileDataProvider(Result).ProvideData;
    Exit;
  end;

  if prDataProviderType = DATAPROVIDER_TYPE_LIST_PATHS then
  begin
    Result := TJupiterDirectoryDataProvider.Create;

    TJupiterDirectoryDataProvider(Result).Path := prParam;
    TJupiterDirectoryDataProvider(Result).ProvideData;
    Exit;
  end;

  if prDataProviderType = DATAPROVIDER_TYPE_LIST_CSV then
  begin
    Result := TJupiterCSVDataProvider.Create;

    TJupiterCSVDataProvider(Result).Filename := prParam;
    TJupiterCSVDataProvider(Result).ProvideData;
    Exit;
  end;

  if prDataProviderType = DATAPROVIDER_TYPE_TASKS then
  begin
    Result := TJupiterTasksDataProvider.Create;

    TJupiterTasksDataProvider(Result).ProvideData;
    Exit;
  end;

  Result := TJupiterDataProvider.Create;
  Result.ProvideData;
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

procedure TJupiterDataProvider.ProvideData;
begin
  //
end;

{ TJupiterDataProviderRow }

function TJupiterDataProviderRow.CanShowInSearch(prSearch: String): Boolean;
var
  vrVez : Integer;
begin
  Result := False;

  for vrVez := 0 to Self.Fields.Size - 1 do
    with Self.Fields.VariableByIndex(vrVez) do
      if Pos(AnsiUpperCase(prSearch), AnsiUpperCase(Self.Fields.VariableByIndex(vrVez).Value)) = 0 then
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

