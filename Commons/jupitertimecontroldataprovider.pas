unit jupiterTimeControlDataProvider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterDataProvider, JupiterConsts, JupiterCSVDataProvider,
  JupiterEnviroment, SysUtils;

type

  { TJupiterTimeControlDataProvider }

  TJupiterTimeControlDataProvider = class(TJupiterCSVDataProvider)
  private
    FStarted : Boolean;
    FEnded : Boolean;
    FCurrentDate : TDate;
    FExecutedTime : TTime;

    function Internal_GetFileName : String;
  published
    property CurrentDate  : TDate   read FCurrentDate  write FCurrentDate;
    property ExecutedTime : TTime   read FExecutedTime write FExecutedTime;
    property Started      : Boolean read FStarted      write FStarted;
    property Ended        : Boolean read FEnded        write FEnded;
  public
    procedure RegisterStartTime(prDetail : String = '');
    procedure RegisterEndTime;
    procedure CleanTimes;
    procedure CreateStandardTimes;
    procedure EditLine(prLine : Integer; prStart, prEnd, prDetails : String);
    procedure RemoveLine(prLine : Integer);

    procedure ProvideData; override;

    constructor Create; override;
  end;

implementation

{ TJupiterTimeControlDataProvider }

function TJupiterTimeControlDataProvider.Internal_GetFileName: String;
var
  vrEnviroment : TJupiterEnviroment;
begin
  Result := EmptyStr;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    Result := vrEnviroment.FullPath('/datasets/TimeControl.csv');
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterTimeControlDataProvider.RegisterStartTime(prDetail : String = '');
var
  vrStr : TStrings;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;

    if FileExists(Self.Filename) then
      vrStr.LoadFromFile(Self.Filename)
    else
      vrStr.Add('date;start;end;info;');

    vrStr.Add(FormatDateTime('dd/mm/yyyy', Self.CurrentDate) + ';' + FormatDateTime('hh:nn:ss', Now) + ';;' + prDetail + ';');

    vrStr.SaveToFile(Self.Filename);
  finally
    vrStr.Clear;

    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterTimeControlDataProvider.RegisterEndTime;
var
  vrStr : TStrings;
  vrVez : Integer;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.LoadFromFile(Self.Filename);

    for vrVez := 0 to vrStr.Count - 1 do
    begin
      if Trim(vrStr[vrVez]) = EmptyStr then
        Continue;

      if GetCSVColumn(vrStr[vrVez], 0) = FormatDateTime('dd/mm/yyyy', Self.CurrentDate) then
        if Trim(GetCSVColumn(vrStr[vrVez], 2)) = EmptyStr then
        begin
          vrStr[vrVez] := GetCSVColumn(vrStr[vrVez], 0) + ';' + GetCSVColumn(vrStr[vrVez], 1) + ';' + FormatDateTime('hh:nn:ss', Now) + ';' + GetCSVColumn(vrStr[vrVez], 3) + ';';

          vrStr.SaveToFile(Self.Filename);

          Exit;
        end;
    end;
  finally
    vrStr.Clear;

    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterTimeControlDataProvider.CleanTimes;
var
  vrStr : TStrings;
  vrVez : Integer;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.LoadFromFile(Self.Filename);

    for vrVez := vrStr.Count - 1 downto 0 do
    begin
      if Trim(vrStr[vrVez]) = EmptyStr then
        Continue;

      if GetCSVColumn(vrStr[vrVez], 0) = FormatDateTime('dd/mm/yyyy', Self.CurrentDate) then
        vrStr.Delete(vrVez);
    end;

    vrStr.SaveToFile(Self.Filename);
  finally
    vrStr.Clear;

    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterTimeControlDataProvider.CreateStandardTimes;
begin
  //
end;

procedure TJupiterTimeControlDataProvider.EditLine(prLine: Integer; prStart,
  prEnd, prDetails: String);
var
  vrStr : TStrings;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.LoadFromFile(Self.Filename);

    vrStr[prLine] := GetCSVColumn(vrStr[prLine], 0) + ';' + prStart + ';' + prEnd + ';' + prDetails + ';';

    vrStr.SaveToFile(Self.Filename);
  finally
    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterTimeControlDataProvider.RemoveLine(prLine: Integer);
var
  vrStr : TStrings;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.LoadFromFile(Self.Filename);

    vrStr.Delete(prLine);

    vrStr.SaveToFile(Self.Filename);
  finally
    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterTimeControlDataProvider.ProvideData;
var
  vrVez       : Integer;
  vrStartTime : TTime;
  vrEndTime   : TTime;
begin
  Self.FStarted      := False;
  Self.FEnded        := False;

  Self.Filename      := Self.Internal_GetFileName;

  if not FileExists(Self.Filename) then
    Exit;

  Self.ClearRows;

  Self.FilterColumn  := 0;
  Self.FilterValue   := FormatDateTime('dd/mm/yyyy', Self.CurrentDate);
  Self.FExecutedTime := 0;

  inherited ProvideData;

  for vrVez := 0 to Self.Count - 1 do
    with Self.GetRowByIndex(vrVez) do
    begin
      if not Fields.Exists('end') then
        Continue;

      if Trim(Fields.VariableById('end').Value) = EmptyStr then
        Continue;

      vrStartTime := StrToTime(Fields.VariableById('start').Value);
      vrEndTime   := StrToTime(Fields.VariableById('end').Value);

      Self.ExecutedTime := Self.ExecutedTime + (vrEndTime - vrStartTime);
    end;

  if not Self.IsEmpty then
  begin
    if Self.GetLastRow.Fields.Exists('start') then
      Self.FStarted := Trim(Self.GetLastRow.Fields.VariableById('start').Value) <> EmptyStr;

    if Self.GetLastRow.Fields.Exists('end') then
    begin
      Self.FEnded := Trim(Self.GetLastRow.Fields.VariableById('end').Value) <> EmptyStr;

      if Self.FEnded then
        Self.FStarted := False;
    end;
  end;
end;

constructor TJupiterTimeControlDataProvider.Create;
begin
  inherited Create;

  Self.CurrentDate := Now;
end;

end.

