unit JupiterCSVDataProvider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterDataProvider, JupiterConsts, JupiterVariable;

  function GetCSVColumn(prLine : String; prIndex : Integer) : String;

type

  { TJupiterCSVDataProvider }

  TJupiterCSVDataProvider = class(TJupiterDataProvider)
  private
    FFilename    : String;
    FColumnCount : Integer;

    function  Internal_GetCSVColumnCount(prLine : String) : Integer;
    function  Internal_GetCSVColumn(prLine: String; prIndex: Integer): String;
    procedure Internal_ProcessLine(prLineStr : String; prHeaderLine : String);
    procedure Internal_SaveLine;
  published
    property Filename : String read FFilename write FFilename;
  public
    procedure ProvideData; override;
    procedure SaveLine(prFields : TJupiterVariableList);
  end;


implementation

{ TJupiterCSVDataProvider }

function TJupiterCSVDataProvider.Internal_GetCSVColumnCount(prLine: String): Integer;
var
  vrStr : TStrings;
begin
  Result := NULL_KEY;

  if Trim(prLine) = EmptyStr then
    Exit;

  vrStr := TStringList.Create;
  try
    vrStr.Delimiter     := ';';
    vrStr.DelimitedText := StringReplace(prLine, ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]);

    Result := vrStr.Count;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

function TJupiterCSVDataProvider.Internal_GetCSVColumn(prLine: String; prIndex: Integer): String;
var
  vrStr : TStrings;
begin
  Result := EmptyStr;

  if Trim(prLine) = EmptyStr then
    Exit;

  vrStr := TStringList.Create;
  try
    vrStr.Delimiter     := ';';
    vrStr.DelimitedText := StringReplace(prLine, ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]);

    if (vrStr.Count - 1) < prIndex then
      Exit;

    Result := vrStr[prIndex];
    Result := StringReplace(Result, EMPTY_SPACE_SEPARATOR, ' ', [rfIgnoreCase, rfReplaceAll]);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterCSVDataProvider.Internal_ProcessLine(prLineStr: String; prHeaderLine: String);
var
  vrVez : Integer;
begin
  Self.AddRow;

  for vrVez := 0 to Self.FColumnCount - 1 do
  begin
    if Self.Internal_GetCSVColumn(prHeaderLine, vrVez) = EmptyStr then
      Continue;

    Self.GetLastRow.Fields.AddVariable(Self.Internal_GetCSVColumn(prHeaderLine, vrVez), Self.Internal_GetCSVColumn(prLineStr, vrVez), Self.Internal_GetCSVColumn(prHeaderLine, vrVez));
  end;

  Self.GetLastRow.Fields.AddVariable('Line', IntToStr(Self.Size), 'Linha');
end;

procedure TJupiterCSVDataProvider.Internal_SaveLine;
var
  vrStrFile : TStrings;
  vrVez     : Integer;
  vrVez2    : Integer;
  vrStrAux  : String;
begin
  vrStrFile := TStringList.Create;
  try
    vrStrFile.LoadFromFile(Self.Filename);

    for vrVez := vrStrFile.Count - 1 downto 1 do
      vrStrFile.Delete(vrVez);

    for vrVez := 0 to Self.Size - 1 do
      with Self.GetRowByIndex(vrVez) do
      begin
        vrStrAux := EmptyStr;

        for vrVez2 := 0 to Fields.Size - 1 do
        begin
          if ((vrVez2 = (Fields.Size - 1)) and (Fields.VariableByIndex(vrVez2).ID = 'Line')) then
            Continue;

          vrStrAux := vrStrAux + Fields.VariableByIndex(vrVez2).Value + ';';
        end;

        vrStrFile.Add(vrStrAux);
      end;

    vrStrFile.SaveToFile(Self.Filename);
  finally
    FreeAndNil(vrStrFile);
  end;
end;

procedure TJupiterCSVDataProvider.ProvideData;
var
  vrFile : TStrings;
  vrVez  : Integer;
begin
  inherited ProvideData;

  if ((Trim(Self.Filename) = EmptyStr) or (not FileExists(Self.Filename))) then
     raise Exception.Create('Filename must be valid');

  vrFile := TStringList.Create;
  try
    vrFile.Clear;
    vrFile.LoadFromFile(Self.Filename);

    if vrFile.Count > 0 then
       Self.FColumnCount := Self.Internal_GetCSVColumnCount(vrFile[0]);

    for vrVez := 1 to vrFile.Count - 1 do
    begin
      if Trim(vrFile[vrVez]) = EmptyStr then
        Continue;

      Self.Internal_ProcessLine(vrFile[vrVez], vrFile[0]);
    end;
  finally
    vrFile.Clear;
    FreeAndNil(vrFile);
  end;
end;

procedure TJupiterCSVDataProvider.SaveLine(prFields: TJupiterVariableList);
var
  vrLine : Integer;
begin
  vrLine := -1;

  try
    if prFields.Exists('Line') then
      vrLine := StrToIntDef(prFields.VariableById('Line').Value, -1);

    if vrLine <= 0 then
    begin
      Self.AddRow;

      Self.GetLastRow.Fields.CopyValues(prFields);
    end
    else
      with Self.GetRowByIndex(vrLine - 1) do
        Fields.CopyValues(prFields);
  finally
    Self.Internal_SaveLine;
  end;
end;

function GetCSVColumn(prLine: String; prIndex: Integer): String;
var
  vrStr : TStrings;
begin
  Result := EmptyStr;

  if Trim(prLine) = EmptyStr then
    Exit;

  vrStr := TStringList.Create;
  try
    vrStr.Delimiter     := ';';
    vrStr.DelimitedText := StringReplace(prLine, ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]);

    if (vrStr.Count - 1) < prIndex then
      Exit;

    Result := vrStr[prIndex];
    Result := StringReplace(Result, EMPTY_SPACE_SEPARATOR, ' ', [rfIgnoreCase, rfReplaceAll]);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

end.
