unit JupiterTaskTimesDataProvider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterDataProvider, JupiterConsts, JupiterCSVDataProvider;

type

  { TJupiterTaskTimesDataProvider }

  TJupiterTaskTimesDataProvider = class(TJupiterDataProvider)
  private
    FFileName : String;

    procedure Internal_InsertLine(prStartTime, prEndTime : String);
  published
    property FileName : String read FFileName write FFileName;
  public
    procedure ProvideData; override;

    class procedure GetFieldsLayout(var prList : TStrings); override;
  end;

implementation

{ TJupiterTaskTimesDataProvider }

procedure TJupiterTaskTimesDataProvider.Internal_InsertLine(prStartTime, prEndTime : String);
begin
  Self.AddRow;

  if Trim(prStartTime) <> EmptyStr then
    Self.GetLastRow.Fields.AddVariable('startTime',
                                       Format('%0:s %1:s', [GetCSVColumn(prStartTime, 1), GetCSVColumn(prStartTime, 2)]),
                                       'Tempo Inicial')
  else
    Self.GetLastRow.Fields.AddVariable('startTime',
                                       EmptyStr,
                                       'Tempo Inicial');

  if Trim(prEndTime) <> EmptyStr then
    Self.GetLastRow.Fields.AddVariable('endTime',
                                       Format('%0:s %1:s', [GetCSVColumn(prEndTime, 1), GetCSVColumn(prEndTime, 2)]),
                                       'Tempo Final')
  else
    Self.GetLastRow.Fields.AddVariable('endTime',
                                       EmptyStr,
                                       'Tempo Final');
end;

procedure TJupiterTaskTimesDataProvider.ProvideData;
var
  vrStr    : TStrings;
  vrInicio : String;
  vrFim    : String;
  vrVez    : Integer;
begin
  inherited ProvideData;

  if ((Trim(Self.FileName) = EmptyStr) or (not FileExists(Self.FileName))) then
     raise Exception.Create('Filename must be valid');

  vrStr := TStringList.Create;
  try
    vrInicio := EmptyStr;
    vrFim    := EmptyStr;

    vrStr.LoadFromFile(Self.FileName);

    for vrVez := 0 to vrStr.Count - 1 do
    begin
      if Trim(vrStr[vrVez]) = EmptyStr then
         Continue;

      if Copy(Trim(vrStr[vrVez]), 1, 1) = 'I' then
        vrInicio := Trim(vrStr[vrVez]);

      if Copy(Trim(vrStr[vrVez]), 1, 1) = 'F' then
      begin
        vrFim := Trim(vrStr[vrVez]);

        Self.Internal_InsertLine(vrInicio, vrFim);

        vrInicio := EmptyStr;
        vrFim    := EmptyStr;
      end;
    end;

    if Trim(vrInicio) <> EmptyStr then
      Self.Internal_InsertLine(vrInicio, vrFim);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

class procedure TJupiterTaskTimesDataProvider.GetFieldsLayout(
  var prList: TStrings);
begin
  inherited GetFieldsLayout(prList);

  prList.Add('startTime');
  prList.Add('endTime');
end;

end.

