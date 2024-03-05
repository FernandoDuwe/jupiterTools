unit jupiterlayoutvalidator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterConsts, JupiterObject, JupiterCSVDataProvider;

type

  { TJupiterLayoutValidator }

  TJupiterLayoutValidator = class(TJupiterObject)
  private
    FLine : String;
    FFileName : String;
  published
    property FileName : String read FFileName write FFileName;
    property Line     : String read FLine     write FLine;
  public
    function Validate : TStrings;
  end;

implementation

{ TJupiterLayoutValidator }

function TJupiterLayoutValidator.Validate: TStrings;
var
  vrValidator: TJupiterCSVDataProvider;
  vrVez : Integer;
  vrValue : Double;
begin
  Result := TStringList.Create;
  Result.Clear;

  vrValidator := TJupiterCSVDataProvider.Create;
  try
    vrValidator.Filename := Self.FileName;
    vrValidator.ProvideData;

    for vrVez := 0 to vrValidator.Count - 1 do
      with vrValidator.GetRowByIndex(vrVez) do
      begin
        if Trim(Fields.VariableById('Type').Value) = EmptyStr then
          Continue;

        if AnsiUpperCase(Trim(Fields.VariableById('Type').Value)) = 'A' then
          Continue;

        if AnsiUpperCase(Trim(Fields.VariableById('Type').Value)) = 'N' then
        begin
          if not TryStrToFloat(Copy(Self.Line, StrToIntDef(Fields.VariableById('Start').Value, 0), StrToIntDef(Fields.VariableById('Size').Value, 0)), vrValue) then
            Result.Add(Format('%0:s - Valor "%1:s" numérico inválido', [Fields.VariableById('Field').Value, Copy(Self.Line, StrToIntDef(Fields.VariableById('Start').Value, 0), StrToIntDef(Fields.VariableById('Size').Value, 0))]));

          Continue;
        end;
      end;
  finally
    FreeAndNil(vrValidator);
  end;
end;

end.

