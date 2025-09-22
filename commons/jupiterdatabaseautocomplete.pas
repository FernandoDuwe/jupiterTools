unit jupiterDatabaseAutoComplete;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, jupiterDatabaseWizard, JupiterConsts, SQLDB,
  uJupiterStringUtilsScript;

type

  { TJupiterDatabaseAutoComplete }

  TJupiterDatabaseAutoComplete = class(TJupiterDatabaseWizard)
  public
    TableList : TStrings;
    VariableList : TStrings;
    ReserveWords : TStrings;

    function Internal_GetTableFromCurrent(prCurrentString : String) : String;

    function Internal_GetTableFromNickName(prText, prCurrentString : String) : String;
  public
    function GenerateList(prText, prCurrentString : String) : TStrings;

    constructor Create(prConnection : TSQLConnection); override;
    destructor Destroy; override;
  end;

implementation

{ TJupiterDatabaseAutoComplete }

function TJupiterDatabaseAutoComplete.Internal_GetTableFromCurrent(prCurrentString : String): String;
begin
  Result := EmptyStr;

  prCurrentString := AnsiUpperCase(StringReplace(prCurrentString, '.', EmptyStr, [rfIgnoreCase, rfReplaceAll]));

  if Self.TableList.IndexOf(prCurrentString) <> NULL_KEY then
  begin
    Result := Self.TableList[Self.TableList.IndexOf(prCurrentString)];

    Exit;
  end;
end;

function TJupiterDatabaseAutoComplete.Internal_GetTableFromNickName(prText, prCurrentString: String): String;
begin
  Result := EmptyStr;
end;

function TJupiterDatabaseAutoComplete.GenerateList(prText, prCurrentString : String) : TStrings;
var
  vrCurrentTable : String;
begin
  Result := TStringList.Create;
  Result.Clear;

  if JupiterStringUtilsScript_CountCharInString('{', prText) <> JupiterStringUtilsScript_CountCharInString('}', prText) then
  begin
    Result.AddStrings(VariableList);
    Exit;
  end;

  vrCurrentTable := Self.Internal_GetTableFromCurrent(prCurrentString);

  if vrCurrentTable <> EmptyStr then
  begin
    Self.Connection.GetFieldNames(vrCurrentTable, Result);

    Exit;
  end;

  vrCurrentTable := Self.Internal_GetTableFromNickName(prText, prCurrentString);

  if vrCurrentTable <> EmptyStr then
  begin
    Self.Connection.GetFieldNames(vrCurrentTable, Result);

    Exit;
  end;

  Result.AddStrings(Self.TableList);
end;

constructor TJupiterDatabaseAutoComplete.Create(prConnection: TSQLConnection);
begin
  inherited Create(prConnection);

  Self.TableList := TStringList.Create;
  Self.TableList.Clear;

  Self.VariableList := TStringList.Create;
  Self.VariableList.Clear;

  Self.ReserveWords := TStringList.Create;

end;

destructor TJupiterDatabaseAutoComplete.Destroy;
begin
  Self.TableList.Clear;

  FreeAndNil(Self.TableList);

  Self.VariableList.Clear;

  FreeAndNil(Self.VariableList);

  inherited Destroy;
end;

end.

