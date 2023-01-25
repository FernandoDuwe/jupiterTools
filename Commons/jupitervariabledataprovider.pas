unit JupiterVariableDataProvider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterVariable, JupiterDataProvider;

type

  { TJupiterVariableDataProvider }

  TJupiterVariableDataProvider = class(TJupiterVariable)
  public
    procedure AsList(var prList : TStrings); override;
  end;


  { TJupiterVariableDataProviderList }

  TJupiterVariableDataProviderList = class(TJupiterVariableList)
  public
    procedure AddConfig(prID : String; prValue : String; prTitle : String = ''); override;
    procedure AddVariable(prID : String; prValue : String; prTitle : String = ''); override;
  end;


implementation

{ TJupiterVariableDataProviderList }

procedure TJupiterVariableDataProviderList.AddConfig(prID: String; prValue: String; prTitle: String);
var
  vrObj : TJupiterVariableDataProvider;
begin
  try
    if Assigned(Self.VariableById(prID)) then
    begin
      with Self.VariableById(prID) do
      begin
        Owner := Self;
        Title := prTitle;
        Value := prValue;
        Save  := True;
      end;
    end
    else
    begin
      vrObj := TJupiterVariableDataProvider.Create;
      vrObj.ID    := prID;
      vrObj.Value := prValue;
      vrObj.Title := prTitle;
      vrObj.Save  := True;
      vrObj.Owner := Self;

      vrObj.OnOwnerChangeValue := @Internal_OnOwnerChangeValue;

      Self.Add(vrObj);
    end;
  finally
    if not Self.FReadingFile then
      Self.SaveToFile;
  end;
end;

procedure TJupiterVariableDataProviderList.AddVariable(prID: String; prValue: String; prTitle: String);
var
  vrObj : TJupiterVariableDataProvider;
begin
  if Assigned(Self.VariableById(prID)) then
  begin
    with Self.VariableById(prID) do
    begin
      Title := prTitle;
      Value := prValue;
      Save  := False;
      Owner := Self;
    end;
  end
  else
  begin
    vrObj := TJupiterVariableDataProvider.Create;
    vrObj.ID    := prID;
    vrObj.Value := prValue;
    vrObj.Title := prTitle;
    vrObj.Save  := False;
    vrObj.Owner := Self;

    Self.Add(vrObj);
  end;
end;

{ TJupiterVariableDataProviderList }


{ TJupiterVariableDataProvider }

procedure TJupiterVariableDataProvider.AsList(var prList: TStrings);
var
  vrProvider  : TJupiterDataProvider;
  vrFieldName : String;
  vrVez       : Integer;
begin
  prList.Clear;

  vrFieldName := EmptyStr;

  vrProvider := FactoryDataProviderFromString(Self.Value, vrFieldName);

  for vrVez := 0 to vrProvider.Count - 1 do
    with vrProvider.GetRowByIndex(vrVez) do
    begin
      if Fields.Exists(vrFieldName) then
        prList.Add(Fields.VariableById(vrFieldName).Value)
      else
        prList.Add(EmptyStr);
    end;
end;

end.

