unit JupiterParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jupiterconsts;

type

  { TJupiterParams }

  TJupiterParams = class(TObject)
  private
    FOnAddParam : TJupiterRunnableItemAddParam;

    function  Internal_GetFileContent(prFileName : String) : String;
  published
    property OnAddParam : TJupiterRunnableItemAddParam read FOnAddParam write FOnAddParam;
  public
    procedure CheckFile;
    procedure List;
  end;

implementation

uses fpjson, JsonParser;

{ TJupiterParams }

function TJupiterParams.Internal_GetFileContent(prFileName: String): String;
var
  vrStr : TStrings;
begin
  Result := EmptyStr;

  vrStr := TStringList.Create;
  try
    vrStr.LoadFromFile(prFileName);

    Result := vrStr.Text;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterParams.CheckFile;
var
  vrStr : TStrings;
begin
  if FileExists('./datasets/Actions.json') then
    Exit;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add('{');
    vrStr.Add('   "itens":[');
    vrStr.Add('   ]');
    vrStr.Add('}');

    vrStr.SaveToFile('./datasets/Actions.json');
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterParams.List;
var
  vrData     : TJSONData;
  vrArray    : TJSONArray;
  vrItem     : TJSONObject;
  vrVez      : Integer;
  vrObj      : TJupiterAction;
begin
  vrData := GetJSON(Self.Internal_GetFileContent('./datasets/Actions.json'));

  vrArray := TJSONArray(vrData.FindPath('itens'));

  for vrVez := 0 to vrArray.Count -1 do
  begin
    vrItem := vrArray.Objects[vrVez];

    vrObj                := TJupiterAction.Create;
    vrObj.Title          := vrItem['title'].AsString;
    vrObj.Icon           := vrItem['icon'].AsInteger;
    vrObj.RunnableAction := vrItem['runnableAction'].AsString;
    vrObj.Filter         := vrItem['filter'].AsString;
    vrObj.Params         := vrItem['params'].AsString;
    vrObj.Flags          := vrItem['flags'].AsString;
    vrObj.Category       := vrItem['category'].AsString;

    {
    vrObj.Action     := vrItem['action'].AsString;
    vrObj.ListAction := vrItem['listAction'].AsString;
    vrObj.OptionPath := vrItem['optionPath'].AsString;
    vrObj.Param      := vrItem['param'].AsString;
    vrObj.Tags       := vrItem['tags'].AsString;
    }

    Self.FOnAddParam(Self, vrObj);
  end;
end;

end.

