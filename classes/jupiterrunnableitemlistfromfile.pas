unit JupiterRunnableItemListFromFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JupiterRunnableItem, jupiterconsts;

type

  { TJupiterRunnableItemListFromFile }

  TJupiterRunnableItemListFromFile = class(TJupiterRunnableItem)
  public
    procedure Internal_CheckFile;
    procedure Internal_Execute; override;
    function  Internal_GetFileContent(prFileName : String) : String;

    class function ListAction : String; override; static;
  end;

implementation

uses fpjson, JsonParser;

{ TJupiterRunnableItemListFromFile }

procedure TJupiterRunnableItemListFromFile.Internal_CheckFile;
var
  vrStr : TStrings;
begin
  if FileExists('./datasets/' + Self.Param.OptionPath) then
    Exit;

  {
     Title       : String;
     Description : String;
     Param       : String;
     Icon        : Integer;
  }

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add('{');
    vrStr.Add('   "itens":[{');
    vrStr.Add('       "title": "New Item",');
    vrStr.Add('       "description": "Item description",');
    vrStr.Add('       "param": "",');
    vrStr.Add('       "icon": -1');
    vrStr.Add('     }');
    vrStr.Add('   ]');
    vrStr.Add('}');

    vrStr.SaveToFile('./datasets/' + Self.Param.OptionPath);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterRunnableItemListFromFile.Internal_Execute;
var
  vrData     : TJSONData;
  vrArray    : TJSONArray;
  vrItem     : TJSONObject;
  vrVez      : Integer;
begin
  Self.Internal_CheckFile;

  inherited Internal_Execute;

  vrData := GetJSON(Self.Internal_GetFileContent('./datasets/' + Self.Param.OptionPath));

  vrArray := TJSONArray(vrData.FindPath('itens'));

  for vrVez := 0 to vrArray.Count -1 do
  begin
    vrItem := vrArray.Objects[vrVez];

    Self.Internal_AddItem(TJupiterListItem.Create(vrItem['title'].AsString,
                                                  vrItem['description'].AsString,
                                                  vrItem['param'].AsString,
                                                  vrItem['icon'].AsInteger));
  end;
end;

function TJupiterRunnableItemListFromFile.Internal_GetFileContent(prFileName: String): String;
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

class function TJupiterRunnableItemListFromFile.ListAction: String;
begin
  Result := 'ListFromFile';
end;

end.

