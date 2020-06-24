unit JupiterParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TJupiterParams }
  TJupiterParamsNode = class(TObject)
  public
    OptionPath : String;
    Action     : String;
    Param      : String;
    Tags       : String;
  end;

  TJupiterParams = class(TObject)
  public
    procedure CheckFile;
  end;

implementation

{ TJupiterParams }

procedure TJupiterParams.CheckFile;
var
  vrStr : TStrings;
begin
  if FileExists('./datasets/Params.json') then
    Exit;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add('{');
    vrStr.Add('   "itens":[');
    vrStr.Add('   ]');
    vrStr.Add('}');

    vrStr.SaveToFile('./datasets/Params.json');
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

end.

