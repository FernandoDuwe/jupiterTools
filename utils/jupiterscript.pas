unit jupiterScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterApp;

type

  { TJupiterScript }

  TJupiterScript = class(TObject)
  private
    FFileName : String;
  published
    property FileName : String read FFileName write FFileName;
  public
    procedure ResolveAndSave(prNewFile : String);
  end;

implementation

{ TJupiterScript }

procedure TJupiterScript.ResolveAndSave(prNewFile: String);
var
  vrStr : TStrings;
  vrVez : Integer;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.LoadFromFile(Self.FileName);

    for vrVez := 0 to vrStr.Count -1 do
      vrStr[vrVez] := vrJupiterApp.Config.ResolveString(vrStr[vrVez]);

    vrStr.SaveToFile(prNewFile);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

end.

