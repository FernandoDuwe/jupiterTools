unit JupiterXMLDataProvider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterDataProvider, JupiterConsts, JupiterVariable;

type

  { TJupiterXMLDataProvider }

  TJupiterXMLDataProvider = class(TJupiterDataProvider)
  private
    FFilename : String;
  published
    property Filename : String read FFilename write FFilename;
  public
    procedure ProvideData; override;
  end;

implementation

{ TJupiterXMLDataProvider }

procedure TJupiterXMLDataProvider.ProvideData;
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

end.

