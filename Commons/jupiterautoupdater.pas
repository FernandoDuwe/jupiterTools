unit jupiterautoupdater;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterConsts, jupiterScript, JupiterObject,
  JupiterXMLDataProvider;

type

  { TJupiterAutoUpdater }

  TJupiterAutoUpdater = class(TJupiterObject)
  private
    FFileName : String;
    FVersion : String;
    FSystem : String;

    procedure Internal_SetFileName(prNewFileName : String);
  published
    property FileName : String read FFileName write Internal_SetFileName;
  public
    procedure Execute;
  end;

implementation

{ TJupiterAutoUpdater }

procedure TJupiterAutoUpdater.Internal_SetFileName(prNewFileName: String);
var
  vrXML : TJupiterXMLDataProvider;
begin
  Self.FFileName := prNewFileName;

  vrXML := TJupiterXMLDataProvider.Create;
  try
    vrXML.SearchNode := 'header';
    vrXML.Filename := prNewFileName;
    vrXML.ProvideData;

    Self.FSystem  := vrXML.GetRowByIndex(0).Fields.VariableById('system').Value;
    Self.FVersion := vrXML.GetRowByIndex(0).Fields.VariableById('version').Value;
  finally
    FreeAndNil(vrXML);
  end;
end;

procedure TJupiterAutoUpdater.Execute;
begin
  WriteLn('System: ' + Self.FSystem);
  WriteLn('Version: ' + Self.FVersion);
  WriteLn(EmptyStr);
  WriteLn('Downloading files...');
  WriteLn(EmptyStr);
  WriteLn('Coping files...');
  WriteLn(EmptyStr);
  WriteLn('Running files...');
  WriteLn(EmptyStr);
  WriteLn('Deleting files...');
end;

end.

