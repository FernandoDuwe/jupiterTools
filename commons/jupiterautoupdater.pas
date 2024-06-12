unit jupiterautoupdater;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterConsts, jupiterScript, JupiterObject,
  JupiterXMLDataProvider, JupiterEnviroment, jupiterfiledownloader;

type

  { TJupiterAutoUpdater }

  TJupiterAutoUpdater = class(TJupiterObject)
  private
    FFileName : String;
    FVersion : String;
    FSystem : String;

    procedure Internal_SetFileName(prNewFileName : String);

    procedure Internal_DoProgress(Sender: TObject; Percent: Integer);

    procedure Internal_DoDownloadFiles;
    procedure Internal_DoCopyFiles;
    procedure Internal_DoRunFiles;
    procedure Internal_DoDeleteFiles;
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

procedure TJupiterAutoUpdater.Internal_DoProgress(Sender: TObject; Percent: Integer);
begin
  WriteLn('  ' + IntToStr(Percent) + ' %');
end;

procedure TJupiterAutoUpdater.Internal_DoDownloadFiles;
var
  vrXML : TJupiterXMLDataProvider;
  vrVez : Integer;
  vrEnviroment : TJupiterEnviroment;
begin
  vrXML := TJupiterXMLDataProvider.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrXML.SearchNode := 'downloadListItem';
    vrXML.Filename := Self.FileName;
    vrXML.ProvideData;

    for vrVez := 0 to vrXML.Count - 1 do
      with vrXML.GetRowByIndex(vrVez) do
      begin
        WriteLn('Downloading file: ' + Fields.VariableById('name').Value);

        vrEnviroment.CreatePath('temp/autoupdater/' + Fields.VariableById('path').Value);

        WriteLn(Fields.VariableById('downloadPath').Value);
        WriteLn(vrEnviroment.FullPath('temp/autoupdater/' + Fields.VariableById('path').Value + Fields.VariableById('name').Value));

        DonwloadFile(@Self.Internal_DoProgress,
                     Fields.VariableById('downloadPath').Value,
                     vrEnviroment.FullPath('temp/autoupdater/' + Fields.VariableById('path').Value + Fields.VariableById('name').Value));

        WriteLn(EmptyStr);
      end;
  finally
    FreeAndNil(vrXML);
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterAutoUpdater.Internal_DoCopyFiles;
begin
  //
end;

procedure TJupiterAutoUpdater.Internal_DoRunFiles;
begin
  //
end;

procedure TJupiterAutoUpdater.Internal_DoDeleteFiles;
begin
  //
end;

procedure TJupiterAutoUpdater.Execute;
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;

  try
    vrEnviroment.CreatePath('temp/autoupdater/');

    WriteLn('System: ' + Self.FSystem);
    WriteLn('Version: ' + Self.FVersion);
    WriteLn(EmptyStr);
    WriteLn('Downloading files...');
    WriteLn(EmptyStr);
    Self.Internal_DoDownloadFiles;

    WriteLn(EmptyStr);
    WriteLn('Coping files...');

    Self.Internal_DoCopyFiles;

    WriteLn(EmptyStr);
    WriteLn('Running files...');

    Self.Internal_DoRunFiles;

    WriteLn(EmptyStr);
    WriteLn('Deleting files...');

    Self.Internal_DoDeleteFiles;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

end.

