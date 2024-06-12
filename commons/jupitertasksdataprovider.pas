unit JupiterTasksDataProvider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterDataProvider, JupiterConsts, JupiterEnviroment,
  JupiterCSVDataProvider;

type

  { TJupiterTasksDataProvider }

  TJupiterTasksDataProvider = class(TJupiterDataProvider)
  protected
    procedure Internal_Search(prPath, prClient : String); virtual;
    procedure Internal_GetDataFromCache(prFileCachePath : String);
  public
    procedure ProvideData; override;
    procedure CreateCacheData;

    class procedure GetFieldsLayout(var prList : TStrings); override;
  end;

implementation

uses JupiterApp;

{ TJupiterTasksDataProvider }

procedure TJupiterTasksDataProvider.Internal_Search(prPath, prClient: String);
var
  vrInfo : TSearchRec;
begin
  if FindFirst(prPath + '*', faDirectory, vrInfo) = 0 then
    repeat
      if (((vrInfo.Name = '.') or (vrInfo.Name = '..')) or (vrInfo.Name = EmptyStr)) then
        Continue;

      if not DirectoryExists(prPath + vrInfo.Name) then
        Continue;

      if DirectoryExists(prPath + vrInfo.Name) then
      begin
        Self.AddRow;
        Self.GetLastRow.Fields.AddVariable('Client', prClient, 'Cliente');
        Self.GetLastRow.Fields.AddVariable('Task', vrInfo.Name, 'Tarefa');
        Self.GetLastRow.Fields.AddVariable('Path', Format('%0:s%1:s%2:s', [prPath, vrInfo.Name, GetDirectorySeparator]), 'Caminho');
      end;
    until FindNext(vrInfo) <> 0;

  FindClose(vrInfo);
end;

procedure TJupiterTasksDataProvider.Internal_GetDataFromCache(prFileCachePath : String);
var
  vrCSV : TJupiterCSVDataProvider;
  vrVez : Integer;
  vrVez2 : Integer;
begin
  vrCSV :=  TJupiterCSVDataProvider.Create;
  try
    vrCSV.Filename := prFileCachePath;
    vrCSV.ProvideData;

    for vrVez := 0 to vrCSV.Count - 1 do
      with vrCSV.GetRowByIndex(vrVez) do
      begin
        Self.AddRow;

        for vrVez2 := 0 to Fields.Count -1 do
          Self.GetLastRow.Fields.AddVariable(Fields.VariableByIndex(vrVez2).ID,
                                             Fields.VariableByIndex(vrVez2).Value,
                                             Fields.VariableByIndex(vrVez2).Title);
      end;
  finally
    FreeAndNil(vrCSV);
  end;
end;

procedure TJupiterTasksDataProvider.ProvideData;
var
  vrInfo       : TSearchRec;
  vrPath       : String;
  vrEnviroment : TJupiterEnviroment;
begin
  inherited ProvideData;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    if FileExists(vrEnviroment.FullPath('/temp/taskDataProvider.cache')) then
    begin
      Self.Internal_GetDataFromCache(vrEnviroment.FullPath('/temp/taskDataProvider.cache'));

      Exit;
    end;
  finally
    FreeAndNil(vrEnviroment);
  end;

  vrPath := vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools').Params.VariableById('Jupiter.Tools.Tasks.Path').Value;

  if FindFirst(vrPath + '*', faDirectory, vrInfo) = 0 then
    repeat
      if (((vrInfo.Name = '.') or (vrInfo.Name = '..')) or (vrInfo.Name = EmptyStr)) then
        Continue;

      if not DirectoryExists(vrPath + vrInfo.Name) then
        Continue;

      if DirectoryExists(vrPath + vrInfo.Name) then
        Self.Internal_Search(Format('%0:s%1:s%2:s', [vrPath, vrInfo.Name, GetDirectorySeparator]), vrInfo.Name);
    until FindNext(vrInfo) <> 0;

  FindClose(vrInfo);
end;

procedure TJupiterTasksDataProvider.CreateCacheData;
var
  vrStr        : TStrings;
  vrVez        : Integer;
  vrVez2       : Integer;
  vrLine       : String;
  vrEnviroment : TJupiterEnviroment;
begin
  vrStr := TStringList.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    if FileExists(vrEnviroment.FullPath('/temp/taskDataProvider.cache')) then
      DeleteFile(vrEnviroment.FullPath('/temp/taskDataProvider.cache'));

    ProvideData;

    vrStr.Clear;
    vrStr.Add('Client;Task;Path;');

    for vrVez := 0 to Self.Count - 1 do
    begin
      vrLine := EmptyStr;

      with Self.GetRowByIndex(vrVez) do
      begin
        for vrVez2 := 0 to Fields.Count - 1 do
          vrLine := vrLine + Fields.VariableByIndex(vrVez2).Value + ';';
      end;

      vrStr.Add(vrLine);
    end;

    vrStr.SaveToFile(vrEnviroment.FullPath('/temp/taskDataProvider.cache'));
  finally
    FreeAndNil(vrStr);
    FreeAndNil(vrEnviroment);
  end;
end;

class procedure TJupiterTasksDataProvider.GetFieldsLayout(var prList: TStrings);
begin
  inherited GetFieldsLayout(prList);

  prList.Add('Client');
  prList.Add('Task');
  prList.Add('Path');
end;

end.

