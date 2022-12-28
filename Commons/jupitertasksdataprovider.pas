unit JupiterTasksDataProvider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterDataProvider, JupiterConsts;

type

  { TJupiterTasksDataProvider }

  TJupiterTasksDataProvider = class(TJupiterDataProvider)
  protected
    procedure Internal_Search(prPath, prClient : String); virtual;
  public
    procedure ProvideData; override;

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

procedure TJupiterTasksDataProvider.ProvideData;
var
  vrInfo : TSearchRec;
  vrPath : String;
begin
  inherited ProvideData;

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

class procedure TJupiterTasksDataProvider.GetFieldsLayout(var prList: TStrings);
begin
  inherited GetFieldsLayout(prList);

  prList.Add('Client');
  prList.Add('Task');
  prList.Add('Path');
end;

end.

