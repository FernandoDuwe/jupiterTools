unit uNewTask;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CheckLst, uJupiterForm, FileUtil, fileUtils, JupiterApp, JupiterTasks;

type

  { TFNewTask }

  TFNewTask = class(TJupiterForm)
    btDo: TButton;
    cbClient: TComboBox;
    cbFiles: TCheckListBox;
    cbCurrent: TCheckBox;
    cbStartTimer: TCheckBox;
    edTaskName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btDoClick(Sender: TObject);
    procedure cbCurrentChange(Sender: TObject);
  private
    function Internal_GetPath: String;
    function Internal_GetTemplatePath : String;

    procedure Internal_UpdateDatasets; override;

    procedure Internal_CopyFile(prFile : String);
  published
    property TaskPath : String read Internal_GetPath;
  public

  end;

var
  FNewTask: TFNewTask;

implementation

{$R *.lfm}

{ TFNewTask }

procedure TFNewTask.btDoClick(Sender: TObject);
var
  vrVez : Integer;
  vrStrClients : TStrings;
begin
  if Trim(edTaskName.Text) = EmptyStr then
    raise Exception.Create('O campo nome Tarefa é obrigatório.');

  vrStrClients := TStringList.Create;
  try
    if not DirectoryExists(vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.Path').Value + GetDirectorySeparator + cbClient.Text) then
      CreateDir(vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.Path').Value + GetDirectorySeparator + cbClient.Text);

    if not DirectoryExists(Self.TaskPath) then
      CreateDir(Self.TaskPath);

    if cbCurrent.Checked then
    begin
      vrJupiterApp.Config.AddConfig('JupiterTools.Modules.Tasks.Current',
                                    Self.TaskPath,
                                    'Tarefa atual');

      vrJupiterApp.Config.AddConfig('JupiterTools.Modules.Tasks.CurrentNumber',
                                    edTaskName.Text,
                                    'Número Tarefa atual');

      if cbStartTimer.Checked then
        TJupiterTasks(vrJupiterApp.GetModuleByID('JupiterTools.Modules.Tasks')).CreateTaskDetail.TimeNote.Start;
    end;

    vrStrClients.Clear;

    ListDirectories(Self.Internal_GetTemplatePath, vrStrClients);

    for vrVez := 0 to vrStrClients.Count - 1 do
       CreateDir(Self.TaskPath + StringReplace(vrStrClients[vrVez], Self.Internal_GetTemplatePath, EmptyStr, [rfIgnoreCase, rfReplaceAll]));

    for vrVez := 0 to cbFiles.Items.Count - 1 do
      if cbFiles.Checked[vrVez] then
        Self.Internal_CopyFile(cbFiles.Items[vrVez]);
  finally
    vrStrClients.Clear;
    FreeAndNil(vrStrClients);

    vrJupiterApp.Log.AddLog(Now, Self.Caption, 'Criada tarefa: ' + cbClient.Text + '/' + edTaskName.Text);
  end;

  Self.Close;
end;

procedure TFNewTask.cbCurrentChange(Sender: TObject);
begin
  cbStartTimer.Enabled := cbCurrent.Checked;
end;

function TFNewTask.Internal_GetPath: String;
begin
  if Copy(vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.Path').Value, Length(vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.Path').Value), 1) = GetDirectorySeparator then
    Result := vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.Path').Value + cbClient.Text + GetDirectorySeparator + edTaskName.Text + GetDirectorySeparator
  else
    Result := vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.Path').Value + GetDirectorySeparator + cbClient.Text + GetDirectorySeparator + edTaskName.Text + GetDirectorySeparator;
end;

function TFNewTask.Internal_GetTemplatePath: String;
begin
  Result := ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules' +  GetDirectorySeparator + 'tasks' + GetDirectorySeparator + 'templates' + GetDirectorySeparator;
end;

procedure TFNewTask.Internal_UpdateDatasets;
var
  vrStrClients : TStrings;
  vrStrSubClients : TStrings;
  vrVez : Integer;
  vrVez2 : Integer;
begin
  inherited Internal_UpdateDatasets;

  cbClient.Text := EmptyStr;
  cbClient.Items.Clear;

  vrStrClients := TStringList.Create;
  vrStrSubClients := TStringList.Create;
  try
    ListDirectories(vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.Path').Value, vrStrClients);

    for vrVez := 0 to vrStrClients.Count - 1 do
    begin
      vrStrClients[vrVez] := StringReplace(vrStrClients[vrVez], vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.Path').Value, EmptyStr, [rfIgnoreCase, rfReplaceAll]);
      vrStrClients[vrVez] := StringReplace(vrStrClients[vrVez], GetDirectorySeparator, EmptyStr, [rfIgnoreCase, rfReplaceAll]);
      vrStrClients[vrVez] := Trim(vrStrClients[vrVez]);
    end;

    cbClient.Items.AddStrings(vrStrClients);

    if vrStrClients.Count > 0 then
      cbClient.ItemIndex := 0;

    vrStrClients.Clear;

    ListFiles(Self.Internal_GetTemplatePath, vrStrClients);

    for vrVez := 0 to vrStrClients.Count - 1 do
    begin
      vrStrClients[vrVez] := StringReplace(vrStrClients[vrVez], Self.Internal_GetTemplatePath, EmptyStr, [rfIgnoreCase, rfReplaceAll]);
      vrStrClients[vrVez] := StringReplace(vrStrClients[vrVez], GetDirectorySeparator, EmptyStr, [rfIgnoreCase, rfReplaceAll]);
      vrStrClients[vrVez] := Trim(vrStrClients[vrVez]);
    end;

    cbFiles.Items.Clear;
    cbFiles.Items.AddStrings(vrStrClients);

    vrStrClients.Clear;
    vrStrSubClients.Clear;

    ListDirectories(Self.Internal_GetTemplatePath, vrStrClients);

    for vrVez := 0 to vrStrClients.Count -1 do
    begin
      vrStrSubClients.Clear;

      ListFiles(vrStrClients[vrVez] + GetDirectorySeparator, vrStrSubClients);

      for vrVez2 := 0 to vrStrSubClients.Count - 1 do
      begin
        vrStrSubClients[vrVez2] := StringReplace(vrStrSubClients[vrVez2], Self.Internal_GetTemplatePath, EmptyStr, [rfIgnoreCase, rfReplaceAll]);
        vrStrSubClients[vrVez2] := StringReplace(vrStrSubClients[vrVez2], GetDirectorySeparator + GetDirectorySeparator, GetDirectorySeparator, [rfIgnoreCase, rfReplaceAll]);
        vrStrSubClients[vrVez2] := Trim(vrStrSubClients[vrVez2]);
      end;

      cbFiles.Items.AddStrings(vrStrSubClients);
    end;

    for vrVez := 0 to cbFiles.Count - 1 do
      cbFiles.Checked[vrVez] := True;
  finally
    vrStrClients.Clear;
    FreeAndNil(vrStrClients);

    vrStrSubClients.Clear;
    FreeAndNil(vrStrSubClients);
  end;
end;

procedure TFNewTask.Internal_CopyFile(prFile: String);
var
  vrVez  : Integer;
  vrStr  : TStrings;
  vrFile : String;

  vrExtensions : String;
  vrContent : Integer;
begin
  vrFile := Self.TaskPath + vrJupiterApp.Config.ResolveString(prFile);

  CopyFile(Self.Internal_GetTemplatePath + prFile, vrFile);

  vrExtensions := vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.DoNotChangeContentExtensions').Value;

  vrContent := Pos(AnsiUpperCase(ExtractFileExt(prFile)), AnsiUpperCase(vrExtensions));

  if vrContent <> 0 then
    Exit;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.LoadFromFile(vrFile);

    for vrVez := 0 to vrStr.Count - 1 do
      vrStr[vrVez] := vrJupiterApp.Config.ResolveString(vrStr[vrVez]);

    vrStr.SaveToFile(vrFile);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

end.

