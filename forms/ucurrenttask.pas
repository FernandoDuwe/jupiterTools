unit uCurrentTask;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, uCustomJupiterForm, JupiterAction, JupiterConsts, JupiterApp,
  JupiterRunnable, JupiterFileDataProvider, JupiterTaskTimesDataProvider,
  JupiterDirectoryDataProvider, JupiterForm, JupiterEnviroment,
  JupiterToolsModule, LCLType;

type

  { TFCurrentTask }

  TFCurrentTask = class(TFCustomJupiterForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbCurrentProject: TLabel;
    lbCurrentTask: TLabel;
    lvTimes: TListView;
    pnCurrentTask: TPanel;
    tvExplorer: TTreeView;
    procedure FormShow(Sender: TObject);
    procedure lvFilesDblClick(Sender: TObject);
  private
    procedure Internal_MarcarTempoInicial(Sender : TObject);
    procedure Internal_MarcarTempoFinal(Sender : TObject);
    procedure Internal_LimparTempos(Sender : TObject);
    procedure Internal_ListFiles(prTreeNode : TTreeNode; prDirectory : String);
    procedure Internal_ListDirectories(prTreeNode : TTreeNode; prDirectory : String);
  protected
    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
  public

  end;

var
  FCurrentTask: TFCurrentTask;

implementation

uses uMain;

{$R *.lfm}

{ TFCurrentTask }

procedure TFCurrentTask.lvFilesDblClick(Sender: TObject);
begin
  if not Assigned(tvExplorer.Selected) then
    Exit;

  if not Assigned(tvExplorer.Selected.Data) then
    Exit;

  TJupiterRunnable(tvExplorer.Selected.Data).Execute;
end;

procedure TFCurrentTask.FormShow(Sender: TObject);
begin
  inherited;

  pnCurrentTask.Height := sbBody.Height - 5;
end;

procedure TFCurrentTask.Internal_MarcarTempoInicial(Sender: TObject);
begin
  try
    TJupiterToolsModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools')).SetStartTime;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFCurrentTask.Internal_MarcarTempoFinal(Sender: TObject);
begin
  try
    TJupiterToolsModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools')).SetEndTime;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFCurrentTask.Internal_LimparTempos(Sender: TObject);
begin
  try
    TJupiterToolsModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools')).ClearTime;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFCurrentTask.Internal_ListFiles(prTreeNode: TTreeNode; prDirectory: String);
var
  vrFile       : TJupiterFileDataProvider;
  vrVez        : Integer;
  vrNode       : TTreeNode;
  vrEnviroment : TJupiterEnviroment;
begin
  Self.Internal_ListDirectories(prTreeNode, prDirectory);

  vrEnviroment := TJupiterEnviroment.Create;
  vrFile       := TJupiterFileDataProvider.Create;
  try
    vrFile.Path := prDirectory;
    vrFile.ProvideData;

    for vrVez := 0 to vrFile.Size - 1 do
      with vrFile.GetRowByIndex(vrVez) do
      begin
        if Assigned(prTreeNode) then
          vrNode := tvExplorer.Items.AddChild(prTreeNode, Fields.VariableById('FieldName').Value)
        else
          vrNode := tvExplorer.Items.Add(nil, Fields.VariableById('FieldName').Value);

        vrNode.ImageIndex    := vrEnviroment.IconOfFile(Fields.VariableById('File').Value);
        vrNode.SelectedIndex := vrEnviroment.IconOfFile(Fields.VariableById('File').Value);

        vrNode.Data := TJupiterRunnable.Create(Fields.VariableById('File').Value);
      end;
  finally
    FreeAndNil(vrFile);
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFCurrentTask.Internal_ListDirectories(prTreeNode: TTreeNode; prDirectory: String);
var
  vrProvider   : TJupiterDirectoryDataProvider;
  vrNode       : TTreeNode;
  vrVez        : Integer;
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  vrProvider   := TJupiterDirectoryDataProvider.Create;
  try
    vrProvider.SubFolders := False;

    vrProvider.Path := prDirectory;

    vrProvider.ProvideData;

    for vrVez := 0 to vrProvider.Size - 1 do
      with vrProvider.GetRowByIndex(vrVez) do
      begin
        if Assigned(prTreeNode) then
          vrNode := tvExplorer.Items.AddChild(prTreeNode, Fields.VariableById('Folder').Value)
        else
          vrNode := tvExplorer.Items.Add(nil, Fields.VariableById('Folder').Value);

        vrNode.ImageIndex    := vrEnviroment.IconOfFile(Fields.VariableById('Path').Value);
        vrNode.SelectedIndex := vrEnviroment.IconOfFile(Fields.VariableById('Path').Value);

        vrNode.Data := TJupiterRunnable.Create(Fields.VariableById('Path').Value);

        Self.Internal_ListFiles(vrNode, Fields.VariableById('Path').Value);
      end;
  finally
    FreeAndNil(vrProvider);
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFCurrentTask.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  with TJupiterToolsModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools')) do
    Self.Actions.Add(TJupiterAction.Create('Abrir tarefa', TJupiterRunnable.Create(Params.VariableById('Jupiter.Tools.Tasks.Current.Path').Value)));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para abrir o diretório da tarefa atual';
    Icon := ICON_PLAY;
  end;

  Self.Actions.Add(TJupiterAction.Create('Marcar tempo inicial', @Internal_MarcarTempoInicial));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para marcar o tempo inicial da sua tarefa';
    Icon := ICON_STARTTIME;
  end;

  Self.Actions.Add(TJupiterAction.Create('Marcar tempo final', @Internal_MarcarTempoFinal));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para marcar o tempo final da sua tarefa';
    Icon := ICON_ENDTIME;
  end;

  Self.Actions.Add(TJupiterAction.Create('Limpar Tempos Registrados', @Internal_LimparTempos));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui limpar todos os tempos registrados';
    Icon := ICON_DELETE;

    ConfirmBeforeExecute := True;
  end;

  Self.Params.AddVariable(FIELD_ID_GENERADOR, 'CurrentTaskForm', 'ID do formulário');
end;

procedure TFCurrentTask.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  tvExplorer.Font.Size := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);
  lvTimes.Font.Size := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);

  with TJupiterToolsModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools')) do
  begin
    lbCurrentProject.Caption := Params.VariableById('Jupiter.Tools.Tasks.Current.Client').Value;
    lbCurrentTask.Caption    := Params.VariableById('Jupiter.Tools.Tasks.Current.Number').Value;

    Self.Actions.EnableDisableAction(1, sbActions, not StartedTime);
    Self.Actions.EnableDisableAction(2, sbActions, StartedTime);
  end;
end;

procedure TFCurrentTask.Internal_UpdateDatasets;
var
  vrTempos   : TJupiterTaskTimesDataProvider;
  vrVez      : Integer;
  vrTimeNode : TListItem;
begin
  inherited Internal_UpdateDatasets;

  tvExplorer.Items.Clear;

  with TJupiterToolsModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools')) do
    Self.Internal_ListFiles(nil, Params.VariableById('Jupiter.Tools.Tasks.Current.Path').Value);

  vrTempos := TJupiterTaskTimesDataProvider.Create;
  try
    with TJupiterToolsModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools')) do
      vrTempos.FileName := Params.VariableById('Jupiter.Tools.Tasks.Current.Path').Value + 'Tempos.txt';

    if FileExists(vrTempos.FileName) then
      vrTempos.ProvideData;

    lvTimes.Items.Clear;

    for vrVez := 0 to vrTempos.Size - 1 do
      with vrTempos.GetRowByIndex(vrVez) do
      begin
        vrTimeNode := lvTimes.Items.Add;
        vrTimeNode.Caption := Fields.VariableById('startTime').Value + COLUMN_SPACE_SEPARATOR;
        vrTimeNode.SubItems.Add(Fields.VariableById('endTime').Value + COLUMN_SPACE_SEPARATOR);
      end;
  finally
    FreeAndNil(vrTempos);
  end;
end;

end.

