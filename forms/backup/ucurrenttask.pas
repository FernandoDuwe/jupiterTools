unit uCurrentTask;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, uCustomJupiterForm, JupiterAction, JupiterConsts, JupiterApp,
  JupiterRunnable, JupiterFileDataProvider, JupiterTaskTimesDataProvider,
  JupiterToolsModule;

type

  { TFCurrentTask }

  TFCurrentTask = class(TFCustomJupiterForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbCurrentProject: TLabel;
    lbCurrentTask: TLabel;
    lvFiles: TListView;
    lvTimes: TListView;
    pnCurrentTask: TPanel;
    procedure lvFilesDblClick(Sender: TObject);
  private
    procedure Internal_MarcarTempoInicial(Sender : TObject);
    procedure Internal_MarcarTempoFinal(Sender : TObject);
    procedure Internal_LimparTempos(Sender : TObject);
  protected
    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
  public

  end;

var
  FCurrentTask: TFCurrentTask;

implementation

{$R *.lfm}

{ TFCurrentTask }

procedure TFCurrentTask.lvFilesDblClick(Sender: TObject);
begin
  if not Assigned(lvFiles.Selected) then
    Exit;

  if not Assigned(lvFiles.Selected.Data) then
    Exit;

  TJupiterRunnable(lvFiles.Selected.Data).Execute;
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
  //
end;

procedure TFCurrentTask.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.Params.AddVariable('Generator.FormId', 'CurrentTaskForm', 'Título do formulário');

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
end;

procedure TFCurrentTask.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  with TJupiterToolsModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools')) do
  begin
    lbCurrentProject.Caption := Params.VariableById('Jupiter.Tools.Tasks.Current.Client').Value;
    lbCurrentTask.Caption    := Params.VariableById('Jupiter.Tools.Tasks.Current.Number').Value;

    Self.Actions.GetActionButton(1, sbActions).Enabled := not StartedTime;
    Self.Actions.GetActionButton(2, sbActions).Enabled := StartedTime;
  end;
end;

procedure TFCurrentTask.Internal_UpdateDatasets;
var
  vrProvider : TJupiterFileDataProvider;
  vrTempos   : TJupiterTaskTimesDataProvider;
  vrVez      : Integer;
  vrNode     : TListItem;
begin
  inherited Internal_UpdateDatasets;

  vrProvider := TJupiterFileDataProvider.Create;
  try
    vrProvider.SubFolders := True;

    with TJupiterToolsModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools')) do
      vrProvider.Path := Params.VariableById('Jupiter.Tools.Tasks.Current.Path').Value;

    vrProvider.ProvideData;

    lvFiles.Items.Clear;

    for vrVez := 0 to vrProvider.Size - 1 do
      with vrProvider.GetRowByIndex(vrVez) do
      begin
        vrNode := lvFiles.Items.Add;
        vrNode.Caption := Fields.VariableById('FieldName').Value + COLUMN_SPACE_SEPARATOR;
        vrNode.SubItems.Add(Fields.VariableById('File').Value + COLUMN_SPACE_SEPARATOR);
        vrNode.Data := TJupiterRunnable.Create(Fields.VariableById('File').Value);
      end;
  finally
    FreeAndNil(vrProvider);
  end;

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
        vrNode := lvTimes.Items.Add;
        vrNode.Caption := Fields.VariableById('startTime').Value + COLUMN_SPACE_SEPARATOR;
        vrNode.SubItems.Add(Fields.VariableById('endTime').Value + COLUMN_SPACE_SEPARATOR);
      end;
  finally
    FreeAndNil(vrTempos);
  end;
end;

end.

