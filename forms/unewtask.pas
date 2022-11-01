unit uNewTask;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, CheckLst,
  ExtCtrls, uCustomJupiterForm, JupiterConsts, JupiterAction,
  JupiterDirectoryDataProvider, JupiterApp, JupiterFileDataProvider,
  JupiterEnviroment, JupiterRoute, JupiterToolsModule;

type

  { TFNewTask }

  TFNewTask = class(TFCustomJupiterForm)
    cbClient: TComboBox;
    cbCurrent: TCheckBox;
    cbFiles: TCheckListBox;
    cbStartTimer: TCheckBox;
    edTaskName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    procedure FormShow(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
  private
    procedure Internal_SaveConfigClick(Sender : TObject);

    procedure Internal_ListClientes;
    procedure Internal_ListFiles;
  protected
    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateDatasets; override;
  public

  end;

var
  FNewTask: TFNewTask;

implementation

uses LCLType;

{$R *.lfm}

{ TFNewTask }

procedure TFNewTask.Panel1Resize(Sender: TObject);
begin
  cbClient.Width   := (Panel1.Width - (FORM_MARGIN_LEFT + FORM_MARGIN_RIGHT));
  edTaskName.Width := (Panel1.Width - (FORM_MARGIN_LEFT + FORM_MARGIN_RIGHT));
  cbFiles.Width    := (Panel1.Width - (FORM_MARGIN_LEFT + FORM_MARGIN_RIGHT));
end;

procedure TFNewTask.FormShow(Sender: TObject);
begin
  inherited;

  Panel1.Height := sbBody.Height - 5;
end;

procedure TFNewTask.Internal_SaveConfigClick(Sender: TObject);
var
  vrPath : String;
  vrVez  : Integer;
begin
  try
    if Trim(cbClient.Text) = EmptyStr then
      raise Exception.Create('O campo Cliente é obrigatório');

    if Trim(edTaskName.Text) = EmptyStr then
      raise Exception.Create('O campo Tarefa é obrigatório');

    if Assigned(Self.Generator.Variables) then
      Self.Generator.Variables.Validate;

    with TJupiterToolsModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools')) do
    begin
      vrPath := CreateTask(cbClient.Text, edTaskName.Text, cbCurrent.Checked);

      CreatePathScruture(vrPath);

      for vrVez := 0 to cbFiles.Items.Count - 1 do
        if cbFiles.Checked[vrVez] then
          CopyFileFromTemplate(vrPath, cbFiles.Items[vrVez]);

      if cbStartTimer.Checked then
        SetStartTime;
    end;

    vrJupiterApp.NavigateTo(TJupiterRoute.Create(TASK_FORM_PATH), False);
  except
    Application.MessageBox(PAnsiChar(Exception(ExceptObject).Message), PAnsiChar(Self.Caption), MB_ICONERROR + MB_OK);
  end;
end;

procedure TFNewTask.Internal_ListClientes;
var
  vrOldValueCliente : String;
  vrClienteProvider : TJupiterDirectoryDataProvider;
  vrVez : Integer;
begin
  vrOldValueCliente := cbClient.Text;

  vrClienteProvider := TJupiterDirectoryDataProvider.Create;
  try
    vrClienteProvider.Path := vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools').Params.VariableById('Jupiter.Tools.Tasks.Path').Value;
    vrClienteProvider.ProvideData;

    cbClient.Items.Clear;

    for vrVez := 0 to vrClienteProvider.Size - 1 do
      with vrClienteProvider.GetRowByIndex(vrVez) do
        cbClient.Items.Add(Fields.VariableById('Folder').Value);

    cbClient.ItemIndex := cbClient.Items.IndexOf(vrOldValueCliente);

    if cbClient.ItemIndex = NULL_KEY then
      cbClient.Text := vrOldValueCliente;
  finally
    FreeAndNil(vrClienteProvider);
  end;
end;

procedure TFNewTask.Internal_ListFiles;
var
  vrProvider   : TJupiterFileDataProvider;
  vrEnviroment : TJupiterEnviroment;
  vrVez        : Integer;
  vrStrAux     : String;
  vrLastIndex  : Integer;
begin
  vrLastIndex := cbFiles.Items.Count;

  vrProvider   := TJupiterFileDataProvider.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrProvider.SubFolders := True;
    vrProvider.Path       := vrEnviroment.FullPath('modules/tools/templates/');
    vrProvider.ProvideData;

    for vrVez := 0 to vrProvider.Size - 1 do
      with vrProvider.GetRowByIndex(vrVez) do
      begin
        vrStrAux := Fields.VariableById('File').Value;
        vrStrAux := StringReplace(vrStrAux, vrEnviroment.FullPath('modules/tools/templates/'), EmptyStr, [rfIgnoreCase, rfReplaceAll]);

        if cbFiles.Items.IndexOf(vrStrAux) <> NULL_KEY then
          Continue;

        cbFiles.Items.Add(vrStrAux);
      end;

    for vrVez := vrLastIndex to cbFiles.Items.Count - 1 do
      cbFiles.Checked[vrVez] := True;
  finally
    FreeAndNil(vrProvider);
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFNewTask.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.Hint := 'Crie uma tarefa para controlar o seu trabalho';

  Self.Actions.Add(TJupiterAction.Create('Salvar', @Internal_SaveConfigClick));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para salvar as suas configurações';
    Icon := ICON_SAVE;
  end;

  Self.Params.AddVariable(FIELD_ID_GENERADOR, 'NewTaskForm', 'ID do formulário');
end;

procedure TFNewTask.Internal_UpdateDatasets;
begin
  inherited Internal_UpdateDatasets;

  Self.Internal_ListClientes;
  Self.Internal_ListFiles;
end;

end.

