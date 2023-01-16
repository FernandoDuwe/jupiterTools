unit uConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  uCustomJupiterForm, uNewDataSet, JupiterVariable, JupiterApp, JupiterModule,
  JupiterFormGenerator, JupiterAction, JupiterConsts, JupiterVariableForm,
  JupiterDialogForm, jupiterformutils;

type

  { TFConfig }

  TFConfig = class(TFCustomJupiterForm)
    Splitter1: TSplitter;
    tvNavigation: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvNavigationSelectionChanged(Sender: TObject);
  private
    FConfigGenerator : TJupiterFormGenerator;
    FOldVariant : TJupiterVariableList;
  protected
    procedure Internal_ListModules(prOwner : TTreeNode);
    procedure Internal_PrepareForm; override;
    procedure Internal_NewConfigClick(Sender : TObject);
    procedure Internal_NewDataSetClick(Sender : TObject);
    procedure Internal_SaveConfigClick(Sender : TObject);
  public

  end;

var
  FConfig: TFConfig;

implementation

{$R *.lfm}

{ TFConfig }

procedure TFConfig.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FOldVariant := nil;

  Self.FConfigGenerator := TJupiterFormGenerator.Create;
  Self.FConfigGenerator.ClearContainerOnSet := True;
  Self.FConfigGenerator.Container := sbBody;
end;

procedure TFConfig.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Self.FConfigGenerator);

  inherited;
end;

procedure TFConfig.FormShow(Sender: TObject);
begin
  inherited;

  tvNavigation.Width := PercentOfScreen(Self.Width, 30);

  tvNavigationSelectionChanged(Sender);
end;

procedure TFConfig.tvNavigationSelectionChanged(Sender: TObject);
begin
  if not Assigned(tvNavigation.Selected) then
    Exit;

  if not Assigned(tvNavigation.Selected.Data) then
    Exit;

  sbBody.Visible := False;
  try
    if Assigned(Self.FOldVariant) then
      Self.FOldVariant.CopyValues(Self.FConfigGenerator.Variables);

    if Assigned(Self.Actions.GetActionButton(1, sbActions)) then
      Self.Actions.EnableDisableAction(1, sbActions, TJupiterVariableList(tvNavigation.Selected.Data).Tag <> -3);

    if Assigned(Self.Actions.GetActionButton(2, sbActions)) then
      Self.Actions.EnableDisableAction(2, sbActions, TJupiterVariableList(tvNavigation.Selected.Data).Tag = -3);

    Self.FConfigGenerator.SetVariables(TJupiterVariableFormList.CreateFromVariableList(TJupiterVariableList(tvNavigation.Selected.Data)));
    Self.FOldVariant := TJupiterVariableList(tvNavigation.Selected.Data);
  finally
    sbBody.Visible := True;
  end;
end;

procedure TFConfig.Internal_ListModules(prOwner: TTreeNode);
var
  vrVez       : Integer;
  vrChild     : TTreeNode;
  vrVariables : TJupiterVariableList;
begin
  for vrVez := 0 to vrJupiterApp.ModulesList.Size - 1 do
    with TJupiterModule(vrJupiterApp.ModulesList.GetAtIndex(vrVez)) do
    begin
      vrChild := tvNavigation.Items.AddChild(prOwner, ModuleTitle);

      vrVariables := TJupiterVariableList.Create;
      vrVariables.CopyValues(Params);
      vrVariables.Tag := vrVez;

      vrChild.Data := vrVariables;
    end;
end;

procedure TFConfig.Internal_PrepareForm;
var
  vrNode : TTreeNode;
begin
  inherited Internal_PrepareForm;

  Self.Hint := 'Nesta tela você pode controlar todas as configurações utilizadas pelo Jupiter';

  Self.Actions.Add(TJupiterAction.Create('Salvar', @Internal_SaveConfigClick));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para salvar as suas configurações';
    Icon := ICON_SAVE;
  end;

  Self.Actions.Add(TJupiterAction.Create('Configuração', @Internal_NewConfigClick));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para inserir uma nova configuração';
    Icon := ICON_ADD;
  end;

  Self.Actions.Add(TJupiterAction.Create('Dataset', @Internal_NewDataSetClick));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para inserir um novo Dataset';
    Icon := ICON_ADD;
  end;

  tvNavigation.Items.Clear;

  vrNode := tvNavigation.Items.Add(nil, 'Geral');
  vrNode.Data := TJupiterVariableList.Create;

  TJupiterVariableList(vrNode.Data).CopyValues(vrJupiterApp.Params);
  TJupiterVariableList(vrNode.Data).Tag := -1;

  tvNavigation.Selected := vrNode;

  vrNode := tvNavigation.Items.Add(nil, 'Datasets');
  vrNode.Data := TJupiterVariableList.Create;

  TJupiterVariableList(vrNode.Data).CopyValues(vrJupiterApp.DataSetParams);
  TJupiterVariableList(vrNode.Data).Tag := -3;

  vrNode := tvNavigation.Items.Add(nil, 'Módulos');

  Self.Internal_ListModules(vrNode);

  vrNode := tvNavigation.Items.Add(nil, 'Usuário');
  vrNode.Data := TJupiterVariableList.Create;

  TJupiterVariableList(vrNode.Data).CopyValues(vrJupiterApp.UserParams);
  TJupiterVariableList(vrNode.Data).Tag := -2;

  tvNavigation.FullExpand;
end;

procedure TFConfig.Internal_NewConfigClick(Sender: TObject);
var
  vrDialog : TJupiterDialogForm;
  vrVez    : Integer;
begin
  vrDialog := TJupiterDialogForm.Create;
  try
    vrDialog.Title := 'Nova Configuração';
    vrDialog.Hint  := 'Crie uma nova configuração.';

    vrDialog.Fields.AddField('ID', 'Identificador', 'User.');
    vrDialog.Fields.AddField('DESC', 'Descrição', '');
    vrDialog.Fields.AddField('VALUE', 'Valor', '');

    vrDialog.Fields.VariableFormById('VALUE').Required := False;

    if vrDialog.Show then
    begin
      Self.FConfigGenerator.Variables.AddConfig(vrDialog.Fields.VariableFormById('ID').Value,
                                                vrDialog.Fields.VariableFormById('VALUE').Value,
                                                vrDialog.Fields.VariableFormById('DESC').Value);

      Self.FConfigGenerator.SetVariables(TJupiterVariableFormList.CreateFromVariableList(Self.FConfigGenerator.Variables));
    end;
  finally
    FreeAndNil(vrDialog);
  end;
end;

procedure TFConfig.Internal_NewDataSetClick(Sender: TObject);
begin
  Application.CreateForm(TFNewDataSet, FNewDataSet);
  try
    FNewDataSet.IsModal := True;

    if FNewDataSet.ShowModal = mrOK then
    begin
      Self.FConfigGenerator.Variables.AddConfig(FNewDataSet.edID.Text,
                                                FNewDataSet.GetFieldValue,
                                                FNewDataSet.edDescription.Text);

      Self.FConfigGenerator.SetVariables(TJupiterVariableFormList.CreateFromVariableList(Self.FConfigGenerator.Variables));
    end;
  finally
    FNewDataSet.Release;
    FreeAndNil(FNewDataSet);
  end;
end;

procedure TFConfig.Internal_SaveConfigClick(Sender: TObject);
var
  vrVez : Integer;
begin
  if Assigned(Self.FOldVariant) then
    Self.FOldVariant.CopyValues(Self.FConfigGenerator.Variables);

  for vrVez := 0 to tvNavigation.Items.Count - 1 do
  begin
    if not Assigned(tvNavigation.Items[vrVez].Data) then
      Continue;

    with TJupiterVariableList(tvNavigation.Items[vrVez].Data) do
    begin
      if Tag = -1 then // Variáveis gerais
      begin
        vrJupiterApp.Params.CopyValues(TJupiterVariableList(tvNavigation.Items[vrVez].Data));
        vrJupiterApp.Params.SaveToFile;
        Continue;
      end;

      if Tag = -2 then // Variáveis de usuário
      begin
        vrJupiterApp.UserParams.CopyValues(TJupiterVariableList(tvNavigation.Items[vrVez].Data));
        vrJupiterApp.UserParams.SaveToFile;
        Continue;
      end;

      if Tag = -3 then // Datasets
      begin
        vrJupiterApp.DataSetParams.CopyValues(TJupiterVariableList(tvNavigation.Items[vrVez].Data));
        vrJupiterApp.DataSetParams.SaveToFile;
        Continue;
      end;

      vrJupiterApp.ModulesList.GetModuleByIndex(Tag).Params.CopyValues(TJupiterVariableList(tvNavigation.Items[vrVez].Data));
      vrJupiterApp.ModulesList.GetModuleByIndex(Tag).Params.SaveToFile;
    end;
  end;

  Self.Close;
end;

end.

