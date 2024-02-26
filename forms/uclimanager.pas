unit uclimanager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  JupiterForm, JupiterAction, JupiterRunnable, JupiterConsts, JupiterObject,
  JupiterFileDataProvider, JupiterEnviroment, jupiterclicommand,
  jupiterformutils, unewcommandcli;

type

  { TFCLIManager }

  TFCLIManager = class(TFJupiterForm)
    lvCommands: TListView;
    procedure FormCreate(Sender: TObject);
    procedure lvCommandsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FIndex : Integer;

    procedure Internal_NewItemClick(Sender: TObject);
    procedure Internal_UpdateItemClick(Sender: TObject);
    procedure Internal_DeleteItemClick(Sender: TObject);

    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_Resize; override;
  public

  end;

var
  FCLIManager: TFCLIManager;

implementation

uses LCLType;

{$R *.lfm}

{ TFCLIManager }

procedure TFCLIManager.FormCreate(Sender: TObject);
begin
  inherited;

  FIndex := NULL_KEY;
end;

procedure TFCLIManager.lvCommandsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  try
    Self.FIndex := NULL_KEY;

    if Assigned(Item.Data) then
      Self.FIndex := TJupiterObject(Item.Data).Tag;
  finally
    Self.UpdateForm(False);
  end;
end;

procedure TFCLIManager.Internal_NewItemClick(Sender: TObject);
begin
  Application.CreateForm(TFNewCommandCli, FNewCommandCli);
  try
    FNewCommandCli.IsModal := True;
    FNewCommandCli.ShowModal;
  finally
    FNewCommandCli.Release;
    FreeAndNil(FNewCommandCli);

    Self.UpdateForm();
  end;
end;

procedure TFCLIManager.Internal_UpdateItemClick(Sender: TObject);
begin
  try
    Application.CreateForm(TFNewCommandCli, FNewCommandCli);
    try
      FNewCommandCli.IsModal := True;

      FNewCommandCli.CommandCLI := TJupiterCLICommand(lvCommands.Selected.Data);
      FNewCommandCli.ShowModal;
    finally
      FNewCommandCli.Release;
      FreeAndNil(FNewCommandCli);

      Self.UpdateForm();
    end;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFCLIManager.Internal_DeleteItemClick(Sender: TObject);
begin
  if not Assigned(lvCommands.Selected) then
    Exit;

  if not Assigned(lvCommands.Selected.Data) then
    Exit;

  DeleteFile(TJupiterCLICommand(lvCommands.Selected.Data).FileName);

  Self.UpdateForm();
end;

procedure TFCLIManager.Internal_PrepareForm;
var
  vrAction : TJupiterAction;
begin
  inherited Internal_PrepareForm;

  Self.Hint := 'Adicione, edite ou remova comandos que podem ser executados pelo CLI Europa.';

  vrAction      := TJupiterAction.Create('Novo', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para adicionar um novo campo';
  vrAction.Icon := ICON_NEW;
  vrAction.OnClick := @Internal_NewItemClick;

  Self.Actions.Add(vrAction);

  vrAction      := TJupiterAction.Create('Editar', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para editar o campo';
  vrAction.Icon := ICON_EDIT;
  vrAction.OnClick := @Internal_UpdateItemClick;

  Self.Actions.Add(vrAction);

  vrAction      := TJupiterAction.Create('Excluir', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para excluir o campo';
  vrAction.Icon := ICON_DELETE;
  vrAction.ConfirmBeforeExecute := True;
  vrAction.OnClick := @Internal_DeleteItemClick;

  Self.Actions.Add(vrAction);
end;

procedure TFCLIManager.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if Self.Actions.Count > 1 then
  begin
    Self.Actions.GetActionButton(1, sbActions).Enabled := (FIndex <> NULL_KEY);
    Self.Actions.GetMenuItem(1).Enabled := (FIndex <> NULL_KEY);
  end;

  if Self.Actions.Count > 2 then
  begin
    Self.Actions.GetActionButton(2, sbActions).Enabled := (FIndex <> NULL_KEY);
    Self.Actions.GetMenuItem(2).Enabled := (FIndex <> NULL_KEY);
  end;
end;

procedure TFCLIManager.Internal_UpdateDatasets;
var
  vrFileDataProvider : TJupiterFileDataProvider;
  vrEnviroment : TJupiterEnviroment;
  vrVez : Integer;
  vrItem : TListItem;
  vrCommand : TJupiterCLICommand;
begin
  inherited Internal_UpdateDatasets;

  lvCommands.Items.Clear;

  vrFileDataProvider := TJupiterFileDataProvider.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrFileDataProvider.Path := vrEnviroment.FullPath('/modules/CLI/');
    vrFileDataProvider.ProvideData;

    for vrVez := 0 to vrFileDataProvider.Count - 1 do
    begin
      if vrFileDataProvider.GetRowByIndex(vrVez).Fields.VariableById('Extension').Value <> '.jcc' then
        Continue;

      vrCommand := TJupiterCLICommand.Create;
      vrCommand.FileName := vrFileDataProvider.GetRowByIndex(vrVez).Fields.VariableById('File').Value;
      vrCommand.LoadFromFile;

      vrItem := lvCommands.Items.Add;
      vrItem.Caption := vrCommand.CommandName + '   ';
      vrItem.SubItems.Add(vrCommand.Command + '   ');

      if vrCommand.ParamList.Count > 0 then
        vrItem.SubItems.Add('Sim')
      else
        vrItem.SubItems.Add('Não');

      vrItem.Data := vrCommand;
    end;
  finally
    FreeAndNil(vrFileDataProvider);
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFCLIManager.Internal_Resize;
begin
  inherited Internal_Resize;

  lvCommands.Columns[0].Width := PercentOfScreen(lvCommands.Width, 30);
  lvCommands.Columns[1].Width := PercentOfScreen(lvCommands.Width, 30);
  lvCommands.Columns[2].Width := PercentOfScreen(lvCommands.Width, 29);
end;

end.

