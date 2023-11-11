unit ulayoutbuilder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, JupiterForm,
  JupiterConsts, JupiterAction, JupiterRunnable, JupiterCSVDataProvider,
  JupiterDialogForm, JupiterObject, JupiterApp, JupiterDataProvider;

type

  { TFLayoutBuilder }

  TFLayoutBuilder = class(TFJupiterForm)
    lvFields: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvFieldsResize(Sender: TObject);
    procedure lvFieldsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FIndex : Integer;
    FCSVDataProvider : TJupiterCSVDataProvider;

    procedure Internal_PrepareForm; override;

    procedure Internal_NewItemClick(Sender: TObject);
    procedure Internal_UpdateItemClick(Sender: TObject);
    procedure Internal_DeleteItemClick(Sender: TObject);

    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
  public

  end;

var
  FLayoutBuilder: TFLayoutBuilder;

implementation

{$R *.lfm}

{ TFLayoutBuilder }

procedure TFLayoutBuilder.FormCreate(Sender: TObject);
begin
  inherited;

  FIndex := NULL_KEY;

  Self.FCSVDataProvider := TJupiterCSVDataProvider.Create;
end;

procedure TFLayoutBuilder.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCSVDataProvider);

  inherited;
end;

procedure TFLayoutBuilder.lvFieldsResize(Sender: TObject);
begin

end;

procedure TFLayoutBuilder.lvFieldsSelectItem(Sender: TObject; Item: TListItem;
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

procedure TFLayoutBuilder.Internal_PrepareForm;
var
  vrAction : TJupiterAction;
begin
  inherited Internal_PrepareForm;

  Self.Hint := 'Cadastre um layout de arquivo';

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

  Self.FCSVDataProvider.Filename := Params.VariableById('filename').Value;
  Self.FCSVDataProvider.ProvideData;
end;

procedure TFLayoutBuilder.Internal_NewItemClick(Sender: TObject);
var
  vrDialog : TJupiterDialogForm;
begin
  vrDialog := TJupiterDialogForm.Create;
  try
    vrDialog.Title := 'Novo campo';
    vrDialog.Hint  := 'Crie um novo campo no layout.';

    vrDialog.Fields.AddField('Field', 'Nome do campo', '');
    vrDialog.Fields.AddField('Type', 'Tipo do Campo (A, N, D)', '');
    vrDialog.Fields.AddField('Start', 'Posição inicial', '');
    vrDialog.Fields.AddField('Size', 'Tamanho do campo', '');

    if vrDialog.Show then
    begin
      Self.FCSVDataProvider.SaveLine(vrDialog.Fields);

      Self.UpdateForm;
    end;
  finally
    FreeAndNil(vrDialog);
  end;
end;

procedure TFLayoutBuilder.Internal_UpdateItemClick(Sender: TObject);
var
  vrDialog : TJupiterDialogForm;
  vrRow : TJupiterDataProviderRow;
begin
  vrRow := Self.FCSVDataProvider.GetRowByIndex(Self.FIndex);

  vrDialog := TJupiterDialogForm.Create;
  try
    vrDialog.Title := 'Novo campo';
    vrDialog.Hint  := 'Crie um novo campo no layout.';

    vrDialog.Fields.AddField('Field', 'Nome do campo', vrRow.Fields.VariableById('Field').Value);
    vrDialog.Fields.AddField('Type', 'Tipo do Campo (A, N, D)', vrRow.Fields.VariableById('Type').Value);
    vrDialog.Fields.AddField('Start', 'Posição inicial', vrRow.Fields.VariableById('Start').Value);
    vrDialog.Fields.AddField('Size', 'Tamanho do campo', vrRow.Fields.VariableById('Size').Value);

    if vrDialog.Show then
    begin
      vrRow.Fields.CopyValues(vrDialog.Fields);

      Self.FCSVDataProvider.SaveFile;

      Self.UpdateForm;
    end;
  finally
    FreeAndNil(vrDialog);
  end;
end;

procedure TFLayoutBuilder.Internal_DeleteItemClick(Sender: TObject);
begin
  try
    Self.FCSVDataProvider.RemoveLine(Self.FIndex);

    Self.FIndex := NULL_KEY;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFLayoutBuilder.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if Self.Actions.Count > 1 then
    Self.Actions.GetActionButton(1, sbActions).Enabled := ((FCSVDataProvider.Count > 0) and (FIndex <> NULL_KEY));

  if Self.Actions.Count > 2 then
    Self.Actions.GetActionButton(2, sbActions).Enabled := ((FCSVDataProvider.Count > 0) and (FIndex <> NULL_KEY));
end;

procedure TFLayoutBuilder.Internal_UpdateDatasets;
var
  vrVez : Integer;
  vrItem : TListItem;
  vrItemObj : TJupiterObject;
begin
  inherited Internal_UpdateDatasets;

  lvFields.Items.Clear;

  for vrVez := 0 to FCSVDataProvider.Count - 1 do
    with FCSVDataProvider.GetRowByIndex(vrVez) do
    begin
      vrItemObj := TJupiterObject.Create;
      vrItemObj.Tag := vrVez;

      vrItem := lvFields.Items.Add;

      vrItem.Caption := Fields.VariableById('Field').Value;
      vrItem.SubItems.Add(Fields.VariableById('Type').Value);
      vrItem.SubItems.Add(Fields.VariableById('Start').Value);
      vrItem.SubItems.Add(Fields.VariableById('Size').Value);
      vrItem.Data := vrItemObj;
    end;
end;

end.

