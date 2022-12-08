unit uGenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Buttons, uCustomJupiterForm, uNewAction, uNewField,
  JupiterFileDataProvider, JupiterEnviroment, JupiterConsts,
  JupiterXMLDataProvider, JupiterGeneratorForm, JupiterAction, JupiterRunnable,
  JupiterVariableForm, LCLType;

type

  { TFGenerator }

  TFGenerator = class(TFCustomJupiterForm)
    edFile: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbFormList: TListBox;
    lvActions: TListView;
    lvFields: TListView;
    mmLines: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pnForm: TPanel;
    pcTabs: TPageControl;
    sbActionDelete: TSpeedButton;
    sbFieldDelete: TSpeedButton;
    sbActionAdd: TSpeedButton;
    sbFieldAdd: TSpeedButton;
    spForms: TSplitter;
    Splitter1: TSplitter;
    tsForms: TTabSheet;
    tsInicio: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbFormListClick(Sender: TObject);
    procedure lvActionsDblClick(Sender: TObject);
    procedure lvActionsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvFieldsDblClick(Sender: TObject);
    procedure lvFieldsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure sbActionAddClick(Sender: TObject);
    procedure sbActionDeleteClick(Sender: TObject);
    procedure sbFieldAddClick(Sender: TObject);
    procedure sbFieldDeleteClick(Sender: TObject);
  private
    FFormID : String;

    procedure Internal_ShowForm(prFile : String);
    procedure Internal_EditAction(prActionIndex : Integer; prAction : TJupiterAction);
    procedure Internal_DeleteAction(prActionIndex : Integer);
    procedure Internal_EditField(prFieldIndex : Integer; prField : TJupiterVariableForm);
    procedure Internal_DeleteField(prFieldIndex : Integer);
    procedure Internal_PrepareForm; override;
    procedure Internal_RefreshClick(Sender: TObject);

    procedure Internal_UpdateFormForm;
  protected
    procedure Internal_UpdateDatasets; override;
  published
    property FormID : String read FFormID write FFormID;
  public

  end;

var
  FGenerator: TFGenerator;

implementation

uses uMain;

{$R *.lfm}

{ TFGenerator }

procedure TFGenerator.lbFormListClick(Sender: TObject);
begin
  if lbFormList.ItemIndex = NULL_KEY then
    Exit;

  Self.Internal_ShowForm(lbFormList.Items[lbFormList.ItemIndex]);
end;

procedure TFGenerator.lvActionsDblClick(Sender: TObject);
begin
  if not Assigned(lvActions.Selected) then
    Exit;

  if not Assigned(lvActions.Selected.Data) then
    Exit;

  Application.CreateForm(TFNewAction, FNewAction);
  try
    FNewAction.ActionIndex := TJupiterAction(lvActions.Selected.Data).Tag;
    FNewAction.Action := TJupiterAction(lvActions.Selected.Data);

    if FNewAction.ShowModal = mrOK then
      Self.Internal_EditAction(FNewAction.ActionIndex, FNewAction.Action);
  finally
    FreeAndNil(FNewAction);
  end;
end;

procedure TFGenerator.lvActionsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  Self.Internal_UpdateFormForm;
end;

procedure TFGenerator.lvFieldsDblClick(Sender: TObject);
begin
  if not Assigned(lvFields.Selected) then
    Exit;

  if not Assigned(lvFields.Selected.Data) then
    Exit;

  Application.CreateForm(TFNewField, FNewField);
  try
    FNewField.FieldIndex := TJupiterVariableForm(lvFields.Selected.Data).Tag;
    FNewField.Field      := TJupiterVariableForm(lvFields.Selected.Data);

    if FNewField.ShowModal = mrOK then
      Self.Internal_EditField(FNewField.FieldIndex, FNewField.Field);
  finally
    FreeAndNil(FNewField);
  end;
end;

procedure TFGenerator.lvFieldsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  Self.Internal_UpdateFormForm;
end;

procedure TFGenerator.sbActionAddClick(Sender: TObject);
begin
  Application.CreateForm(TFNewAction, FNewAction);
  try
    FNewAction.ActionIndex := NULL_KEY;

    if FNewAction.ShowModal = mrOK then
      Self.Internal_EditAction(FNewAction.ActionIndex, FNewAction.Action);
  finally
    FreeAndNil(FNewAction);
  end;
end;

procedure TFGenerator.sbActionDeleteClick(Sender: TObject);
begin
  if not Assigned(lvActions.Selected) then
    Exit;

  if not Assigned(lvActions.Selected.Data) then
    Exit;

  if Application.MessageBox('Deseja realmente excluir', 'Excluir Ação', MB_ICONQUESTION + MB_YESNO) = ID_YES then
    Self.Internal_DeleteAction(TJupiterAction(lvActions.Selected.Data).Tag);
end;

procedure TFGenerator.sbFieldAddClick(Sender: TObject);
begin
  Application.CreateForm(TFNewField, FNewField);
  try
    FNewField.FieldIndex := NULL_KEY;

    if FNewField.ShowModal = mrOK then
      Self.Internal_EditField(FNewField.FieldIndex, FNewField.Field);
  finally
    FreeAndNil(FNewField);
  end;
end;

procedure TFGenerator.sbFieldDeleteClick(Sender: TObject);
begin
  if not Assigned(lvFields.Selected) then
    Exit;

  if not Assigned(lvFields.Selected.Data) then
    Exit;

  if Application.MessageBox('Deseja realmente excluir', 'Excluir Campo', MB_ICONQUESTION + MB_YESNO) = ID_YES then
    Self.Internal_DeleteField(TJupiterVariableForm(lvFields.Selected.Data).Tag);
end;

procedure TFGenerator.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FFormID := EmptyStr;
end;

procedure TFGenerator.FormShow(Sender: TObject);
begin
  inherited;

  if Self.FormID <> EmptyStr then
  begin
    pcTabs.ActivePageIndex := 1;

    lbFormList.ItemIndex := lbFormList.Items.IndexOf(Self.FormID + '.xml');

    if lbFormList.ItemIndex <> NULL_KEY then
      lbFormListClick(Sender);
  end;
end;

procedure TFGenerator.Internal_ShowForm(prFile: String);
var
  vrEnviroment : TJupiterEnviroment;
  vrGenerator  : TJupiterGeneratorForm;
  vrVez        : Integer;
  vrRow        : TListItem;
begin
  lvActions.Items.Clear;
  lvFields.Items.Clear;

  vrEnviroment := TJupiterEnviroment.Create;
  vrGenerator  := TJupiterGeneratorForm.Create;
  try
    edFile.Text := vrEnviroment.FullPath('modules/generator/' + prFile);

    vrGenerator.FormID := StringReplace(prFile, '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]);

    for vrVez := 0 to vrGenerator.Actions.Size - 1 do
    begin
      vrRow := lvActions.Items.Add;
      vrRow.Caption := TJupiterAction(vrGenerator.Actions.GetAtIndex(vrVez)).Title;
      vrRow.SubItems.Add(TJupiterAction(vrGenerator.Actions.GetAtIndex(vrVez)).Runnable.CommandLine);
      vrRow.Data := TJupiterAction(vrGenerator.Actions.GetAtIndex(vrVez));
    end;

    for vrVez := 0 to vrGenerator.Fields.Size - 1 do
    begin
      vrRow := lvFields.Items.Add;
      vrRow.Caption := vrGenerator.Fields.VariableByIndex(vrVez).ID;
      vrRow.SubItems.Add(vrGenerator.Fields.VariableByIndex(vrVez).Value);
      vrRow.SubItems.Add(vrGenerator.Fields.VariableByIndex(vrVez).Title);
      vrRow.Data := vrGenerator.Fields.VariableByIndex(vrVez);
    end;

    Self.Internal_UpdateFormForm;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFGenerator.Internal_EditAction(prActionIndex: Integer; prAction: TJupiterAction);
var
  vrGenerator : TJupiterGeneratorForm;
begin
  vrGenerator  := TJupiterGeneratorForm.Create;
  try
    vrGenerator.FormID := StringReplace(ExtractFileName(edFile.Text), '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]);

    if prActionIndex = NULL_KEY then
    begin
      vrGenerator.Actions.Add(TJupiterAction.Create(prAction.Title, TJupiterRunnable.Create(prAction.Runnable.CommandLine)));

      with TJupiterAction(vrGenerator.Actions.GetLastObject) do
      begin
        Hint := prAction.Hint;
        Icon := prAction.Icon;

        ConfirmBeforeExecute := prAction.ConfirmBeforeExecute;
      end;
    end
    else
    begin
      with TJupiterAction(vrGenerator.Actions.GetAtIndex(prActionIndex)) do
      begin
        Title := prAction.Title;
        Hint  := prAction.Hint;
        Icon  := prAction.Icon;
        ConfirmBeforeExecute := prAction.ConfirmBeforeExecute;

        Runnable.CommandLine := prAction.Runnable.CommandLine;
      end;
    end;

    vrGenerator.SaveFile;

    Self.Internal_ShowForm(StringReplace(ExtractFileName(edFile.Text), '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]));
  finally
    FreeAndNil(vrGenerator);
  end;
end;

procedure TFGenerator.Internal_DeleteAction(prActionIndex: Integer);
var
  vrGenerator : TJupiterGeneratorForm;
begin
  vrGenerator  := TJupiterGeneratorForm.Create;
  try
    vrGenerator.FormID := StringReplace(ExtractFileName(edFile.Text), '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
    vrGenerator.Actions.DeleteAtIndex(prActionIndex);
    vrGenerator.SaveFile;

    Self.Internal_ShowForm(StringReplace(ExtractFileName(edFile.Text), '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]));
  finally
    FreeAndNil(vrGenerator);
  end;
end;

procedure TFGenerator.Internal_EditField(prFieldIndex: Integer; prField: TJupiterVariableForm);
var
  vrGenerator : TJupiterGeneratorForm;
begin
  vrGenerator  := TJupiterGeneratorForm.Create;
  try
    vrGenerator.FormID := StringReplace(ExtractFileName(edFile.Text), '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]);

    if prFieldIndex = NULL_KEY then
    begin
      vrGenerator.Fields.Add(TJupiterVariableForm.Create);

      with TJupiterVariableForm(vrGenerator.Fields.GetLastObject) do
      begin
        ID    := prField.ID;
        Title := prField.Title;
        Value := prField.Value;

        CleanOnShow   := prField.CleanOnShow;
        Required      := prField.Required;
        ReadOnly      := prField.ReadOnly;
        CopyButton    := prField.CopyButton;
        RunButton     := prField.RunButton;
        ComponentType := prField.ComponentType;
        ListVariable  := prField.ListVariable;
      end;
    end
    else
    begin
      with TJupiterVariableForm(vrGenerator.Fields.GetAtIndex(prFieldIndex)) do
      begin
        ID    := prField.ID;
        Title := prField.Title;
        Value := prField.Value;

        CleanOnShow   := prField.CleanOnShow;
        Required      := prField.Required;
        ReadOnly      := prField.ReadOnly;
        CopyButton    := prField.CopyButton;
        RunButton     := prField.RunButton;
        ComponentType := prField.ComponentType;
        ListVariable  := prField.ListVariable;
      end;
    end;

    vrGenerator.SaveFile;

    Self.Internal_ShowForm(StringReplace(ExtractFileName(edFile.Text), '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]));
  finally
    FreeAndNil(vrGenerator);
  end;
end;

procedure TFGenerator.Internal_DeleteField(prFieldIndex: Integer);
var
  vrGenerator : TJupiterGeneratorForm;
begin
  vrGenerator  := TJupiterGeneratorForm.Create;
  try
    vrGenerator.FormID := StringReplace(ExtractFileName(edFile.Text), '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
    vrGenerator.Fields.DeleteAtIndex(prFieldIndex);
    vrGenerator.SaveFile;

    Self.Internal_ShowForm(StringReplace(ExtractFileName(edFile.Text), '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]));
  finally
    FreeAndNil(vrGenerator);
  end;
end;

procedure TFGenerator.Internal_PrepareForm;
var
  vrAction : TJupiterAction;
begin
  inherited Internal_PrepareForm;

  vrAction      := TJupiterAction.Create('Atualizar', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para atualizar a página';
  vrAction.Icon := ICON_REFRESH;
  vrAction.OnClick := @Internal_RefreshClick;

  Self.Actions.Add(vrAction);

  lvActions.Enabled := False;
  lvFields.Enabled  := False;

  sbActionAdd.Enabled    := False;
  sbActionAdd.Images     := FMain.ilIconFamily;
  sbActionAdd.ImageIndex := ICON_NEW;

  sbActionDelete.Enabled    := False;
  sbActionDelete.Images     := FMain.ilIconFamily;
  sbActionDelete.ImageIndex := ICON_DELETE;

  sbFieldAdd.Enabled    := False;
  sbFieldAdd.Images     := FMain.ilIconFamily;
  sbFieldAdd.ImageIndex := ICON_NEW;

  sbFieldDelete.Enabled    := False;
  sbFieldDelete.Images     := FMain.ilIconFamily;
  sbFieldDelete.ImageIndex := ICON_DELETE;

  lvActions.LargeImages := FMain.ilIconFamily;
  lvActions.SmallImages := FMain.ilIconFamily;
  lvActions.StateImages := FMain.ilIconFamily;

  Self.Hint := 'No módulo Generator você pode criar suas próprias funcionalidades no sistema';
end;

procedure TFGenerator.Internal_RefreshClick(Sender: TObject);
begin
  Self.UpdateForm;
end;

procedure TFGenerator.Internal_UpdateFormForm;
begin
  sbActionAdd.Enabled := Trim(edFile.Text) <> EmptyStr;
  sbFieldAdd.Enabled  := Trim(edFile.Text) <> EmptyStr;

  lvActions.Enabled := Trim(edFile.Text) <> EmptyStr;
  lvFields.Enabled := Trim(edFile.Text) <> EmptyStr;

  sbActionDelete.Enabled := Assigned(lvActions.Selected);
  sbFieldDelete.Enabled  := Assigned(lvFields.Selected);
end;

procedure TFGenerator.Internal_UpdateDatasets;
var
  vrFile       : TJupiterFileDataProvider;
  vrVez        : Integer;
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Internal_UpdateDatasets;

  lbFormList.Items.Clear;

  vrFile       := TJupiterFileDataProvider.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrFile.Path := vrEnviroment.FullPath('modules/generator/');
    vrFile.ProvideData;

    for vrVez := 0 to vrFile.Size - 1 do
      with vrFile.GetRowByIndex(vrVez) do
        lbFormList.Items.Add(Fields.VariableById('FieldName').Value);
  finally
    FreeAndNil(vrFile);
    FreeAndNil(vrEnviroment);
  end;
end;

end.

