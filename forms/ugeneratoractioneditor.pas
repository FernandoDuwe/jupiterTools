unit uGeneratorActionEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ExtCtrls, Buttons, StdCtrls, Spin, uJupiterForm, ugeneratoractioneditorfield,
  ugeneratoractioneditoraction, JupiterCustomAction, JupiterApp, uMain;

type

  { TFGeneratorActionEditor }

  TFGeneratorActionEditor = class(TJupiterForm)
    BitBtn1: TBitBtn;
    btnNewField: TBitBtn;
    btnNewField1: TBitBtn;
    edHint: TLabeledEdit;
    edTitle: TLabeledEdit;
    gbBody: TGroupBox;
    gbLeft: TGroupBox;
    gbActions: TGroupBox;
    lbIcon: TLabel;
    lvActions: TListView;
    lvFields: TListView;
    pcConfig: TPageControl;
    pcContent: TPageControl;
    pnTaskBar: TPanel;
    sbIcon: TSpeedButton;
    sbStatus: TStatusBar;
    seIcon: TSpinEdit;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    tsDadosAcao: TTabSheet;
    tsFields: TTabSheet;
    procedure BitBtn1Click(Sender: TObject);
    procedure btnNewField1Click(Sender: TObject);
    procedure btnNewFieldClick(Sender: TObject);
    procedure edHintChange(Sender: TObject);
    procedure edTitleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvActionsDblClick(Sender: TObject);
    procedure lvFieldsDblClick(Sender: TObject);
    procedure seIconChange(Sender: TObject);
  private
    FCustomAction : TJupiterCustomAction;

  protected
    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_UpdateCalcs; override;

  published
    property CustomAction : TJupiterCustomAction read FCustomAction write FCustomAction;

  public

  end;

var
  FGeneratorActionEditor: TFGeneratorActionEditor;

implementation

{$R *.lfm}

{ TFGeneratorActionEditor }

procedure TFGeneratorActionEditor.FormCreate(Sender: TObject);
begin
  Self.FCustomAction := TJupiterCustomAction.Create;
end;

procedure TFGeneratorActionEditor.BitBtn1Click(Sender: TObject);
begin
  Self.CustomAction.SaveToFile;

  Self.Close;
end;

procedure TFGeneratorActionEditor.btnNewField1Click(Sender: TObject);
var
  vrObj : TJupiterCustomActionAction;
begin
  Application.CreateForm(TFGeneratorActionEditorAction, FGeneratorActionEditorAction);
  try
    if FGeneratorActionEditorAction.ShowModal = mrOK then
    begin
      vrObj := TJupiterCustomActionAction.Create;

      vrObj.Title      := FGeneratorActionEditorAction.Action.Title;
      vrObj.Hint       := FGeneratorActionEditorAction.Action.Hint;
      vrObj.ScriptFile := FGeneratorActionEditorAction.Action.ScriptFile;
      vrObj.ImageIndex := FGeneratorActionEditorAction.Action.ImageIndex;

      Self.CustomAction.AddAction(vrObj);
    end;
  finally
    FGeneratorActionEditorAction.Release;
    FreeAndNil(FGeneratorActionEditorAction);

    Self.UpdateForm;
  end;
end;

procedure TFGeneratorActionEditor.btnNewFieldClick(Sender: TObject);
var
  vrObj : TJupiterCustomActionField;
begin
  Application.CreateForm(TFGeneratorActionEditorField, FGeneratorActionEditorField);
  try
    if FGeneratorActionEditorField.ShowModal = mrOK then
    begin
      vrObj := TJupiterCustomActionField.Create;

      vrObj.Title    := FGeneratorActionEditorField.Action.Title;
      vrObj.Hint     := FGeneratorActionEditorField.Action.Hint;
      vrObj.ConfigID := FGeneratorActionEditorField.Action.ConfigID;

      Self.CustomAction.AddField(vrObj);
    end;
  finally
    FGeneratorActionEditorField.Release;
    FreeAndNil(FGeneratorActionEditorField);

    Self.UpdateForm;
  end;
end;

procedure TFGeneratorActionEditor.edHintChange(Sender: TObject);
begin
  try
    Self.CustomAction.Hint := edHint.Text;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFGeneratorActionEditor.edTitleChange(Sender: TObject);
begin
  try
    Self.CustomAction.Title := edTitle.Text;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFGeneratorActionEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Self.FCustomAction);
end;

procedure TFGeneratorActionEditor.FormShow(Sender: TObject);
begin
  Self.CustomAction.LoadFromFile;

  edTitle.Text := Self.CustomAction.Title;
  edHint.Text  := Self.CustomAction.Hint;
  seIcon.Value := Self.CustomAction.ImageIndex;

  sbStatus.Panels[0].Text := Self.CustomAction.FileName;

  if vrJupiterApp.Config.GetByID('JupiterTools.UI.Display.WindowsState').Value = 'Maximized' then
    Self.WindowState := wsMaximized;
end;

procedure TFGeneratorActionEditor.lvActionsDblClick(Sender: TObject);
begin
  if not Assigned(lvActions.Selected) then
    Exit;

  if not Assigned(lvActions.Selected.Data) then
    Exit;

  Application.CreateForm(TFGeneratorActionEditorAction, FGeneratorActionEditorAction);
  try
    FGeneratorActionEditorAction.Action.Title      := TJupiterCustomActionAction(lvActions.Selected.Data).Title;
    FGeneratorActionEditorAction.Action.Hint       := TJupiterCustomActionAction(lvActions.Selected.Data).Hint;
    FGeneratorActionEditorAction.Action.ScriptFile := TJupiterCustomActionAction(lvActions.Selected.Data).ScriptFile;
    FGeneratorActionEditorAction.Action.ImageIndex := TJupiterCustomActionAction(lvActions.Selected.Data).ImageIndex;

    if FGeneratorActionEditorAction.ShowModal = mrOK then
    begin
      Self.FCustomAction.ActionByIndex(lvActions.Selected.Index).Title      := FGeneratorActionEditorAction.Action.Title;
      Self.FCustomAction.ActionByIndex(lvActions.Selected.Index).Hint       := FGeneratorActionEditorAction.Action.Hint;
      Self.FCustomAction.ActionByIndex(lvActions.Selected.Index).ScriptFile := FGeneratorActionEditorAction.Action.ScriptFile;
      Self.FCustomAction.ActionByIndex(lvActions.Selected.Index).ImageIndex := FGeneratorActionEditorAction.Action.ImageIndex;
    end;
  finally
    FGeneratorActionEditorAction.Release;
    FreeAndNil(FGeneratorActionEditorAction);

    Self.UpdateForm;
  end;
end;

procedure TFGeneratorActionEditor.lvFieldsDblClick(Sender: TObject);
begin
  if not Assigned(lvFields.Selected) then
    Exit;

  if not Assigned(lvFields.Selected.Data) then
    Exit;

  Application.CreateForm(TFGeneratorActionEditorField, FGeneratorActionEditorField);
  try
    FGeneratorActionEditorField.Action.Title    := TJupiterCustomActionField(lvFields.Selected.Data).Title;
    FGeneratorActionEditorField.Action.Hint     := TJupiterCustomActionField(lvFields.Selected.Data).Hint;
    FGeneratorActionEditorField.Action.ConfigID := TJupiterCustomActionField(lvFields.Selected.Data).ConfigID;

    if FGeneratorActionEditorField.ShowModal = mrOK then
    begin
      Self.FCustomAction.FieldByIndex(lvFields.Selected.Index).Title    := FGeneratorActionEditorField.Action.Title;
      Self.FCustomAction.FieldByIndex(lvFields.Selected.Index).Hint     := FGeneratorActionEditorField.Action.Hint;
      Self.FCustomAction.FieldByIndex(lvFields.Selected.Index).ConfigID := FGeneratorActionEditorField.Action.ConfigID;
    end;
  finally
    FGeneratorActionEditorField.Release;
    FreeAndNil(FGeneratorActionEditorField);

    Self.UpdateForm;
  end;
end;

procedure TFGeneratorActionEditor.seIconChange(Sender: TObject);
begin
  try
    Self.CustomAction.ImageIndex := seIcon.Value;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFGeneratorActionEditor.Internal_UpdateComponents;
begin
  Self.Caption := Self.CustomAction.Title + ' - Editor de Ações';

  seIcon.MinValue := 0;
  seIcon.MaxValue := FMain.ilMainIcons.Count - 1;

  sbIcon.ImageIndex := seIcon.Value;

  inherited Internal_UpdateComponents;
end;

procedure TFGeneratorActionEditor.Internal_UpdateDatasets;
var
  vrVez  : Integer;
  vrItem : TListItem;
begin
  lvFields.Items.Clear;
  lvActions.Items.Clear;

  for vrVez := 0 to Self.FCustomAction.FieldCount - 1 do
  begin
    vrItem := lvFields.Items.Add;

    vrItem.Caption := Self.FCustomAction.FieldByIndex(vrVez).Title;
    vrItem.SubItems.Add(Self.FCustomAction.FieldByIndex(vrVez).Hint);
    vrItem.SubItems.Add(Self.FCustomAction.FieldByIndex(vrVez).ConfigID);
    vrItem.Data := Self.FCustomAction.FieldByIndex(vrVez);
  end;

  for vrVez := 0 to Self.FCustomAction.ActionCount - 1 do
  begin
    vrItem := lvActions.Items.Add;

    vrItem.Caption := Self.FCustomAction.ActionByIndex(vrVez).Title;
    vrItem.SubItems.Add(Self.FCustomAction.ActionByIndex(vrVez).Hint);
    vrItem.SubItems.Add(Self.FCustomAction.ActionByIndex(vrVez).ScriptFile);
    vrItem.Data := Self.FCustomAction.ActionByIndex(vrVez);
  end;

  inherited Internal_UpdateDatasets;
end;

procedure TFGeneratorActionEditor.Internal_UpdateCalcs;
begin
  inherited Internal_UpdateCalcs;
end;

end.

