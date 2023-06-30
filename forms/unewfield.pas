unit uNewField;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  uCustomJupiterForm, JupiterVariableForm, JupiterConsts, JupiterAction,
  JupiterApp, JupiterVariable, LCLType;

type

  { TFNewField }

  TFNewField = class(TFCustomJupiterForm)
    cbCleanOnShow: TCheckBox;
    cbRunButton: TCheckBox;
    cbRequired: TCheckBox;
    cbReadOnly: TCheckBox;
    cbCopyButton: TCheckBox;
    cbListField: TComboBox;
    edValue: TEdit;
    edID: TEdit;
    edDescription: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    pcFieldType: TPageControl;
    TabSheet1: TTabSheet;
    tsFieldText: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    FField      : TJupiterVariableForm;
    FFieldIndex : Integer;

    procedure Internal_SaveFieldClick(Sender : TObject);
    procedure Internal_ListVariables;
  published
    property Field      : TJupiterVariableForm read FField      write FField;
    property FieldIndex : Integer              read FFieldIndex write FFieldIndex;
  protected
    procedure Internal_PrepareForm; override;
  public

  end;

var
  FNewField: TFNewField;

implementation


{$R *.lfm}

{ TFNewField }

procedure TFNewField.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FieldIndex := NULL_KEY;

  Self.IsModal := True;
end;

procedure TFNewField.Internal_SaveFieldClick(Sender: TObject);
begin
 try
   if Trim(edID.Text) = EmptyStr then
     raise Exception.Create('O campo ID é obrigatório');

   if Trim(edDescription.Text) = EmptyStr then
     raise Exception.Create('O campo Descrição é obrigatório');

   Self.Field.ID    := edID.Text;
   Self.Field.Value := edValue.Text;
   Self.Field.Title := edDescription.Text;

   Self.Field.Required    := cbRequired.Checked;
   Self.Field.ReadOnly    := cbReadOnly.Checked;
   Self.Field.CleanOnShow := cbCleanOnShow.Checked;
   Self.Field.CopyButton  := cbCopyButton.Checked;
   Self.Field.RunButton   := cbRunButton.Checked;

   case pcFieldType.ActivePageIndex of
     0 : Self.Field.ComponentType := FIELD_TYPE_EDIT;
     1 : Self.Field.ComponentType := FIELD_TYPE_COMBO;
   end;

   Self.Field.ListVariable := cbListField.Text;

   Self.ModalResult := mrOK;
 except
   Application.MessageBox(PAnsiChar(Exception(ExceptObject).Message), PAnsiChar(Self.Caption), MB_ICONERROR + MB_OK);
 end;
end;

procedure TFNewField.Internal_ListVariables;
var
  vrVez  : Integer;
  vrVez2 : Integer;
begin
  cbListField.Items.Clear;

  for vrVez := 0 to vrJupiterApp.Params.Size - 1 do
    cbListField.Items.Add(vrJupiterApp.Params.VariableByIndex(vrVez).ID);

  for vrVez := 0 to vrJupiterApp.Params.ChildList.Size - 1 do
    for vrVez2 := 0 to TJupiterVariableList(vrJupiterApp.Params.ChildList.GetAtIndex(vrVez)).Size - 1 do
      cbListField.Items.Add(TJupiterVariableList(vrJupiterApp.Params.ChildList.GetAtIndex(vrVez)).VariableByIndex(vrVez2).ID);
end;

procedure TFNewField.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.Internal_ListVariables;

  if not Assigned(Self.Field) then
  begin
    Self.Field      := TJupiterVariableForm.Create;
    Self.FieldIndex := NULL_KEY;
    Self.Caption    := 'Novo campo';
  end
  else
    Self.Caption := 'Editar campo';

  Self.Hint := 'Utilize os campos para armazenar informações úteis.';

  Self.Actions.Add(TJupiterAction.Create('Salvar', @Internal_SaveFieldClick));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para salvar o campo';
    Icon := ICON_SAVE;
  end;

  edID.Text             := Self.Field.ID;
  edValue.Text          := Self.Field.Value;
  edDescription.Text    := Self.Field.Title;
  cbRequired.Checked    := Self.Field.Required;
  cbReadOnly.Checked    := Self.Field.ReadOnly;
  cbCleanOnShow.Checked := Self.Field.CleanOnShow;
  cbCopyButton.Checked  := Self.Field.CopyButton;
  cbRunButton.Checked   := Self.Field.RunButton;

  if Self.Field.ComponentType = FIELD_TYPE_COMBO then
    pcFieldType.ActivePageIndex := 1
  else
    pcFieldType.ActivePageIndex := 0;

  cbListField.Caption := EmptyStr;

  if Self.Field.ListVariable <> EmptyStr then
    cbListField.ItemIndex := cbListField.Items.IndexOf(Self.Field.ListVariable);
end;

end.

