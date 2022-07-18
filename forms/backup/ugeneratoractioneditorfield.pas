unit ugeneratoractioneditorfield;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, uJupiterForm,
  JupiterCustomAction, JupiterApp, StdCtrls;

type

  { TFGeneratorActionEditorField }

  TFGeneratorActionEditorField = class(TJupiterForm)
    btCancel: TButton;
    btSave: TButton;
    cbConfig: TComboBox;
    edTitle: TLabeledEdit;
    edHint: TLabeledEdit;
    Label1: TLabel;
    pnBottom: TPanel;
    procedure cbConfigChange(Sender: TObject);
    procedure edHintChange(Sender: TObject);
    procedure edTitleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FAction : TJupiterCustomActionField;

    procedure Internal_UpdateComponents; override;
  published
    property Action : TJupiterCustomActionField read FAction write FAction;
  public

  end;

var
  FGeneratorActionEditorField: TFGeneratorActionEditorField;

implementation

{$R *.lfm}

{ TFGeneratorActionEditorField }

procedure TFGeneratorActionEditorField.FormCreate(Sender: TObject);
var
  vrVez : Integer;
begin
  Self.FAction := TJupiterCustomActionField.Create;

  cbConfig.Items.Clear;

  for vrVez := 0 to vrJupiterApp.Config.Count - 1 do
    cbConfig.Items.Add(vrJupiterApp.Config.GetByIndex(vrVez).ID);
end;

procedure TFGeneratorActionEditorField.edTitleChange(Sender: TObject);
begin
  try
    Self.FAction.Title := edTitle.Text;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFGeneratorActionEditorField.edHintChange(Sender: TObject);
begin
  try
    Self.FAction.Hint := edHint.Text;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFGeneratorActionEditorField.cbConfigChange(Sender: TObject);
begin
  try
    Self.FAction.ConfigID := cbConfig.Text;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFGeneratorActionEditorField.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Self.FAction);
end;

procedure TFGeneratorActionEditorField.Internal_UpdateComponents;
begin
  edTitle.Text  := Self.FAction.Title;
  edHint.Text   := Self.FAction.Hint;
  cbConfig.Text := Self.FAction.ConfigID;

  inherited Internal_UpdateComponents;
end;

end.

