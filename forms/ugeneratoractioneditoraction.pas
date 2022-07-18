unit ugeneratoractioneditoraction;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, uJupiterForm,
  JupiterCustomAction, uMain, StdCtrls, Buttons, Spin;

type

  { TFGeneratorActionEditorAction }

  TFGeneratorActionEditorAction = class(TJupiterForm)
    btCancel: TButton;
    btSave: TButton;
    edHint: TLabeledEdit;
    edScritFile: TLabeledEdit;
    edTitle: TLabeledEdit;
    lbIcon: TLabel;
    pnBottom: TPanel;
    sbIcon: TSpeedButton;
    seIcon: TSpinEdit;
    procedure btSaveClick(Sender: TObject);
    procedure edHintChange(Sender: TObject);
    procedure edScritFileChange(Sender: TObject);
    procedure edTitleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure seIconChange(Sender: TObject);
  private
    FAction : TJupiterCustomActionAction;

    procedure Internal_UpdateComponents; override;
  published
    property Action : TJupiterCustomActionAction read FAction write FAction;
  public

  end;

var
  FGeneratorActionEditorAction: TFGeneratorActionEditorAction;

implementation

{$R *.lfm}

{ TFGeneratorActionEditorAction }

procedure TFGeneratorActionEditorAction.FormCreate(Sender: TObject);
begin
  Self.FAction := TJupiterCustomActionAction.Create;
end;

procedure TFGeneratorActionEditorAction.edTitleChange(Sender: TObject);
begin
  try
    Self.FAction.Title := edTitle.Text;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFGeneratorActionEditorAction.edHintChange(Sender: TObject);
begin
  try
    Self.FAction.Hint := edHint.Text;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFGeneratorActionEditorAction.btSaveClick(Sender: TObject);
begin

end;

procedure TFGeneratorActionEditorAction.edScritFileChange(Sender: TObject);
begin
  try
    Self.FAction.ScriptFile := edScritFile.Text;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFGeneratorActionEditorAction.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Self.FAction);
end;

procedure TFGeneratorActionEditorAction.seIconChange(Sender: TObject);
begin
  try
    Self.FAction.ImageIndex := seIcon.Value;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFGeneratorActionEditorAction.Internal_UpdateComponents;
begin
  edTitle.Text      := Self.FAction.Title;
  edHint.Text       := Self.FAction.Hint;
  edScritFile.Text  := Self.FAction.ScriptFile;
  seIcon.Value      := Self.FAction.ImageIndex;
  sbIcon.ImageIndex := Self.FAction.ImageIndex;

  inherited Internal_UpdateComponents;
end;

end.

