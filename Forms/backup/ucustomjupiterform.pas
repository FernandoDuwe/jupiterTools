unit uCustomJupiterForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, JupiterForm,
  JupiterAction, JupiterConsts, JupiterRunnable, JupiterFormGenerator;

type

  { TFCustomJupiterForm }

  TFCustomJupiterForm = class(TFJupiterForm)
    sbBody: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FFormGenerator : TJupiterFormGenerator;
  published
    property FormGenerator : TJupiterFormGenerator read FFormGenerator write FFormGenerator;
  protected
    procedure Internal_PrepareForm; override;
  public
    procedure PrepareForm; override;
  end;

var
  FCustomJupiterForm: TFCustomJupiterForm;

implementation

{$R *.lfm}

{ TFCustomJupiterForm }

procedure TFCustomJupiterForm.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FFormGenerator := TJupiterFormGenerator.Create;
  Self.FFormGenerator.Container := sbBody;
end;

procedure TFCustomJupiterForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFormGenerator);

  inherited;
end;

procedure TFCustomJupiterForm.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;
end;

procedure TFCustomJupiterForm.PrepareForm;
begin
  inherited PrepareForm;

  Self.FFormGenerator.Variables.CreateFromVariableList(Self.Generator.Fields);

  if ((Assigned(Self.FFormGenerator.Variables)) and (Self.FFormGenerator.Variables.Size > 0)) then
    Self.FFormGenerator.DrawForm;
end;

end.

