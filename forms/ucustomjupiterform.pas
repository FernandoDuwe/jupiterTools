unit uCustomJupiterForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, JupiterForm,
  JupiterAction, JupiterConsts, JupiterRunnable, JupiterFormGenerator,
  jupiterformutils;

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

    procedure Internal_SaveGeneratorClick(Sender : TObject); override;
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

procedure TFCustomJupiterForm.Internal_SaveGeneratorClick(Sender: TObject);
begin
  inherited Internal_SaveGeneratorClick(Sender);

  if Assigned(Self.FormGenerator.Variables) then
  begin
    Self.FormGenerator.Variables.Validate;
    Self.FormGenerator.Variables.Save;
  end;
end;

procedure TFCustomJupiterForm.PrepareForm;
begin
  inherited PrepareForm;

  DrawForm(Self);

  Self.FFormGenerator.Variables.CopyFromVariableList(Self.Generator.Fields);

  if ((Assigned(Self.FFormGenerator.Variables)) and (Self.FFormGenerator.Variables.Size > 0)) then
    Self.FFormGenerator.DrawForm;
end;

end.

