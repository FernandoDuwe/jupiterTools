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
    FGenerator : TJupiterFormGenerator;
  published
    property Generator : TJupiterFormGenerator read FGenerator write FGenerator;
  protected
    procedure Internal_PrepareForm; override;
  public

  end;

var
  FCustomJupiterForm: TFCustomJupiterForm;

implementation

{$R *.lfm}

{ TFCustomJupiterForm }

procedure TFCustomJupiterForm.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FGenerator := TJupiterFormGenerator.Create;
  Self.FGenerator.Container := sbBody;
end;

procedure TFCustomJupiterForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGenerator);

  inherited;
end;

procedure TFCustomJupiterForm.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;
end;

end.

