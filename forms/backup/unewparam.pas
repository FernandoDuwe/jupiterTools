unit uNewParam;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uCustomJupiterForm, JupiterVariable, JupiterConsts, JupiterAction;

type

  { TFNewParam }

  TFNewParam = class(TFCustomJupiterForm)
    edDescription: TEdit;
    edID: TEdit;
    edValue: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    FParam      : TJupiterVariable;
    FParamIndex : Integer;

    procedure Internal_SaveFieldClick(Sender : TObject);
  published
    property Param      : TJupiterVariable read FParam      write FParam;
    property ParamIndex : Integer          read FParamIndex write FParamIndex;
  protected
    procedure Internal_PrepareForm; override;
  public

  end;

var
  FNewParam: TFNewParam;

implementation

uses LCLType;

{$R *.lfm}

{ TFNewParam }

procedure TFNewParam.FormCreate(Sender: TObject);
begin
  inherited;

  Self.ParamIndex := NULL_KEY;
end;

procedure TFNewParam.Internal_SaveFieldClick(Sender: TObject);
begin
  try
    if Trim(edID.Text) = EmptyStr then
      raise Exception.Create('O campo ID é obrigatório');

    if Trim(edDescription.Text) = EmptyStr then
      raise Exception.Create('O campo Descrição é obrigatório');

    Self.Param.ID    := edID.Text;
    Self.Param.Value := edValue.Text;
    Self.Param.Title := edDescription.Text;

    Self.ModalResult := mrOK;
  except
    Application.MessageBox(PAnsiChar(Exception(ExceptObject).Message), PAnsiChar(Self.Caption), MB_ICONERROR + MB_OK);
  end;
end;

procedure TFNewParam.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  if not Assigned(Self.Param) then
  begin
    Self.Param      := TJupiterVariable.Create;
    Self.ParamIndex := NULL_KEY;
    Self.Caption    := 'Novo parâmetro';
  end
  else
    Self.Caption := 'Editar parâmetro';

  Self.Hint := 'Nos menus criados, adicione parâmetros para customizar as ações.';

  Self.Actions.Add(TJupiterAction.Create('Salvar', @Internal_SaveFieldClick));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para salvar o parâmetro';
    Icon := ICON_SAVE;
  end;

  edID.Text             := Self.Param.ID;
  edValue.Text          := Self.Param.Value;
  edDescription.Text    := Self.Param.Title;
end;

end.

