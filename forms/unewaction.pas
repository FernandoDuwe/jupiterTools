unit uNewAction;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Buttons, EditBtn, FileCtrl, ShellCtrls, Arrow, Spin, uCustomJupiterForm,
  uMain, JupiterAction, JupiterConsts, JupiterRunnable;

type

  { TFNewAction }

  TFNewAction = class(TFCustomJupiterForm)
    cbConfirmBeforeRun: TCheckBox;
    edTitle: TEdit;
    edHint: TEdit;
    edRunnableFile: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    PageControl1: TPageControl;
    sbIcon: TSpeedButton;
    seIcon: TSpinEdit;
    TabSheet1: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure seIconChange(Sender: TObject);
  private
    FAction      : TJupiterAction;
    FActionIndex : Integer;

    procedure Internal_SaveActionClick(Sender : TObject);
  published
    property Action      : TJupiterAction read FAction      write FAction;
    property ActionIndex : Integer        read FActionIndex write FActionIndex;
  protected
    procedure Internal_PrepareForm; override;
  public

  end;

var
  FNewAction: TFNewAction;

implementation

uses LCLType;

{$R *.lfm}

{ TFNewAction }

procedure TFNewAction.FormCreate(Sender: TObject);
begin
  inherited;

  Self.ActionIndex := NULL_KEY;
end;

procedure TFNewAction.seIconChange(Sender: TObject);
begin
  sbIcon.ImageIndex := seIcon.Value;
end;

procedure TFNewAction.Internal_SaveActionClick(Sender: TObject);
begin
  try
    if Trim(edTitle.Text) = EmptyStr then
      raise Exception.Create('O campo Título é obrigatório');

    if Trim(edRunnableFile.Text) = EmptyStr then
      raise Exception.Create('O campo Arquivo/Diretório é obrigatório');

    Self.Action.Title := edTitle.Text;
    Self.Action.Hint  := edHint.Text;
    Self.Action.Icon  := seIcon.Value;
    Self.Action.ConfirmBeforeExecute := cbConfirmBeforeRun.Checked;
    Self.Action.Runnable.CommandLine := edRunnableFile.Text;

    Self.ModalResult := mrOK;
  except
    Application.MessageBox(PAnsiChar(Exception(ExceptObject).Message), PAnsiChar(Self.Caption), MB_ICONERROR + MB_OK);
  end;
end;

procedure TFNewAction.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  if not Assigned(Self.Action) then
  begin
    Self.Action      := TJupiterAction.Create(EmptyStr, TJupiterRunnable.Create(EmptyStr));
    Self.ActionIndex := NULL_KEY;
    Self.Caption     := 'Nova ação';
  end
  else
    Self.Caption := 'Editar ação';

  Self.Hint := 'Com as ações, você pode criar novas funcionalidades nos seus formulários.';

  Self.Actions.Add(TJupiterAction.Create('Salvar', @Internal_SaveActionClick));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para salvar a ação';
    Icon := ICON_SAVE;
  end;

  edTitle.Text := Self.Action.Title;
  edHint.Text  := Self.Action.Hint;

  sbIcon.Images := FMain.ilIconFamily;

  edRunnableFile.Text := Self.Action.Runnable.CommandLine;
  seIcon.MinValue := NULL_KEY;
  seIcon.MaxValue := FMain.ilIconFamily.Count - 1;
  seIcon.Value    := Self.Action.Icon;

  cbConfirmBeforeRun.Checked := Self.Action.ConfirmBeforeExecute;
end;

end.

