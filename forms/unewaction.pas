unit uNewAction;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Buttons, EditBtn, FileCtrl, ShellCtrls, Arrow, Spin, uCustomJupiterForm,
  uMain, JupiterAction, JupiterConsts, JupiterRunnable, jupiterScript,
  JupiterEnviroment, jupiterformutils, LCLType, SynEdit, SynCompletion,
  SynHighlighterPas;

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
    seCode: TSynEdit;
    seIcon: TSpinEdit;
    SynAutoComplete1: TSynAutoComplete;
    SynCompletion1: TSynCompletion;
    SynPasSyn1: TSynPasSyn;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure edTitleUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
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
    procedure Internal_UpdateComponents; override;
  public

  end;

var
  FNewAction: TFNewAction;

implementation

{$R *.lfm}

{ TFNewAction }

procedure TFNewAction.FormCreate(Sender: TObject);
begin
  inherited;

  Self.ActionIndex := NULL_KEY;

  Self.IsModal := True;
end;

procedure TFNewAction.edTitleUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin

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

    if ((PageControl1.ActivePageIndex = 0) and (Trim(edRunnableFile.Text) = EmptyStr)) then
      raise Exception.Create('O campo Arquivo/Diretório é obrigatório');

    Self.Action.Title := edTitle.Text;
    Self.Action.Hint  := edHint.Text;
    Self.Action.Icon  := seIcon.Value;
    Self.Action.ConfirmBeforeExecute := cbConfirmBeforeRun.Checked;
    Self.Action.Runnable.CommandLine := edRunnableFile.Text;
    Self.Action.ActionType := PageControl1.ActivePageIndex;
    Self.Action.OnClickScript.Clear;
    Self.Action.OnClickScript.AddStrings(seCode.Lines);

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

  PageControl1.ActivePageIndex := Self.Action.ActionType;

  seCode.Lines.Clear;
  seCode.Lines.AddStrings(Self.Action.OnClickScript);
end;

procedure TFNewAction.Internal_UpdateComponents;
var
  vrVez : Integer;
  vrAnalyser : TJupiterScriptAnalyserList;
  vrScript     : TJupiterScript;
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Internal_UpdateComponents;

  vrScript     := TJupiterScript.Create;
    vrEnviroment := TJupiterEnviroment.Create;
    try
      vrScript.LoadFromFile(vrEnviroment.FullPath('modules/jpas/promptCommand.jpas'));

      SynAutoComplete1.AutoCompleteList.Clear;
      SynCompletion1.ItemList.Clear;

      vrAnalyser := vrScript.AnalyseCode;

      for vrVez := 0 to vrAnalyser.Count - 1 do
        with vrAnalyser.ItemByIndex(vrVez) do
        begin
          SynAutoComplete1.AutoCompleteList.Add(Text);
          SynCompletion1.ItemList.Add(Text);
        end;

      TStringList(SynAutoComplete1.AutoCompleteList).Sort;
      TStringList(SynCompletion1.ItemList).Sort;

      SynCompletion1.Width := PercentOfScreen(Self.Width, 40);
    finally
      FreeAndNil(vrScript);
      FreeAndNil(vrEnviroment);
    end;
end;

end.

