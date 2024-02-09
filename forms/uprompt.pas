unit uPrompt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, SynEdit,
  SynCompletion, SynHighlighterPas, JupiterForm, JupiterAction, JupiterConsts,
  jupiterformutils, jupiterScript, JupiterEnviroment, Types, LCLType;

type

  { TFPrompt }

  TFPrompt = class(TFJupiterForm)
    sePrompt: TSynEdit;
    SynAutoComplete1: TSynAutoComplete;
    SynCompletion1: TSynCompletion;
    SynPasSyn1: TSynPasSyn;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SynCompletion1CodeCompletion(var Value: string;
      SourceValue: string; var SourceStart, SourceEnd: TPoint;
      KeyChar: TUTF8Char; Shift: TShiftState);
  private
    FEditMode : Boolean;

    procedure Internal_SetEditMode(prEditMode : Boolean);
    procedure Internal_SaveActionClick(Sender : TObject);
    procedure Internal_ClearActionClick(Sender : TObject);
    procedure Internal_CloseActionClick(Sender : TObject);

    procedure Internal_UpdateComponents; override;
  published
    property EditMode : Boolean read FEditMode write Internal_SetEditMode;
  protected
    procedure Internal_PrepareForm; override;
  public

  end;

var
  FPrompt: TFPrompt;

implementation

{$R *.lfm}

{ TFPrompt }

procedure TFPrompt.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FEditMode := True;

  sePrompt.Lines.Clear;
end;

procedure TFPrompt.SynCompletion1CodeCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
begin

end;

procedure TFPrompt.Internal_SetEditMode(prEditMode: Boolean);
begin
  Self.FEditMode := prEditMode;

  Self.UpdateForm;
end;

procedure TFPrompt.FormActivate(Sender: TObject);
begin
  inherited;

  sePrompt.SetFocus;
end;

procedure TFPrompt.Internal_SaveActionClick(Sender: TObject);
var
  vrScript     : TJupiterScript;
  vrEnviroment : TJupiterEnviroment;
begin
  if not Self.FEditMode then
    Exit;

  vrScript     := TJupiterScript.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrScript.LoadFromFile(vrEnviroment.FullPath('modules/jpas/promptCommand.jpas'));

    vrScript.UserCommand := sePrompt.Text;
    vrScript.Execute;

    sePrompt.Lines.Add(EmptyStr);

    if not vrScript.Runned then
      sePrompt.Lines.AddStrings(vrScript.Messages)
    else
      sePrompt.Lines.AddStrings(vrScript.RunMessages);

    Self.FEditMode := False;
    Self.UpdateForm;
  finally
    FreeAndNil(vrScript);
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFPrompt.Internal_ClearActionClick(Sender: TObject);
begin
  try
    sePrompt.Lines.Clear;

    Self.FEditMode := True;
    Self.UpdateForm;
  finally
    sePrompt.SetFocus;
  end;
end;

procedure TFPrompt.Internal_CloseActionClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TFPrompt.Internal_UpdateComponents;
var
  vrVez : Integer;
  vrAnalyser : TJupiterScriptAnalyserList;
  vrScript     : TJupiterScript;
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Internal_UpdateComponents;

  sePrompt.ReadOnly := not Self.EditMode;

  Self.Actions.GetActionButton(0, sbActions).Enabled := Self.EditMode;
  Self.Actions.GetActionButton(1, sbActions).Enabled := not Self.EditMode;

  Self.Actions.GetMenuItem(0).Enabled := Self.EditMode;
  Self.Actions.GetMenuItem(1).Enabled := not Self.EditMode;

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

procedure TFPrompt.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.Actions.Add(TJupiterAction.Create('Executar', @Internal_SaveActionClick));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para executar o cómando';
    Icon := ICON_PLAY;
  end;

  Self.Actions.Add(TJupiterAction.Create('Limpar', @Internal_ClearActionClick));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para limpar o prompt';
    Icon := ICON_DELETE;
  end;

  Self.Actions.Add(TJupiterAction.Create('Fechar', @Internal_CloseActionClick));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para fechar o prompt';
    Icon := ICON_EXIT;
  end;

  try
    Self.Position := poDefault;

    Self.Width  := PercentOfScreen(Screen.Width, 50);
    Self.Height := PercentOfScreen(Screen.Width, 30);
  finally
    Self.Position := poScreenCenter;
  end;
end;

end.

