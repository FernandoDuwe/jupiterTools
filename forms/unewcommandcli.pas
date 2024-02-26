unit unewcommandcli;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Buttons, SynEdit, SynCompletion, SynHighlighterPas, uCustomJupiterForm,
  jupiterclicommand, JupiterAction, JupiterConsts, jupiterScript,
  JupiterEnviroment, jupiterformutils, JupiterDialogForm, uMain;

type

  { TFNewCommandCli }

  TFNewCommandCli = class(TFCustomJupiterForm)
    edCommandName: TEdit;
    edCommand: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    lvMenuVariables: TListView;
    pcCommand: TPageControl;
    sbParamDelete: TSpeedButton;
    sbMenuParamDelete: TSpeedButton;
    seCode: TSynEdit;
    SynAutoComplete1: TSynAutoComplete;
    SynCompletion1: TSynCompletion;
    SynPasSyn1: TSynPasSyn;
    tsCode: TTabSheet;
    tsParams: TTabSheet;
    procedure FormShow(Sender: TObject);
    procedure lvMenuVariablesDblClick(Sender: TObject);
    procedure lvMenuVariablesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure sbMenuParamDeleteClick(Sender: TObject);
  private
    FCommandCLI : TJupiterCLICommand;

    procedure Internal_PrepareForm; override;

    procedure Internal_SaveActionClick(Sender : TObject);
    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_Resize; override;
  published
    property CommandCLI : TJupiterCLICommand read FCommandCLI write FCommandCLI;
  public

  end;

var
  FNewCommandCli: TFNewCommandCli;

implementation

{$R *.lfm}

{ TFNewCommandCli }

procedure TFNewCommandCli.sbMenuParamDeleteClick(Sender: TObject);
var
  vrDialog : TJupiterDialogForm;
begin
  vrDialog := TJupiterDialogForm.Create;
  try
    vrDialog.Title := 'Novo Parâmetro';
    vrDialog.Hint  := 'Crie um novo parâmetro.';

    vrDialog.Fields.AddField('NAME', 'Nome do Parâmetro', '');

    if vrDialog.Show then
    begin
      Self.CommandCLI.AddParam(vrDialog.Fields.VariableById('NAME').Value, True);

      Self.UpdateForm();
    end;
  finally
    FreeAndNil(vrDialog);
  end;
end;

procedure TFNewCommandCli.lvMenuVariablesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  Self.UpdateForm(False);
end;

procedure TFNewCommandCli.lvMenuVariablesDblClick(Sender: TObject);
begin
  if not Assigned(lvMenuVariables.Selected) then
    Exit;

  if not Assigned(lvMenuVariables.Selected.Data) then
    Exit;

  try
    TJupiterCLICommandParam(lvMenuVariables.Selected.Data).Required := not TJupiterCLICommandParam(lvMenuVariables.Selected.Data).Required;

    lvMenuVariables.Selected := nil;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFNewCommandCli.FormShow(Sender: TObject);
begin
  inherited;
end;

procedure TFNewCommandCli.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  if not Assigned(Self.CommandCLI) then
  begin
    Self.CommandCLI := TJupiterCLICommand.Create;
  end;

  edCommandName.Text := Self.CommandCLI.CommandName;
  edCommand.Text := Self.CommandCLI.Command;

  seCode.Lines.Clear;
  seCode.Lines.AddStrings(Self.CommandCLI.CommandText);

  Self.Hint := 'Comando a ser chamado pelo CLI Europa';

  Self.Actions.Add(TJupiterAction.Create('Salvar', @Internal_SaveActionClick));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para salvar a ação';
    Icon := ICON_SAVE;
  end;
end;

procedure TFNewCommandCli.Internal_SaveActionClick(Sender: TObject);
begin
  Self.CommandCLI.CommandName := edCommandName.Text;
  Self.CommandCLI.Command := edCommand.Text;
  Self.CommandCLI.CommandText.Clear;
  Self.CommandCLI.CommandText.AddStrings(seCode.Lines);

  Self.CommandCLI.SaveToFile;

  Self.Close;
end;

procedure TFNewCommandCli.Internal_UpdateComponents;
var
  vrVez : Integer;
  vrAnalyser : TJupiterScriptAnalyserList;
  vrScript     : TJupiterScript;
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Internal_UpdateComponents;

  sbParamDelete.Enabled := Assigned(lvMenuVariables.Selected);

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

procedure TFNewCommandCli.Internal_UpdateDatasets;
var
  vrVez : Integer;
  vrListItem : TListItem;
  vrParam : TJupiterCLICommandParam;
begin
  inherited Internal_UpdateDatasets;

  lvMenuVariables.Items.Clear;

  for vrVez := 0 to Self.CommandCLI.ParamList.Count - 1 do
  begin
    vrParam := TJupiterCLICommandParam(Self.CommandCLI.ParamList.GetAtIndex(vrVez));

    vrListItem := lvMenuVariables.Items.Add;
    vrListItem.Caption := vrParam.ParamName;

    if vrParam.Required then
      vrListItem.SubItems.Add('Sim')
    else
      vrListItem.SubItems.Add('Não');

    vrListItem.Data := vrParam;
  end;
end;

procedure TFNewCommandCli.Internal_Resize;
begin
  inherited Internal_Resize;

  lvMenuVariables.Columns[0].Width := PercentOfScreen(lvMenuVariables.Width, 69);
  lvMenuVariables.Columns[1].Width := PercentOfScreen(lvMenuVariables.Width, 30);
end;

end.
