unit ulayoutreader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ValEdit,
  StdCtrls, SynEdit, JupiterForm, JupiterConsts, JupiterAction, JupiterRunnable,
  JupiterEnviroment, jupiterformutils, JupiterFileDataProvider,
  JupiterCSVDataProvider, JupiterApp, jupiterlayoutvalidator, utimecontrol;

type

  { TFLayoutReader }

  TFLayoutReader = class(TFJupiterForm)
    cbLayouts: TComboBox;
    gbMessage: TGroupBox;
    lbFile: TSynEdit;
    lbMessages: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    pnBottom: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    vlFields: TValueListEditor;
    procedure cbLayoutsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbFileChange(Sender: TObject);
    procedure lbFileClick(Sender: TObject);
  private
    FCurrentLine: String;
    FFileName : String;
    FCurrentLayout : TJupiterCSVDataProvider;

    procedure Internal_OpenItemClick(Sender: TObject);
    procedure Internal_SaveItemClick(Sender: TObject);
    procedure Internal_CompileItemClick(Sender: TObject);

    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_Resize; override;

    procedure Internal_ListLayouts;
  public

  end;

var
  FLayoutReader: TFLayoutReader;

implementation

{$R *.lfm}

{ TFLayoutReader }

procedure TFLayoutReader.FormCreate(Sender: TObject);
begin
  inherited;

  lbFile.Lines.Clear;

  Self.FCurrentLayout := TJupiterCSVDataProvider.Create;
end;

procedure TFLayoutReader.cbLayoutsChange(Sender: TObject);
begin
  if ((cbLayouts.Items.Count = 0) or (cbLayouts.Text = EmptyStr)) then
    Exit;

  try
    Self.FCurrentLayout.ClearListItens;
    Self.FCurrentLayout.Filename := cbLayouts.Text;
    Self.FCurrentLayout.ProvideData;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFLayoutReader.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCurrentLayout);

  inherited;
end;

procedure TFLayoutReader.lbFileChange(Sender: TObject);
begin
  lbFileClick(Sender);
end;

procedure TFLayoutReader.lbFileClick(Sender: TObject);
var
  vrLineNum : Integer;
begin
  try
    vrLineNum := lbFile.CaretY - 1;

    Self.FCurrentLine := lbFile.Lines[vrLineNum];
  finally
    Self.UpdateForm();
  end;
end;

procedure TFLayoutReader.Internal_OpenItemClick(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.BasePath := vrJupiterApp.Params.VariableById('Enviroment.WorkDir').Value;

    Self.FFileName := vrEnviroment.OpenFile('*.txt');

    if (Self.FFileName = EmptyStr) then
      Exit;

    vrJupiterApp.Params.VariableById('Enviroment.WorkDir').Value := ExtractFileDir(Self.FFileName);

    lbFile.Lines.LoadFromFile(Self.FFileName);

    Self.FCurrentLine := EmptyStr;
  finally
    Self.UpdateForm;
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFLayoutReader.Internal_SaveItemClick(Sender: TObject);
begin
  lbFile.Lines.SaveToFile(Self.FFileName);
end;

procedure TFLayoutReader.Internal_CompileItemClick(Sender: TObject);
var
  vrErrorCount : Integer;
  vrVez : Integer;
  vrVez2 : Integer;
  vrValidator : TJupiterLayoutValidator;
  vrValidatorList : TStrings;
begin
  lbMessages.Items.Clear;

  vrErrorCount := 0;

  if Self.Params.Exists('Hint.Success') then
    Self.Params.DeleteVariable('Hint.Success');

  if Self.Params.Exists('Hint.Error') then
    Self.Params.DeleteVariable('Hint.Error');

  try
    try
      if ((cbLayouts.Items.Count = 0) or (cbLayouts.Text = EmptyStr)) then
        raise Exception.Create('Nenhum layout selecionado. Selecione um layout e tente novamente.');

      for vrVez := 0 to lbFile.Lines.Count - 1 do
      begin
        vrValidator := TJupiterLayoutValidator.Create;
        try
          vrValidator.FileName := cbLayouts.Text;
          vrValidator.Line     := lbFile.Lines[vrVez];

          vrValidatorList := vrValidator.Validate;

          vrErrorCount := vrErrorCount + vrValidatorList.Count;

          for vrVez2 := 0 to vrValidatorList.Count - 1 do
            lbMessages.Items.Add(Format('Linha: %0:d: %1:s', [vrVez + 1, vrValidatorList[vrVez2]]));

          FreeAndNil(vrValidatorList);
        finally
          FreeAndNil(vrValidator);
        end;
      end;

      if vrErrorCount = 0 then
      begin
        lbMessages.Items.Add('Nenhum erro foi encontrado no layout.');

        Self.Params.AddVariable('Hint.Success', EmptyStr, 'Hint de sucesso');
      end
      else
        Self.Params.AddVariable('Hint.Error', EmptyStr, 'Hint de erro');
    except
      lbMessages.Items.Add('Erro: ' + Exception(ExceptObject).Message);

      Self.Params.AddVariable('Hint.Error', EmptyStr, 'Hint de erro');
    end;
  finally
    Self.UpdateForm(False, False);
  end;
end;

procedure TFLayoutReader.Internal_PrepareForm;
var
  vrAction : TJupiterAction;
begin
  Self.FCurrentLine := EmptyStr;
  Self.Hint := 'Abra o arquivo que deseja analisar e selecione o layout desejado na barra de navegação';

  Self.FFileName := EmptyStr;

  lbFile.Lines.Clear;

  inherited Internal_PrepareForm;

  vrAction      := TJupiterAction.Create('Abrir', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para abrir um arquivo existente';
  vrAction.Icon := ICON_OPEN;
  vrAction.OnClick := @Internal_OpenItemClick;

  Self.Actions.Add(vrAction);

  vrAction      := TJupiterAction.Create('Salvar', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para salvar as alterações efetuadas';
  vrAction.Icon := ICON_SAVE;
  vrAction.OnClick := @Internal_SaveItemClick;

  Self.Actions.Add(vrAction);

  vrAction      := TJupiterAction.Create('Analisar', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para analisar o arquivo atual';
  vrAction.Icon := ICON_PLAY;
  vrAction.OnClick := @Internal_CompileItemClick;

  Self.Actions.Add(vrAction);

  pnBottom.Width := PercentOfScreen(Self.Width, 50);

  vlFields.DefaultColWidth := PercentOfScreen(vlFields.Width, 50);

  gbMessage.Height := PercentOfScreen(Self.Height, 30);

  Self.Internal_ListLayouts;
end;

procedure TFLayoutReader.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if Self.Actions.Count > 0 then
  begin
    Self.Actions.GetActionButton(0, sbActions).Enabled := ((Self.FFileName = EmptyStr) and (not FileExists(Self.FFileName)));
    Self.Actions.GetMenuItem(0).Enabled := ((Self.FFileName = EmptyStr) and (not FileExists(Self.FFileName)));

    if Self.Actions.Count >= 1 then
      Self.Actions.GetActionButton(1, sbActions).Enabled := FileExists(Self.FFileName);
  end;
end;

procedure TFLayoutReader.Internal_UpdateDatasets;
var
  vrVez        : Integer;
  vrStr        : String;
  vrStrContent : String;
begin
  inherited Internal_UpdateDatasets;

  vlFields.Strings.Clear;

  for vrVez := 0 to Self.FCurrentLayout.Count - 1 do
    with Self.FCurrentLayout.GetRowByIndex(vrVez) do
    begin
      vrStr := Fields.VariableById('Field').Value + ' (' + Fields.VariableById('Type').Value + ', ' + Fields.VariableById('Start').Value + ', ' + Fields.VariableById('Size').Value + ')';
      vrStrContent := Copy(FCurrentLine, StrToIntDef(Fields.VariableById('Start').Value, 0), StrToIntDef(Fields.VariableById('Size').Value, 0));

      vlFields.Strings.AddPair(vrStr, vrStrContent);
    end;
end;

procedure TFLayoutReader.Internal_Resize;
begin
  inherited Internal_Resize;

  vlFields.DefaultColWidth := PercentOfScreen(vlFields.Width, 49);
end;

procedure TFLayoutReader.Internal_ListLayouts;
var
  vrFile       : TJupiterFileDataProvider;
  vrEnviroment : TJupiterEnviroment;
  vrVez        : Integer;
begin
  vrFile       := TJupiterFileDataProvider.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrFile.Path := vrEnviroment.FullPath('/modules/tools/library/');
    vrFile.SubFolders := True;
    vrFile.ProvideData;

    cbLayouts.Items.Clear;

    for vrVez := 0 to vrFile.Count - 1 do
      with vrFile.GetRowByIndex(vrVez) do
        if vrEnviroment.IsOfExtension(Fields.VariableById('File').Value, '.jlt') then
          cbLayouts.Items.Add(Fields.VariableById('File').Value);

    if cbLayouts.Items.Count > 0 then
      cbLayouts.ItemIndex := -1;
  finally
    FreeAndNil(vrFile);
    FreeAndNil(vrEnviroment);
  end;
end;

end.

