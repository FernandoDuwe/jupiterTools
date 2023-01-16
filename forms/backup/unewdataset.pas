unit uNewDataSet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  uCustomJupiterForm, JupiterConsts, JupiterAction, JupiterCSVDataProvider,
  JupiterDirectoryDataProvider, JupiterFileDataProvider;

type

  { TFNewDataSet }

  TFNewDataSet = class(TFCustomJupiterForm)
    cbCampo: TComboBox;
    cbSubFolders: TCheckBox;
    edDescription: TEdit;
    edFile: TEdit;
    edSearchNode: TEdit;
    edID: TEdit;
    gbProvider: TGroupBox;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lbProviders: TListBox;
    Splitter1: TSplitter;
    procedure FormShow(Sender: TObject);
    procedure lbProvidersClick(Sender: TObject);
  private
    procedure Internal_SaveClick(Sender : TObject);
  protected
    procedure Internal_PrepareForm; override;

    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
  public
    function GetFieldValue : String;
  end;

var
  FNewDataSet: TFNewDataSet;

implementation

uses LCLType;

{$R *.lfm}

{ TFNewDataSet }

procedure TFNewDataSet.lbProvidersClick(Sender: TObject);
begin
  Self.UpdateForm;
end;

procedure TFNewDataSet.FormShow(Sender: TObject);
begin
  inherited;

  gbProvider.Width := PercentOfScreen(Self.Width, 30);
end;

procedure TFNewDataSet.Internal_SaveClick(Sender: TObject);
begin
  try
    if Trim(edID.Text) = EmptyStr then
      raise Exception.Create('O campo ID é obrigatório');

    if Trim(edDescription.Text) = EmptyStr then
      raise Exception.Create('O campo Descrição é obrigatório');

    if Trim(cbCampo.Text) = EmptyStr then
      raise Exception.Create('O campo Campo é obrigatório');

    if Trim(edFile.Text) = EmptyStr then
      raise Exception.Create('O campo Caminho/Arquivo é obrigatório');

    if edSearchNode.Enabled then
      if Trim(edSearchNode.Text) = EmptyStr then
        raise Exception.Create('O campo Pesquisar em é obrigatório');

    Self.ModalResult := mrOK;
  except
    Application.MessageBox(PAnsiChar(Exception(ExceptObject).Message), PAnsiChar(Self.Caption), MB_ICONERROR + MB_OK);
  end;
end;

procedure TFNewDataSet.Internal_PrepareForm;
var
  vrVez : Integer;
begin
  inherited Internal_PrepareForm;

  Self.Actions.Add(TJupiterAction.Create('Salvar', @Internal_SaveClick));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para salvar';
    Icon := ICON_SAVE;
  end;

  lbProviders.Items.Clear;

  for vrVez := 0 to Length(PROVIDER_LIST) - 1 do
    lbProviders.Items.Add(PROVIDER_LIST[vrVez]);

  lbProviders.ItemIndex := 0;

  Self.Hint := 'Com o Datasets, você consegue criar variáveis de sistema com conteúdo dinâmico. Para começar, selecione um DataProvider.';
end;

procedure TFNewDataSet.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  cbSubFolders.Enabled := ((lbProviders.Items[lbProviders.ItemIndex] = DATAPROVIDER_TYPE_LIST_PATHS) or (lbProviders.Items[lbProviders.ItemIndex] = DATAPROVIDER_TYPE_LIST_FILES));

  edSearchNode.Enabled := (lbProviders.Items[lbProviders.ItemIndex] = DATAPROVIDER_TYPE_XML);
end;

procedure TFNewDataSet.Internal_UpdateDatasets;
var
  vrStr : TStrings;
begin
  inherited Internal_UpdateDatasets;

  vrStr := TStringList.Create;
  try
    if lbProviders.Items[lbProviders.ItemIndex] = DATAPROVIDER_TYPE_LIST_CSV then
      TJupiterCSVDataProvider.GetFieldsLayout(vrStr);

    if lbProviders.Items[lbProviders.ItemIndex] = DATAPROVIDER_TYPE_LIST_PATHS then
      TJupiterDirectoryDataProvider.GetFieldsLayout(vrStr);

    if lbProviders.Items[lbProviders.ItemIndex] = DATAPROVIDER_TYPE_LIST_FILES then
      TJupiterFileDataProvider.GetFieldsLayout(vrStr);

    cbCampo.Items.Clear;
    cbCampo.Items.AddStrings(vrStr);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

function TFNewDataSet.GetFieldValue: String;
begin
  Result := Format('%0:s(%1:s, %2:s)', [lbProviders.Items[lbProviders.ItemIndex], edFile.Text, cbCampo.Text]);

  if lbProviders.Items[lbProviders.ItemIndex] = DATAPROVIDER_TYPE_LIST_CSV then
    Result := Format('%0:s(%1:s, %2:s)', [lbProviders.Items[lbProviders.ItemIndex], edFile.Text, cbCampo.Text]);

  if lbProviders.Items[lbProviders.ItemIndex] = DATAPROVIDER_TYPE_LIST_PATHS then
    Result := Format('%0:s(%1:s, %2:s, %3:s)', [lbProviders.Items[lbProviders.ItemIndex], edFile.Text, cbCampo.Text, BoolToStr(cbSubFolders.Checked)]);

  if lbProviders.Items[lbProviders.ItemIndex] = DATAPROVIDER_TYPE_LIST_FILES then
    Result := Format('%0:s(%1:s, %2:s, %3:s)', [lbProviders.Items[lbProviders.ItemIndex], edFile.Text, cbCampo.Text, BoolToStr(cbSubFolders.Checked)]);

  if lbProviders.Items[lbProviders.ItemIndex] = DATAPROVIDER_TYPE_XML then
    Result := Format('%0:s(%1:s, %2:s, %3:s)', [lbProviders.Items[lbProviders.ItemIndex], edFile.Text, cbCampo.Text, edSearchNode.Text]);
end;

end.

