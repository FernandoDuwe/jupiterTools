unit ulayoutreader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ValEdit,
  StdCtrls, SynEdit, JupiterForm, JupiterConsts, JupiterAction, JupiterRunnable,
  JupiterEnviroment, jupiterformutils, JupiterFileDataProvider,
  JupiterCSVDataProvider, JupiterApp;

type

  { TFLayoutReader }

  TFLayoutReader = class(TFJupiterForm)
    cbLayouts: TComboBox;
    lbFile: TListBox;
    Panel1: TPanel;
    pnBottom: TPanel;
    Splitter1: TSplitter;
    vlFields: TValueListEditor;
    procedure cbLayoutsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbFileClick(Sender: TObject);
    procedure seFileTextChange(Sender: TObject);
    procedure seFileTextClick(Sender: TObject);
  private
    FCurrentLine: String;
    FFileName : String;
    FCurrentLayout : TJupiterCSVDataProvider;

    procedure Internal_OpenItemClick(Sender: TObject);
    procedure Internal_SaveItemClick(Sender: TObject);

    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;

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

procedure TFLayoutReader.lbFileClick(Sender: TObject);
begin
  try
    Self.FCurrentLine := EmptyStr;

    if lbFile.ItemIndex >= 0 then
      Self.FCurrentLine := lbFile.Items[lbFile.ItemIndex];
  finally
    Self.UpdateForm();
  end;
end;

procedure TFLayoutReader.seFileTextChange(Sender: TObject);
begin

end;

procedure TFLayoutReader.seFileTextClick(Sender: TObject);
begin

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

    lbFile.Items.LoadFromFile(Self.FFileName);

    Self.FCurrentLine := EmptyStr;
  finally
    Self.UpdateForm;
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFLayoutReader.Internal_SaveItemClick(Sender: TObject);
begin
  lbFile.Items.SaveToFile(Self.FFileName);
end;

procedure TFLayoutReader.Internal_PrepareForm;
var
  vrAction : TJupiterAction;
begin
  Self.FCurrentLine := EmptyStr;
  Self.Hint := 'Abra o arquivo que deseja analisar e selecione o layout desejado na barra de navegação';

  Self.FFileName := EmptyStr;

  lbFile.Items.Clear;

  inherited Internal_PrepareForm;

  vrAction      := TJupiterAction.Create('Abrir', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para abrir um arquivo existente';
  vrAction.Icon := ICON_OPEN;
  vrAction.OnClick := @Internal_OpenItemClick;

  Self.Actions.Add(vrAction);
                                           {
  vrAction      := TJupiterAction.Create('Salvar', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para salvar as alterações efetuadas';
  vrAction.Icon := ICON_SAVE;
  vrAction.OnClick := @Internal_SaveItemClick;

  Self.Actions.Add(vrAction);
  }

  pnBottom.Width := PercentOfScreen(Self.Width, 50);

  vlFields.DefaultColWidth := PercentOfScreen(vlFields.Width, 50);

  Self.Internal_ListLayouts;
end;

procedure TFLayoutReader.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if Self.Actions.Count > 0 then
    Self.Actions.GetActionButton(0, sbActions).Enabled := ((Self.FFileName = EmptyStr) and (not FileExists(Self.FFileName)));

//  if Self.Actions.Count >= 1 then
//    Self.Actions.GetActionButton(1, sbActions).Enabled := FileExists(Self.FFileName);

  // lbFile.ReadOnly := ((Self.FFileName = EmptyStr) and (not FileExists(Self.FFileName)));
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

