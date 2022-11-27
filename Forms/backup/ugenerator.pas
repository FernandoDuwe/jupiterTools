unit uGenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Buttons, uCustomJupiterForm, JupiterFileDataProvider,
  JupiterEnviroment, JupiterConsts, JupiterXMLDataProvider,
  JupiterGeneratorForm, JupiterAction, JupiterRunnable;

type

  { TFGenerator }

  TFGenerator = class(TFCustomJupiterForm)
    edFile: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbFormList: TListBox;
    lvActions: TListView;
    lvFields: TListView;
    mmLines: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pnForm: TPanel;
    pcTabs: TPageControl;
    sbActionDelete: TSpeedButton;
    sbFieldDelete: TSpeedButton;
    sbActionAdd: TSpeedButton;
    sbFieldAdd: TSpeedButton;
    spForms: TSplitter;
    Splitter1: TSplitter;
    tsForms: TTabSheet;
    tsInicio: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbFormListClick(Sender: TObject);
    procedure lvActionsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvFieldsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FFormID : String;

    procedure Internal_ShowForm(prFile : String);
    procedure Internal_PrepareForm; override;
    procedure Internal_RefreshClick(Sender: TObject);

    procedure Internal_UpdateFormForm;
  protected
    procedure Internal_UpdateDatasets; override;
  published
    property FormID : String read FFormID write FFormID;
  public

  end;

var
  FGenerator: TFGenerator;

implementation

uses uMain;

{$R *.lfm}

{ TFGenerator }

procedure TFGenerator.lbFormListClick(Sender: TObject);
begin
  if lbFormList.ItemIndex = NULL_KEY then
    Exit;

  Self.Internal_ShowForm(lbFormList.Items[lbFormList.ItemIndex]);
end;

procedure TFGenerator.lvActionsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  Self.Internal_UpdateFormForm;
end;

procedure TFGenerator.lvFieldsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  Self.Internal_UpdateFormForm;
end;

procedure TFGenerator.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FFormID := EmptyStr;
end;

procedure TFGenerator.FormShow(Sender: TObject);
begin
  inherited;

  if Self.FormID <> EmptyStr then
  begin
    pcTabs.ActivePageIndex := 1;

    lbFormList.ItemIndex := lbFormList.Items.IndexOf(Self.FormID + '.xml');

    if lbFormList.ItemIndex <> NULL_KEY then
      lbFormListClick(Sender);
  end;
end;

procedure TFGenerator.Internal_ShowForm(prFile: String);
var
  vrEnviroment : TJupiterEnviroment;
  vrGenerator  : TJupiterGeneratorForm;
  vrVez        : Integer;
  vrRow        : TListItem;
begin
  lvActions.Items.Clear;
  lvFields.Items.Clear;

  vrEnviroment := TJupiterEnviroment.Create;
  vrGenerator  := TJupiterGeneratorForm.Create;
  try
    edFile.Text := vrEnviroment.FullPath('modules/generator/' + prFile);

    vrGenerator.FormID := StringReplace(prFile, '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]);

    for vrVez := 0 to vrGenerator.Actions.Size - 1 do
    begin
      vrRow := lvActions.Items.Add;
      vrRow.Caption := TJupiterAction(vrGenerator.Actions.GetAtIndex(vrVez)).Title;
      vrRow.SubItems.Add(TJupiterAction(vrGenerator.Actions.GetAtIndex(vrVez)).Runnable.CommandLine);
    end;

    for vrVez := 0 to vrGenerator.Fields.Size - 1 do
    begin
      vrRow := lvFields.Items.Add;
      vrRow.Caption := vrGenerator.Fields.VariableByIndex(vrVez).ID;
      vrRow.SubItems.Add(vrGenerator.Fields.VariableByIndex(vrVez).Value);
      vrRow.SubItems.Add(vrGenerator.Fields.VariableByIndex(vrVez).Title);
    end;

    Self.Internal_UpdateFormForm;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFGenerator.Internal_PrepareForm;
var
  vrAction : TJupiterAction;
begin
  inherited Internal_PrepareForm;

  vrAction      := TJupiterAction.Create('Atualizar', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para atualizar a página';
  vrAction.Icon := ICON_REFRESH;
  vrAction.OnClick := @Internal_RefreshClick;

  Self.Actions.Add(vrAction);

  lvActions.Enabled := False;
  lvFields.Enabled  := False;

  sbActionAdd.Enabled    := False;
  sbActionAdd.Images     := FMain.ilIconFamily;
  sbActionAdd.ImageIndex := ICON_NEW;

  sbActionDelete.Enabled    := False;
  sbActionDelete.Images     := FMain.ilIconFamily;
  sbActionDelete.ImageIndex := ICON_DELETE;

  sbFieldAdd.Enabled    := False;
  sbFieldAdd.Images     := FMain.ilIconFamily;
  sbFieldAdd.ImageIndex := ICON_NEW;

  sbFieldDelete.Enabled    := False;
  sbFieldDelete.Images     := FMain.ilIconFamily;
  sbFieldDelete.ImageIndex := ICON_DELETE;

  lvActions.LargeImages := FMain.ilIconFamily;
  lvActions.SmallImages := FMain.ilIconFamily;
  lvActions.StateImages := FMain.ilIconFamily;

  Self.Hint := 'No módulo Generator você pode criar suas próprias funcionalidades no sistema';
end;

procedure TFGenerator.Internal_RefreshClick(Sender: TObject);
begin
  Self.UpdateForm;
end;

procedure TFGenerator.Internal_UpdateFormForm;
begin
  sbActionAdd.Enabled := Trim(edFile.Text) <> EmptyStr;
  sbFieldAdd.Enabled  := Trim(edFile.Text) <> EmptyStr;

  lvActions.Enabled := Trim(edFile.Text) <> EmptyStr;
  lvFields.Enabled := Trim(edFile.Text) <> EmptyStr;

  sbActionDelete.Enabled := Assigned(lvActions.Selected);
  sbFieldDelete.Enabled  := Assigned(lvFields.Selected);
end;

procedure TFGenerator.Internal_UpdateDatasets;
var
  vrFile       : TJupiterFileDataProvider;
  vrVez        : Integer;
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Internal_UpdateDatasets;

  lbFormList.Items.Clear;

  vrFile       := TJupiterFileDataProvider.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrFile.Path := vrEnviroment.FullPath('modules/generator/');
    vrFile.ProvideData;

    for vrVez := 0 to vrFile.Size - 1 do
      with vrFile.GetRowByIndex(vrVez) do
        lbFormList.Items.Add(Fields.VariableById('FieldName').Value);
  finally
    FreeAndNil(vrFile);
    FreeAndNil(vrEnviroment);
  end;
end;

end.

