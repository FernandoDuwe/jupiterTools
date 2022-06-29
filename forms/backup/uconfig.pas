unit uConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, uJupiterForm, uEditList, JupiterApp, JupiterConfig;

type

  { TFConfig }

  TFConfig = class(TJupiterForm)
    btCancel: TButton;
    btSave: TButton;
    btEdit: TButton;
    btNew: TButton;
    edValue: TEdit;
    edID: TEdit;
    edDescr: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lvParams: TListView;
    pnForm: TPanel;
    sbViewList: TSpeedButton;
    Splitter1: TSplitter;
    procedure btCancelClick(Sender: TObject);
    procedure btEditClick(Sender: TObject);
    procedure btNewClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvParamsClick(Sender: TObject);
    procedure sbViewListClick(Sender: TObject);
  private
    FCurrentItem : TJupiterConfigItem;
    FEditMode : Boolean;

    procedure Internal_UpdateDatasets; override;
    procedure Internal_UpdateComponents; override;
  public

  end;

var
  FConfig: TFConfig;

implementation

{$R *.lfm}

{ TFConfig }

procedure TFConfig.lvParamsClick(Sender: TObject);
begin
  if not Assigned(lvParams.Selected) then
    Exit;

  if not Assigned(lvParams.Selected.Data) then
    Exit;

  try
    Self.FCurrentItem := TJupiterConfigItem(lvParams.Selected.Data);
  finally
    Self.UpdateForm;
  end;
end;

procedure TFConfig.sbViewListClick(Sender: TObject);
begin
  Application.CreateForm(TFEditList, FEditList);
  try
    FEditList.List := edValue.Text;

    FEditList.ShowModal;

    edValue.Text := FEditList.List;
  finally
    FEditList.Release;
    FreeAndNil(FEditList)
  end;
end;

procedure TFConfig.FormCreate(Sender: TObject);
begin
  Self.FCurrentItem := nil;
  Self.FEditMode    := False;
end;

procedure TFConfig.FormShow(Sender: TObject);
begin
  if vrJupiterApp.Config.GetByID('JupiterTools.UI.Display.WindowsState').Value = 'Maximized' then
    Self.WindowState := wsMaximized;
end;

procedure TFConfig.btNewClick(Sender: TObject);
begin
  try
    Self.FCurrentItem := TJupiterConfigItem.Create(EmptyStr, EmptyStr, EmptyStr);

    Self.FEditMode := True;
  finally
    Self.UpdateForm;

    edID.Text := 'User.';

    if edID.CanFocus then
      edID.SetFocus;
  end;
end;

procedure TFConfig.btSaveClick(Sender: TObject);
begin
  if Trim(edID.Text) = EmptyStr then
    raise Exception.Create('Identificador é obrigatório');

  if Trim(edDescr.Text) = EmptyStr then
    raise Exception.Create('Descrição é obrigatória');

  try
    Self.FCurrentItem := nil;
    Self.FEditMode    := False;

    vrJupiterApp.Config.AddConfig(edID.Text, edValue.Text, edDescr.Text);

    lvParams.Items.Clear;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFConfig.btEditClick(Sender: TObject);
begin
  try
    Self.FEditMode := True;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFConfig.btCancelClick(Sender: TObject);
begin
  try
    Self.FCurrentItem := nil;
    Self.FEditMode    := False;

    lvParams.Items.Clear;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFConfig.Internal_UpdateDatasets;
var
  vrVez : Integer;
  vrNode : TListItem;
  vrItem : TJupiterConfigItem;
begin
  inherited Internal_UpdateDatasets;

  if Assigned(Self.FCurrentItem) then
  begin
    edID.Text    := Self.FCurrentItem.ID;
    edDescr.Text := Self.FCurrentItem.Description;
    edValue.Text := Self.FCurrentItem.Value;
  end;

  if lvParams.Items.Count = 0 then
  begin
    lvParams.SortType := stNone;

    lvParams.DisableAutoSizing;
    lvParams.Items.Clear;

    for vrVez := 0 to vrJupiterApp.Config.Count - 1 do
    begin
      vrNode := lvParams.Items.Add;

      vrItem := vrJupiterApp.Config.GetByIndex(vrVez);

      vrNode.Caption := vrItem.ID;
      vrNode.SubItems.Add(vrItem.Description);
      vrNode.SubItems.Add(vrItem.Value);
      vrNode.Data := vrItem;

      if Assigned(Self.FCurrentItem) then
        if vrItem.ID = Self.FCurrentItem.ID then
          lvParams.Selected := vrNode;
    end;

    lvParams.SortType := stText;
    lvParams.SortColumn := 0;
    lvParams.SortDirection:= sdAscending;
    lvParams.Sort;

    if lvParams.Column[0].AutoSize then
      lvParams.EnableAutoSizing;
  end;
end;

procedure TFConfig.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  sbViewList.Height := edValue.Height;
  sbViewList.Width  := edValue.Height;

  btNew.Enabled      := False;
  btEdit.Enabled     := False;
  btSave.Enabled     := False;
  btCancel.Enabled   := False;
  sbViewList.Enabled := False;

  btNew.Enabled  := not Self.FEditMode;

  lvParams.Font.Size := StrToInt(vrJupiterApp.Config.GetByID('JupiterTools.UI.Display.FontSize').Value);

  if not Assigned(Self.FCurrentItem) then
    Exit;

  btEdit.Enabled   := ((not Self.FEditMode) and (Assigned(Self.FCurrentItem))) and (Self.FCurrentItem.CanSave);
  btSave.Enabled   := Self.FEditMode;
  btCancel.Enabled := Self.FEditMode;

  edID.Enabled       := (Self.FEditMode) and (Assigned(Self.FCurrentItem)) and (Self.FCurrentItem.ID = EmptyStr);
  edDescr.Enabled    := (Self.FEditMode) and (Assigned(Self.FCurrentItem));
  edValue.Enabled    := (Self.FEditMode) and (Assigned(Self.FCurrentItem));
  sbViewList.Enabled := (Self.FEditMode) and (Assigned(Self.FCurrentItem));
end;

end.

