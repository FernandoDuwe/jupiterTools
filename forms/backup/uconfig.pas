unit uConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, uJupiterForm, JupiterApp, JupiterConfig;

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
    procedure FormCreate(Sender: TObject);
    procedure lvParamsClick(Sender: TObject);
  private
    FCurrentItem : TJupiterConfigItem;

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

procedure TFConfig.FormCreate(Sender: TObject);
begin
  Self.FCurrentItem := nil;
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
end;

procedure TFConfig.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

   btNew.Enabled    := False;
   btEdit.Enabled   := False;
   btSave.Enabled   := False;
   btCancel.Enabled := False;

  if not Assigned(Self.FCurrentItem) then
    Exit;


end;

end.

