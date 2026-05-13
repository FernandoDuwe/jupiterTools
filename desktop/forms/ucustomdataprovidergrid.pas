unit uCustomDataProviderGrid;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, uJupiterForm, JupiterDataProvider, JupiterApp, JupiterVariable,
  JupiterConsts, jupiterformutils, uJupiterAction;

type

  { TFCustomDataProviderGrid }

  TFCustomDataProviderGrid = class(TFJupiterForm)
    gbData: TGroupBox;
    lvColumns: TListView;
    mmData: TMemo;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure lvColumnsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvColumnsClick(Sender: TObject);
  private
    FLimit : Integer;

    FDataProvider : TJupiterDataProvider;

    procedure Internal_OnShowAll(Sender: TObject);

    procedure Internal_PrepareForm; override;

    procedure Internal_UpdateDatasets; override;
    procedure Internal_UpdateComponents; override;
  public
    procedure FromReference(prReference : String);
  end;

var
  FCustomDataProviderGrid: TFCustomDataProviderGrid;

implementation

{$R *.lfm}

{ TFCustomDataProviderGrid }

procedure TFCustomDataProviderGrid.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FLimit := vrJupiterApp.Params.VariableById('Interface.Form.GridLimit').AsInteger;
end;

procedure TFCustomDataProviderGrid.lvColumnsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
end;

procedure TFCustomDataProviderGrid.lvColumnsClick(Sender: TObject);
var
  vrVez : Integer;
begin
  mmData.Lines.Clear;

  if not Assigned(lvColumns.Selected) then
    Exit;

  mmData.Lines.Add(lvColumns.Column[0].Caption + ':');
  mmData.Lines.Add(lvColumns.Selected.Caption);

  for vrVez := 0 to lvColumns.Selected.SubItems.Count - 1 do
  begin
    mmData.Lines.Add(EmptyStr);
    mmData.Lines.Add(lvColumns.Column[vrVez + 1].Caption + ':');
    mmData.Lines.Add(lvColumns.Selected.SubItems[vrVez]);
  end;

  mmData.CaretPos := Point(0, 0);
end;

procedure TFCustomDataProviderGrid.Internal_OnShowAll(Sender: TObject);
begin
  try
    Self.FLimit := Self.FDataProvider.Count;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFCustomDataProviderGrid.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Todos os registros', 'Clique aqui exibir todos os registros', ICON_VIEW, @Internal_OnShowAll));

  mmData.Lines.Clear;
end;

procedure TFCustomDataProviderGrid.Internal_UpdateDatasets;
var
  vrVez : Integer;
  vrVez2 : Integer;
  vrListItem : TListItem;
  vrCounter : Integer;
begin
  inherited Internal_UpdateDatasets;

  if not Assigned(Self.FDataProvider) then
    Exit;

  lvColumns.DisableAutoSizing;
  try
    for vrVez := 0 to lvColumns.Columns.Count - 1 do
      lvColumns.Columns[vrVez].AutoSize := False;

    lvColumns.Items.Clear;

    vrCounter := Self.FLimit;

    if vrCounter > (Self.FDataProvider.Count - 1) then
      vrCounter := Self.FDataProvider.Count - 1;

    for vrVez := 0 to vrCounter do
      with Self.FDataProvider.GetRowByIndex(vrVez) do
      begin
        vrListItem := lvColumns.Items.Add;

        for vrVez2 := 0 to Fields.Count - 1 do
        begin
          if vrVez2 = 0 then
            vrListItem.Caption := Fields.VariableByIndex(vrVez2).Value
          else
            vrListItem.SubItems.Add(Fields.VariableByIndex(vrVez2).Value);
        end;
      end;
  finally
    for vrVez := 0 to lvColumns.Columns.Count - 1 do
      lvColumns.Columns[vrVez].AutoSize := True;

    lvColumns.EnableAutoSizing;
  end;
end;

procedure TFCustomDataProviderGrid.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if miLookColumn.Checked then
    gbData.Height := PercentOfScreen(Self.Height, Self.PercentDivisor);
end;

procedure TFCustomDataProviderGrid.FromReference(prReference: String);
var
  vrVez : Integer;
  vrListColumn : TListColumn;
begin
  for vrVez := 0 to vrJupiterApp.DataProviders.Count - 1 do
    if TJupiterDataProvider(vrJupiterApp.DataProviders.GetAtIndex(vrVez)).ProviderID = prReference then
      Self.FDataProvider := TJupiterDataProvider(vrJupiterApp.DataProviders.GetAtIndex(vrVez));

  if not Assigned(Self.FDataProvider) then
    Exit;

  if Self.FDataProvider.Count = 0 then
    Exit;

  Self.Caption := 'Data Provider: ' + prReference;

  lvColumns.Columns.Clear;

  for vrVez := 0 to Self.FDataProvider.GetRowByIndex(0).Fields.Count - 1 do
  begin
    vrListColumn := lvColumns.Columns.Add;
    vrListColumn.Caption := TJupiterVariable(Self.FDataProvider.GetRowByIndex(0).Fields.GetAtIndex(vrVez)).Title;

    if Trim(vrListColumn.Caption) = EmptyStr then
      vrListColumn.Caption := TJupiterVariable(Self.FDataProvider.GetRowByIndex(0).Fields.GetAtIndex(vrVez)).ID;

    vrListColumn.AutoSize := False;
  end;

  Self.UpdateForm();
end;

end.

