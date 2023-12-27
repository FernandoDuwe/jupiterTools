unit uExplorer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Buttons,
  Grids, JupiterForm, JupiterAction, JupiterRunnable, JupiterDataProvider,
  JupiterConsts, JupiterApp, JupiterVariable, JupiterEnviroment,
  JupiterCSVDataProvider, JupiterDirectoryDataProvider, JupiterFileDataProvider,
  uCustomJupiterForm, uDynamicRecord, LCLType;

type

  { TFExplorer }

  TFExplorer = class(TFJupiterForm)
    lvItems: TListView;
    procedure lvItemsDblClick(Sender: TObject);
    procedure lvItemsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    FProvider  : TJupiterDataProvider;

    FBtnNewBtn  : Integer;
    FBtnEditBtn : Integer;
    FBtnDelBtn  : Integer;

    function  Internal_IfChecked(prValue1, prValue2 : String) : Boolean;
    function  InternalGetCheckMode : Boolean;
    function  InternalGetCheckListMode : Boolean;
    function  InternalCSVMode : Boolean;
    function  Internal_RecordChecked(prParams : TJupiterVariableList) : Boolean;
    procedure Internal_ChangeValue(var prParams : TJupiterVariableList);
    function  InternalGetRunMode : Boolean;

    procedure Internal_OnClick(prParams : TJupiterVariableList);
    procedure Internal_RefreshClick(Sender: TObject);
    procedure Internal_MarcarTodosClick(Sender: TObject);
    procedure Internal_DesmarcarTodosClick(Sender: TObject);
    procedure Internal_NewClick(Sender: TObject);
    procedure Internal_EditClick(Sender: TObject);
    procedure Internal_DeleteClick(Sender: TObject);
    function  Internal_HideColumn(prColumnName : String) : Boolean;
    procedure Internal_CheckBoxChangeAll(prNewValue : Boolean);
  published
    property CSVMode       : Boolean read InternalCSVMode;
    property CheckMode     : Boolean read InternalGetCheckMode;
    property ChecklistMode : Boolean read InternalGetCheckListMode;
    property RunMode       : Boolean read InternalGetRunMode;

    property Provider : TJupiterDataProvider read FProvider write FProvider;
  protected
    procedure Internal_PrepareForm; override;

    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
  public

  end;

var
  FExplorer: TFExplorer;

implementation

{$R *.lfm}

{ TFExplorer }

procedure TFExplorer.lvItemsDblClick(Sender: TObject);
begin
  if not Assigned(lvItems.Selected) then
    Exit;

  if not Assigned(lvItems.Selected.Data) then
    Exit;

  try
    Self.Internal_OnClick(TJupiterVariableList(lvItems.Selected.Data));
  finally
    Self.UpdateForm;
  end;
end;

procedure TFExplorer.lvItemsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  vrEdit : Boolean;
begin
  if Self.CSVMode then
  begin
    vrEdit := Assigned(Item) and Assigned(Item.Data);

    Self.Actions.GetActionButton(Self.FBtnEditBtn, sbActions).Enabled := vrEdit;
    Self.Actions.GetActionButton(Self.FBtnDelBtn, sbActions).Enabled  := vrEdit;

    Self.Actions.GetMenuItem(Self.FBtnEditBtn).Enabled := vrEdit;
    Self.Actions.GetMenuItem(Self.FBtnDelBtn).Enabled  := vrEdit;
  end;
end;

function TFExplorer.Internal_IfChecked(prValue1, prValue2: String): Boolean;
begin
  Result := prValue1 = prValue2;
end;

function TFExplorer.InternalGetCheckMode: Boolean;
begin
  Result := Self.Params.Exists('checkableField') and Self.Params.Exists('checkableVariable');
end;

function TFExplorer.InternalGetCheckListMode: Boolean;
begin
  Result := Self.Params.Exists('checklistField');
end;

function TFExplorer.InternalCSVMode: Boolean;
begin
  Result := False;

  if not Self.Params.Exists('path') then
    Exit;

  if Self.RunMode then
    Exit;

  if Self.CheckMode then
    Exit;

  Result := AnsiUpperCase(ExtractFileExt(Self.Params.VariableById('path').Value)) = '.CSV';
end;

function TFExplorer.Internal_RecordChecked(prParams : TJupiterVariableList) : Boolean;
begin
  Result := False;

  if not Self.ChecklistMode then
    Exit;

  if not prParams.Exists(Self.Params.VariableById('checklistField').Value) then
    Exit;

  Result := StrToIntDef(prParams.VariableById(Self.Params.VariableById('checklistField').Value).Value, 0) = 1;
end;

procedure TFExplorer.Internal_ChangeValue(var prParams: TJupiterVariableList);
begin
  if Self.Internal_RecordChecked(prParams) then
    prParams.VariableById(Self.Params.VariableById('checklistField').Value).Value := '0'
  else
    prParams.VariableById(Self.Params.VariableById('checklistField').Value).Value := '1';
end;

function TFExplorer.InternalGetRunMode: Boolean;
begin
  Result := Self.Params.Exists('runnableField');
end;

procedure TFExplorer.Internal_OnClick(prParams: TJupiterVariableList);
begin
  if Self.CheckMode then
    with vrJupiterApp.Params.VariableById(Self.Params.VariableById('checkableVariable').Value) do
    begin
      Value := prParams.VariableById(Self.Params.VariableById('checkableField').Value).Value;
      SaveConfig;
    end;

  if Self.RunMode then
    TJupiterRunnable.Create(prParams.VariableById(Self.Params.VariableById('runnableField').Value).Value, True);

  if Self.ChecklistMode then
  begin
    Self.Internal_ChangeValue(prParams);

    TJupiterCSVDataProvider(Self.Provider).SaveLine(prParams);
  end;

  if Self.CSVMode then
  begin
    Application.CreateForm(TFCustomJupiterForm, FCustomJupiterForm);
    try
      FCustomJupiterForm.IsModal := True;
      FCustomJupiterForm.Generator.Fields.CopyFromVariableList(prParams);

      FCustomJupiterForm.ShowModal;
    finally
      FCustomJupiterForm.Release;
      FreeAndNil(FCustomJupiterForm);
    end;
  end;
end;

procedure TFExplorer.Internal_RefreshClick(Sender: TObject);
begin
  Self.UpdateForm;
end;

function TFExplorer.Internal_HideColumn(prColumnName: String): Boolean;
begin
  Result := False;

  if not Self.Params.Exists('hideColumns') then
    Exit;

  Result := Pos(AnsiUpperCase(prColumnName), AnsiUpperCase(Self.Params.VariableById('hideColumns').Value)) <> 0;
end;

procedure TFExplorer.Internal_PrepareForm;
var
  vrProvider : String;
  vrParam    : String;
  vrAction   : TJupiterAction;
begin
  inherited Internal_PrepareForm;

  vrProvider := EmptyStr;
  vrParam := EmptyStr;

  if Self.Params.Exists('type') then
     vrProvider := Self.Params.VariableById('type').Value;

  if Self.Params.Exists('path') then
     vrParam := Self.Params.VariableById('path').Value;

  Self.FProvider := FactoryDataProvider(vrProvider, vrParam, Self.Params.Exists('subfolders'));

  vrAction      := TJupiterAction.Create('Atualizar', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para atualizar a página';
  vrAction.Icon := ICON_REFRESH;
  vrAction.OnClick := @Internal_RefreshClick;

  Self.Actions.Add(vrAction);

  if Self.ChecklistMode then
  begin
    vrAction      := TJupiterAction.Create('Marcar todos', TJupiterRunnable.Create(''), nil);
    vrAction.Hint := 'Clique aqui para marcar todos os itens';
    vrAction.Icon := ICON_CHECK;
    vrAction.OnClick := @Internal_MarcarTodosClick;

    Self.Actions.Add(vrAction);

    vrAction      := TJupiterAction.Create('Desmarcar todos', TJupiterRunnable.Create(''), nil);
    vrAction.Hint := 'Clique aqui para desmarcar todos os itens';
    vrAction.Icon := NULL_KEY;
    vrAction.OnClick := @Internal_DesmarcarTodosClick;

    Self.Actions.Add(vrAction);
  end;

  Self.FBtnNewBtn  := NULL_KEY;
  Self.FBtnEditBtn := NULL_KEY;
  Self.FBtnDelBtn  := NULL_KEY;

  if Self.CSVMode then
  begin
    vrAction      := TJupiterAction.Create('Novo', TJupiterRunnable.Create(''), nil);
    vrAction.Hint := 'Clique aqui para adicionar uma nova linha';
    vrAction.Icon := ICON_NEW;
    vrAction.OnClick := @Internal_NewClick;

    Self.Actions.Add(vrAction);

    Self.FBtnNewBtn := Self.Actions.Count - 1;

    vrAction      := TJupiterAction.Create('Editar', TJupiterRunnable.Create(''), nil);
    vrAction.Hint := 'Clique aqui para editar a linha atual';
    vrAction.Icon := ICON_EDIT;
    vrAction.OnClick := @Internal_EditClick;

    Self.Actions.Add(vrAction);

    Self.FBtnEditBtn := Self.Actions.Count - 1;

    vrAction      := TJupiterAction.Create('Excluir', TJupiterRunnable.Create(''), nil);
    vrAction.Hint := 'Clique aqui para excluir a linha atual';
    vrAction.Icon := ICON_DELETE;
    vrAction.OnClick := @Internal_DeleteClick;

    Self.Actions.Add(vrAction);

    Self.FBtnDelBtn := Self.Actions.Count - 1;
  end;

  if Self.Params.Exists('hint') then
    Self.Hint := Self.Params.VariableById('hint').Value;
end;

procedure TFExplorer.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  lvItems.Font.Size := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);
end;

procedure TFExplorer.Internal_UpdateDatasets;
var
  vrItem          : TListItem;
  vrVez           : Integer;
  vrVez2          : Integer;
  vrCreateColumns : Boolean;
  vrColumn        : TListColumn;
  vrEnviroment    : TJupiterEnviroment;
  vrCountChecked  : Integer;
begin
  inherited Internal_UpdateDatasets;

  lvItems.Items.Clear;
  lvItems.Visible := False;
  lvItems.DisableAutoSizing;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrCreateColumns := lvItems.ColumnCount = 0;

    Self.Provider.ClearRows;
    Self.FProvider.ProvideData;

    vrCountChecked := 0;

    for vrVez := 0 to lvItems.ColumnCount - 1 do
      lvItems.Column[vrVez].AutoSize := False;

    for vrVez := 0 to Self.Provider.Size - 1 do
      with Self.Provider.GetRowByIndex(vrVez) do
      begin
        if ((Trim(Self.SearchText) <> EmptyStr) and (not CanShowInSearch(Self.SearchText))) then
          Continue;

        vrItem := lvItems.Items.Add;

        for vrVez2 := 0 to Fields.Size - 1 do
        begin
          if Self.Internal_HideColumn(Fields.VariableByIndex(vrVez2).ID) then
            Continue;

          if ((vrCreateColumns) and (vrVez = 0)) then
          begin
            vrColumn            := lvItems.Columns.Add;
            vrColumn.Caption    := Fields.VariableByIndex(vrVez2).Title + COLUMN_SPACE_SEPARATOR;
          end;

          if vrVez2 = 0 then
            vrItem.Caption := vrJupiterApp.Params.ResolveString(Fields.VariableByIndex(vrVez2).Value + COLUMN_SPACE_SEPARATOR)
          else
            vrItem.SubItems.Add(vrJupiterApp.Params.ResolveString(Fields.VariableByIndex(vrVez2).Value + COLUMN_SPACE_SEPARATOR));

          if ((Self.CheckMode) or (Self.ChecklistMode)) then
            vrItem.ImageIndex := NULL_KEY
          else
            vrItem.ImageIndex := vrEnviroment.IconOfFile(TrimRight(vrItem.Caption));

          vrItem.Data := TJupiterVariableList.Create;

          TJupiterVariableList(vrItem.Data).CopyValues(Fields);

          if ((Self.CheckMode) and
              (vrJupiterApp.Params.Exists(Self.Params.VariableById('checkableVariable').Value)) and
              (Self.Internal_IfChecked(Fields.VariableById(Self.Params.VariableById('checkableField').Value).Value,
                                       vrJupiterApp.Params.VariableById(Self.Params.VariableById('checkableVariable').Value).Value)
                                       )) then
          begin
            vrItem.ImageIndex := ICON_CHECK;

            vrCountChecked := vrCountChecked + 1;
          end
          else
            if ((Self.ChecklistMode) and (Self.Internal_RecordChecked(Fields))) then
            begin
              vrItem.ImageIndex := ICON_CHECK;

              vrCountChecked := vrCountChecked + 1;
            end
            else
              if Self.Params.Exists('itemIcon') then
                vrItem.ImageIndex := StrToInt(Self.Params.VariableById('itemIcon').Value);
        end;
      end;

    if ((Self.Params.Exists('Hint.Success')) and (vrCountChecked <> Self.Provider.Size)) then
      Self.Params.DeleteVariable('Hint.Success')
    else
      if ((vrCountChecked > 0) and (vrCountChecked = Self.Provider.Size)) and (ChecklistMode) then
        Self.Params.AddVariable('Hint.Success', 'Hint.Success', 'Sucesso');

    for vrVez := 0 to lvItems.ColumnCount - 1 do
      lvItems.Column[vrVez].AutoSize := True;
  finally
    Self.Params.AddVariable('Size', IntToStr(lvItems.Items.Count), 'Qtd. de registros');

    lvItems.Visible := True;
    lvItems.EnableAutoSizing;
  end;
end;

procedure TFExplorer.Internal_MarcarTodosClick(Sender: TObject);
begin
  try
    Self.Internal_CheckBoxChangeAll(True);
  finally
    Self.UpdateForm;
  end;
end;

procedure TFExplorer.Internal_DesmarcarTodosClick(Sender: TObject);
begin
  try
    Self.Internal_CheckBoxChangeAll(False);
  finally
    Self.UpdateForm;
  end;
end;

procedure TFExplorer.Internal_NewClick(Sender: TObject);
begin
  Application.CreateForm(TFDynamicRecord, FDynamicRecord);
  try
    FDynamicRecord.IsModal := True;
    FDynamicRecord.CurrentRecord := TJupiterCSVDataProvider(Self.Provider).BlankLine;

    FDynamicRecord.ShowModal;
  finally
    FDynamicRecord.Release;
    FreeAndNil(FDynamicRecord);
  end;
end;

procedure TFExplorer.Internal_EditClick(Sender: TObject);
begin
  if not Assigned(lvItems.Selected) then
    Exit;

  if not Assigned(lvItems.Selected.Data) then
    Exit;

  Application.CreateForm(TFDynamicRecord, FDynamicRecord);
  try
    FDynamicRecord.IsModal := True;
    FDynamicRecord.CurrentRecord := TJupiterDataProviderRow.Create;
//    FDynamicRecord.CurrentRecord.Fields.CopyList(TJupiterVariableList(lvItems.Selected.Data));

    FDynamicRecord.ShowModal;
  finally
    FDynamicRecord.Release;
    FreeAndNil(FDynamicRecord);
  end;
end;

procedure TFExplorer.Internal_DeleteClick(Sender: TObject);
begin
  //
end;

procedure TFExplorer.Internal_CheckBoxChangeAll(prNewValue : Boolean);
var
  vrVez : INteger;
begin
  for vrVez := 0 to lvItems.Items.Count - 1 do
    with TJupiterVariableList(lvItems.Items[vrVez].Data) do
    begin
      if prNewValue then
        VariableById(Self.Params.VariableById('checklistField').Value).Value := '1'
      else
        VariableById(Self.Params.VariableById('checklistField').Value).Value := '0';

      TJupiterCSVDataProvider(Self.Provider).SaveLine(TJupiterVariableList(lvItems.Items[vrVez].Data));
    end;
end;

end.

