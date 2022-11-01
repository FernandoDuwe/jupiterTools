unit uExplorer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Buttons,
  Grids, JupiterForm, JupiterAction, JupiterRunnable, JupiterDataProvider,
  JupiterConsts, JupiterApp, JupiterVariable, JupiterEnviroment,
  JupiterCSVDataProvider;

type

  { TFExplorer }

  TFExplorer = class(TFJupiterForm)
    lvItems: TListView;
    procedure lvItemsDblClick(Sender: TObject);
  private
    FProvider  : TJupiterDataProvider;

    function  Internal_IfChecked(prValue1, prValue2 : String) : Boolean;
    function  InternalGetCheckMode : Boolean;
    function  InternalGetCheckListMode : Boolean;
    function  Internal_RecordChecked(prParams : TJupiterVariableList) : Boolean;
    procedure Internal_ChangeValue(var prParams : TJupiterVariableList);
    function  InternalGetRunMode : Boolean;

    procedure Internal_OnClick(prParams : TJupiterVariableList);
    procedure Internal_RefreshClick(Sender: TObject);
    function  Internal_HideColumn(prColumnName : String) : Boolean;
  published
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

  Self.FProvider := FactoryDataProvider(vrProvider, vrParam);

  vrAction      := TJupiterAction.Create('Atualizar', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para atualizar a página';
  vrAction.Icon := ICON_REFRESH;

  Self.Actions.Add(vrAction);

  if Self.Params.Exists('hint') then
    Self.Hint := Self.Params.VariableById('hint').Value;
end;

procedure TFExplorer.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;
end;

procedure TFExplorer.Internal_UpdateDatasets;
var
  vrItem          : TListItem;
  vrVez           : Integer;
  vrVez2          : Integer;
  vrCreateColumns : Boolean;
  vrColumn        : TListColumn;
  vrEnviroment    : TJupiterEnviroment;
begin
  inherited Internal_UpdateDatasets;

  lvItems.Items.Clear;
  lvItems.DisableAutoSizing;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrCreateColumns := lvItems.ColumnCount = 0;

    for vrVez := 0 to Self.Provider.Size - 1 do
      with Self.Provider.GetRowByIndex(vrVez) do
      begin
        vrItem := lvItems.Items.Add;

        for vrVez2 := 0 to Fields.Size - 1 do
        begin
          if Self.Internal_HideColumn(Fields.VariableByIndex(vrVez2).ID) then
            Continue;

          if ((vrCreateColumns) and (vrVez = 0)) then
          begin
            vrColumn            := lvItems.Columns.Add;
            vrColumn.Caption    := Fields.VariableByIndex(vrVez2).Title + COLUMN_SPACE_SEPARATOR;
            vrColumn.AutoSize   := True;
          end;

          if vrVez2 = 0 then
            vrItem.Caption := vrJupiterApp.Params.ResolveString(Fields.VariableByIndex(vrVez2).Value + COLUMN_SPACE_SEPARATOR)
          else
            vrItem.SubItems.Add(vrJupiterApp.Params.ResolveString(Fields.VariableByIndex(vrVez2).Value + COLUMN_SPACE_SEPARATOR));

          if ((Self.CheckMode) or (Self.ChecklistMode)) then
            vrItem.ImageIndex := NULL_KEY
          else
            vrItem.ImageIndex := vrEnviroment.IconOfFile(vrItem.Caption);

          vrItem.Data := TJupiterVariableList.Create;

          TJupiterVariableList(vrItem.Data).CopyValues(Fields);

          if ((Self.CheckMode) and
              (vrJupiterApp.Params.Exists(Self.Params.VariableById('checkableVariable').Value)) and
              (Self.Internal_IfChecked(Fields.VariableById(Self.Params.VariableById('checkableField').Value).Value,
                                       vrJupiterApp.Params.VariableById(Self.Params.VariableById('checkableVariable').Value).Value)
                                       )) then
            vrItem.ImageIndex := ICON_CHECK
          else
            if ((Self.ChecklistMode) and (Self.Internal_RecordChecked(Fields))) then
              vrItem.ImageIndex := ICON_CHECK
            else
              if Self.Params.Exists('itemIcon') then
                vrItem.ImageIndex := StrToInt(Self.Params.VariableById('itemIcon').Value);
        end;
      end;
  finally
    Self.Params.AddVariable('Size', IntToStr(lvItems.Items.Count), 'Qtd. de registros');

    lvItems.EnableAutoSizing;
  end;
end;

end.

