unit uExplorer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Buttons,
  Grids, JupiterForm, JupiterAction, JupiterRunnable, JupiterDataProvider,
  JupiterConsts, JupiterApp, JupiterVariable;

type

  { TFExplorer }

  TFExplorer = class(TFJupiterForm)
    lvItems: TListView;
    procedure lvItemsDblClick(Sender: TObject);
  private
    FProvider  : TJupiterDataProvider;

    function Internal_IfChecked(prValue1, prValue2 : String) : Boolean;
    function InternalGetCheckMode : Boolean;

    procedure Internal_OnClick(prParams : TJupiterVariableList);
  published
    property CheckMode : Boolean read InternalGetCheckMode;

    property Provider : TJupiterDataProvider read FProvider write FProvider;
  protected
    procedure Internal_PrepareForm; override;

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

procedure TFExplorer.Internal_OnClick(prParams: TJupiterVariableList);
begin
  if Self.CheckMode then
  begin
    with vrJupiterApp.Params.VariableById(Self.Params.VariableById('checkableVariable').Value) do
    begin
      Value := prParams.VariableById(Self.Params.VariableById('checkableField').Value).Value;
      SaveConfig;
    end;
  end;
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

  {
  vrAction      := TJupiterAction.Create('Novo', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para inserir um novo registro';
  vrAction.Icon := ICON_NEW;

  Self.Actions.Add(vrAction);

  vrAction      := TJupiterAction.Create('Editar', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para editar o registro atual';
  vrAction.Icon := ICON_EDIT;

  Self.Actions.Add(vrAction);

  vrAction      := TJupiterAction.Create('Excluir', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para excluir o registro atual';
  vrAction.Icon := ICON_DELETE;

  Self.Actions.Add(vrAction);
  }

  vrAction      := TJupiterAction.Create('Atualizar', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para atualizar a página';
  vrAction.Icon := ICON_REFRESH;

  Self.Actions.Add(vrAction);
end;

procedure TFExplorer.Internal_UpdateDatasets;
var
  vrItem          : TListItem;
  vrVez           : Integer;
  vrVez2          : Integer;
  vrCreateColumns : Boolean;
  vrColumn        : TListColumn;
begin
  inherited Internal_UpdateDatasets;

  lvItems.Items.Clear;

  vrCreateColumns := lvItems.ColumnCount = 0;

  for vrVez := 0 to Self.Provider.Size - 1 do
    with Self.Provider.GetRowByIndex(vrVez) do
    begin
      vrItem := lvItems.Items.Add;

      for vrVez2 := 0 to Fields.Size - 1 do
      begin
        if ((vrCreateColumns) and (vrVez = 0)) then
        begin
          vrColumn            := lvItems.Columns.Add;
          vrColumn.Caption    := Fields.VariableByIndex(vrVez2).Title + COLUMN_SPACE_SEPARATOR;
          vrColumn.AutoSize   := True;
        end;

        if vrVez2 = 0 then
          vrItem.Caption := Fields.VariableByIndex(vrVez2).Value + COLUMN_SPACE_SEPARATOR
        else
          vrItem.SubItems.Add(Fields.VariableByIndex(vrVez2).Value + COLUMN_SPACE_SEPARATOR);

        vrItem.ImageIndex := NULL_KEY;
        vrItem.Data := TJupiterVariableList.Create;

        TJupiterVariableList(vrItem.Data).CopyValues(Fields);

        if ((Self.CheckMode) and
            (Self.Internal_IfChecked(Fields.VariableById(Self.Params.VariableById('checkableField').Value).Value,
                                     vrJupiterApp.Params.VariableById(Self.Params.VariableById('checkableVariable').Value).Value)
                                     )) then
          vrItem.ImageIndex := ICON_CHECK;
      end;
    end;
end;

end.

