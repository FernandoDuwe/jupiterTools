unit uCheckList;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CheckLst, ActnList,
  uJupiterForm, jupiterStringUtils, JupiterConsts, uJupiterAction, Clipbrd;

type

  { TFCheckList }

  TFCheckList = class(TFJupiterForm)
    acCopyCurrentItem: TAction;
    cbList: TCheckListBox;
    procedure acCopyCurrentItemExecute(Sender: TObject);
    procedure cbListClickCheck(Sender: TObject);
  private
     procedure Internal_PrepareForm; override;
     procedure Internal_UpdateDatasets; override;
     procedure Internal_UpdateCalcs; override;
     procedure Internal_UpdateComponents; override;

     procedure Internal_OnMarkAllClick(Sender: TObject);
     procedure Internal_OnUnMarkAllClick(Sender: TObject);
     procedure Internal_OnAumentarFonte(Sender: TObject);
     procedure Internal_OnDiminuirFonte(Sender: TObject);
  public

  end;

var
  FCheckList: TFCheckList;

implementation

{$R *.lfm}

{ TFCheckList }

procedure TFCheckList.cbListClickCheck(Sender: TObject);
var
  vrStr : TStrings;
  vrVez : Integer;
begin
  inherited Internal_UpdateDatasets;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add('Item;Checked;');

    for vrVez := 0 to cbList.Count - 1 do
      vrStr.Add(cbList.Items[vrVez] + ';' + JupiterStringUtilsBoolToStr(cbList.Checked[vrVez]) + ';');

    vrStr.SaveToFile(Self.Params.VariableById('path').Value);

    Self.UpdateForm(False);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure TFCheckList.acCopyCurrentItemExecute(Sender: TObject);
begin
  if cbList.ItemIndex = NULL_KEY then
    Exit;

  if cbList.Count = 0 then
    Exit;

  Clipboard.AsText := cbList.Items[cbList.ItemIndex];
end;

procedure TFCheckList.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Aumentar fonte', 'Clique aqui para aumentar a fonte', ICON_CURTASK, @Internal_OnAumentarFonte));
  Self.ActionGroup.AddAction(TJupiterAction.Create('Diminuir fonte', 'Clique aqui para diminuir a fonte', ICON_CURTASK, @Internal_OnDiminuirFonte));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Marcar todos', 'Clique aqui para criar marcar todas as caixas', ICON_CHECK, @Internal_OnMarkAllClick));
  Self.ActionGroup.AddAction(TJupiterAction.Create('Desmarcar todos', 'Clique aqui para criar desmarcar todas as caixas', NULL_KEY, @Internal_OnUnMarkAllClick));

  Self.Caption := ExtractFileName(Self.Params.VariableById('path').Value);
end;

procedure TFCheckList.Internal_UpdateDatasets;
var
  vrStr : TStrings;
  vrVez : Integer;
begin
  inherited Internal_UpdateDatasets;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.LoadFromFile(Self.Params.VariableById('path').Value);

    cbList.Items.Clear;

    for vrVez := 1 to vrStr.Count - 1 do
      cbList.Items.Add(JupiterStringUtilsGetCSVColumn(vrStr[vrVez], 0));

    for vrVez := 1 to vrStr.Count - 1 do
      cbList.Checked[vrVez - 1] := JupiterStringUtilsStrToBool(JupiterStringUtilsGetCSVColumn(vrStr[vrVez], 1));
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure TFCheckList.Internal_UpdateCalcs;
begin
  inherited Internal_UpdateCalcs;
end;

procedure TFCheckList.Internal_UpdateComponents;
var
  vrBool : Boolean;
  vrVez : Integer;
begin
  vrBool := True;

  for vrVez := 0 to cbList.Items.Count - 1 do
    if not cbList.Checked[vrVez] then
      vrBool := False;

  if not vrBool then
    Self.Hint := 'Marque as caixas acima para completar o checklist'
  else
    Self.Hint := 'Checklist completo';

  inherited Internal_UpdateComponents;
end;

procedure TFCheckList.Internal_OnMarkAllClick(Sender: TObject);
var
  vrVez : Integer;
begin
  for vrVez := 0 to cbList.Count - 1 do
    cbList.Checked[vrVez] := True;

  cbListClickCheck(Sender);
end;

procedure TFCheckList.Internal_OnUnMarkAllClick(Sender: TObject);
var
  vrVez : Integer;
begin
  for vrVez := 0 to cbList.Count - 1 do
    cbList.Checked[vrVez] := False;

  cbListClickCheck(Sender);
end;

procedure TFCheckList.Internal_OnAumentarFonte(Sender: TObject);
begin
  cbList.Font.Size := cbList.Font.Size + 1;
end;

procedure TFCheckList.Internal_OnDiminuirFonte(Sender: TObject);
begin
  cbList.Font.Size := cbList.Font.Size - 1;
end;

end.

