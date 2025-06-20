unit uMultiLevelTextEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ActnList,
  StdCtrls, ExtCtrls, uJupiterForm, JupiterConsts, JupiterObject,
  jupiterStringUtils, uJupiterAction, uMain, Clipbrd;

type

  { TFMultiLevelTextEditor }

  TFMultiLevelTextEditor = class(TFJupiterForm)
    acNewLine: TAction;
    acNewChildLine: TAction;
    acDelete: TAction;
    acCtrlC: TAction;
    tvText: TTreeView;
    procedure acCtrlCExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acNewChildLineExecute(Sender: TObject);
    procedure acNewLineExecute(Sender: TObject);
    procedure tvTextChange(Sender: TObject; Node: TTreeNode);
    procedure tvTextDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvTextDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure tvTextStartDrag(Sender: TObject; var DragObject: TDragObject);
  private
    FCurrentOwner : TTreeNode;
    FDragNode : TTreeNode;

    function Internal_GetNodeById(prNodeId : Integer) : TTreeNode;

    procedure Internal_BuildTree;

    procedure Internal_PrepareForm; override;

    procedure Internal_OnSave(Sender: TObject);

    procedure Internal_OnAumentarFonte(Sender: TObject);
    procedure Internal_OnDiminuirFonte(Sender: TObject);

    procedure CopyNode(SourceNode, TargetNode: TTreeNode; TreeView: TTreeView);
  public

  end;

var
  tvText: TFMultiLevelTextEditor;

implementation

{$R *.lfm}

{ TFMultiLevelTextEditor }

procedure TFMultiLevelTextEditor.acNewLineExecute(Sender: TObject);
begin
  if Assigned(Self.FCurrentOwner) then
    tvText.Selected := tvText.Items.AddChild(Self.FCurrentOwner, 'Nova linha')
  else
    tvText.Selected := tvText.Items.Add(nil, 'Nova linha');
end;

procedure TFMultiLevelTextEditor.acNewChildLineExecute(Sender: TObject);
begin
  if Assigned(tvText.Selected) then
    tvText.Selected := tvText.Items.AddChild(tvText.Selected, 'Nova linha')
  else
    tvText.Selected := tvText.Items.Add(nil, 'Nova linha');
end;

procedure TFMultiLevelTextEditor.acDeleteExecute(Sender: TObject);
var
  vrCurrent : TTreeNode;
begin
  if not Assigned(tvText.Selected) then
    Exit;

  vrCurrent := tvText.Selected;

  if Assigned(tvText.Selected.Parent) then
    tvText.Selected := vrCurrent.Parent
  else
    if tvText.Items.Count > 1 then
    begin
      tvText.Items.Delete(vrCurrent);

      tvText.Selected := tvText.Items[tvText.Items.Count - 1];
      Exit;
    end;

  tvText.Items.Delete(vrCurrent);
end;

procedure TFMultiLevelTextEditor.acCtrlCExecute(Sender: TObject);
begin
  if not Assigned(tvText.Selected) then
    Exit;

  Clipboard.AsText := tvText.Selected.Text;
end;

procedure TFMultiLevelTextEditor.tvTextChange(Sender: TObject; Node: TTreeNode);
begin
  Self.FCurrentOwner := nil;

  if not Assigned(Node) then
    Exit;

  if not Assigned(Node.Parent) then
    Exit;

  Self.FCurrentOwner := Node.Parent;
end;

procedure TFMultiLevelTextEditor.tvTextDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  vrTargetNode : TTreeNode;
begin
  vrTargetNode := tvText.GetNodeAt(X, Y);

  if (Self.FDragNode = nil) or (vrTargetNode = nil) then
    Exit;

  if vrTargetNode.HasAsParent(Self.FDragNode) then
    Exit;

  Self.FDragNode.MoveTo(vrTargetNode, naAddChild);

  vrTargetNode.Expand(False);

  Self.FDragNode := nil;
end;

procedure TFMultiLevelTextEditor.tvTextDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  vrNode : TTreeNode;
begin
  Accept := Source = tvText;

  vrNode := tvText.GetNodeAt(X, Y);

  if Assigned(vrNode) then
  begin
    tvText.Selected := vrNode;

    if not Assigned(Self.FDragNode) then
      Self.FDragNode := vrNode;
  end;
end;

procedure TFMultiLevelTextEditor.tvTextStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  Self.FDragNode := tvText.Selected;
end;

function TFMultiLevelTextEditor.Internal_GetNodeById(prNodeId: Integer): TTreeNode;
var
  vrVez : Integer;
begin
  Result := nil;

  for vrVez := 0 to tvText.Items.Count - 1 do
  begin
    if not Assigned(tvText.Items[vrVez].Data) then
      Continue;

    if TJupiterObject(tvText.Items[vrVez].Data).Tag = prNodeId then
    begin
      Result := tvText.Items[vrVez];
      Exit;
    end;
  end;
end;

procedure TFMultiLevelTextEditor.Internal_BuildTree;
var
  vrStr  : TStringList;
  vrVez  : Integer;
  vrNode : TTreeNode;
begin
  tvText.Items.Clear;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.LoadFromFile(Self.Params.VariableById('path').Value);

    for vrVez := 1 to vrStr.Count - 1 do
    begin
      if Trim(vrStr[vrVez]) = EmptyStr then
        Continue;

      if JupiterStringUtilsGetCSVColumn(vrStr[vrVez], 1) = '' then
        vrNode := tvText.Items.Add(nil, JupiterStringUtilsGetCSVColumn(vrStr[vrVez], 2))
      else
        vrNode := tvText.Items.AddChild(Self.Internal_GetNodeById(StrToInt(JupiterStringUtilsGetCSVColumn(vrStr[vrVez], 1))), JupiterStringUtilsGetCSVColumn(vrStr[vrVez], 2));

      vrNode.Data := TJupiterObject.Create;
      TJupiterObject(vrNode.Data).Tag := StrToInt(JupiterStringUtilsGetCSVColumn(vrStr[vrVez], 0));

      if JupiterStringUtilsGetCSVColumn(vrStr[vrVez], 3) <> EmptyStr then
      begin
        vrNode.ImageIndex := StrToInt(JupiterStringUtilsGetCSVColumn(vrStr[vrVez], 3));
        vrNode.SelectedIndex := StrToInt(JupiterStringUtilsGetCSVColumn(vrStr[vrVez], 3));
      end;
    end;
  finally
    FreeAndNil(vrStr);
  end;
end;

procedure TFMultiLevelTextEditor.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  tvText.Images := FMain.ilIconFamily;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Salvar', 'Clique aqui para abrir salvar o arquivo', ICON_SAVE, @Internal_OnSave));
  Self.ActionGroup.AddAction(TJupiterAction.Create('Aumentar fonte', 'Clique aqui para aumentar a fonte', ICON_CURTASK, @Internal_OnAumentarFonte));
  Self.ActionGroup.AddAction(TJupiterAction.Create('Diminuir fonte', 'Clique aqui para diminuir a fonte', ICON_CURTASK, @Internal_OnDiminuirFonte));

  Self.Hint := 'Para pular linhas, pressione Enter. Para criar um subitem, pressione Ctrl + Enter. Para apagar uma linha, pressione delete';

  Self.Internal_BuildTree;

  if tvText.Items.Count = 0 then
    acNewLineExecute(Self);
end;

procedure TFMultiLevelTextEditor.Internal_OnSave(Sender: TObject);
var
  vrStr  : TStrings;
  vrVez  : Integer;
  vrLine : String;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add('id;owner;text;icon;');

    // Atribuindo Ids
    for vrVez := 0 to tvText.Items.Count - 1 do
    begin
      tvText.Items[vrVez].Data := TJupiterObject.Create;
      TJupiterObject(tvText.Items[vrVez].Data).Tag := vrVez;
    end;

    // Adicionando ao arquivo
    for vrVez := 0 to tvText.Items.Count - 1 do
    begin
      vrLine := EmptyStr;

      vrLine := IntToStr(TJupiterObject(tvText.Items[vrVez].Data).Tag) + ';';

      if Assigned(tvText.Items[vrVez].Parent) then
        vrLine := vrLine + IntToStr(TJupiterObject(tvText.Items[vrVez].Parent.Data).Tag) + ';'
      else
        vrLine := vrLine + ';';

      if tvText.Items[vrVez].ImageIndex = NULL_KEY then
        vrLine := vrLine + tvText.Items[vrVez].Text + ';;'
      else
        vrLine := vrLine + tvText.Items[vrVez].Text + ';' + IntToStr(tvText.Items[vrVez].ImageIndex) + ';';

      vrStr.Add(vrLine);
    end;

    vrStr.SaveToFile(Self.Params.VariableById('path').Value);
  finally
    FreeAndNil(vrStr);
  end;
end;

procedure TFMultiLevelTextEditor.Internal_OnAumentarFonte(Sender: TObject);
begin
  tvText.Font.Size := tvText.Font.Size + 1;
end;

procedure TFMultiLevelTextEditor.Internal_OnDiminuirFonte(Sender: TObject);
begin
  tvText.Font.Size := tvText.Font.Size - 1;
end;

procedure TFMultiLevelTextEditor.CopyNode(SourceNode, TargetNode: TTreeNode; TreeView: TTreeView);
var
  NewNode, ChildNode: TTreeNode;
begin
  NewNode := TreeView.Items.AddChild(TargetNode, SourceNode.Text);
  NewNode.Data := SourceNode.Data; // Se estiver usando Data

  ChildNode := SourceNode.GetFirstChild;

  while ChildNode <> nil do
  begin
    CopyNode(ChildNode, NewNode, TreeView);
    ChildNode := ChildNode.GetNextSibling;
  end;
end;

end.

