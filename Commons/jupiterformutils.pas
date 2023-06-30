unit jupiterformutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ComCtrls, JupiterRoute, JupiterObject, JupiterAction, JupiterConsts,
  SysUtils, Forms;

  procedure CopyNodes(prSourceNode, prTargetNode: TTreeNode);

  procedure ShowRouteOnTreeView(prTreeView : TTreeView; prRoute : TJupiterRoute; prList : TJupiterObjectList; prNode : TTreeNode);

  procedure SearchOnTreeView(prTreeView : TTreeView; prSearch : String);

  function PercentOfScreen(prTotalSize, prPercent : Integer) : Integer;

  procedure DrawForm(prComponent : TComponent);

implementation

uses JupiterApp, ExtCtrls, Menus, StdCtrls;

procedure CopyNodes(prSourceNode, prTargetNode: TTreeNode);

  procedure ClimbDownTree(ASrcNode, ATgtNode: TTreeNode);
  begin
    ATgtNode := ATgtNode.Owner.AddChild(ATgtNode, ASrcNode.Text);
    ASrcNode := ASrcNode.GetFirstChild;

    while Assigned(ASrcNode) do
    begin
      ClimbDownTree(ASrcNode, ATgtNode);

      ASrcNode := ASrcNode.GetNextSibling;
    end;
  end;

begin
  if Assigned(prSourceNode) and Assigned(prTargetNode) then
    ClimbDownTree(prSourceNode, prTargetNode);
end;

procedure ShowRouteOnTreeView(prTreeView : TTreeView; prRoute : TJupiterRoute; prList : TJupiterObjectList; prNode : TTreeNode);
var
  vrVez      : Integer;
  vrNode     : TTreeNode;
  vrAction   : TJupiterAction;
  vrTreeItem : TTreeNode;
begin
  for vrVez := 0 to prList.Size - 1 do
  begin
    vrAction := TJupiterAction(prList.GetAtIndex(vrVez));

    if not Assigned(vrAction.Location) then
      Continue;

    if vrAction.Location.Path <> prRoute.Path then
      Continue;

    if Assigned(prNode) then
      vrNode := prTreeView.Items.AddChild(prNode, vrAction.Title)
    else
      vrNode := prTreeView.Items.Add(nil, vrAction.Title);

    vrNode.ImageIndex := vrAction.Icon;
    vrNode.SelectedIndex := vrAction.Icon;
    vrNode.Data := vrAction;

    if Assigned(vrAction.Route) then
      ShowRouteOnTreeView(prTreeView, vrAction.Route, prList, vrNode);
  end;

  prTreeView.FullExpand;

  for vrVez := 0 to prTreeView.Items.Count - 1 do
  begin
    vrTreeItem := prTreeView.Items[vrVez];

    if vrTreeItem.Count = 0 then
      Continue;

    if not Assigned(vrTreeItem.Data) then
      Continue;

    with TJupiterAction(vrTreeItem.Data) do
      if Route.Params.Exists(FIELD_TREE_COLAPSE) then
        vrTreeItem.Collapse(False);
  end;
end;

procedure SearchOnTreeView(prTreeView: TTreeView; prSearch: String);
var
  vrVez : Integer;
  vrAux : String;
  vrDeleteList: TJupiterObjectList;
begin
  vrDeleteList := TJupiterObjectList.Create;
  try
    prTreeView.FullExpand;

    prSearch := Trim(AnsiUpperCase(prSearch));

    for vrVez := 0 to prTreeView.Items.Count -1 do
    begin
      vrAux := Trim(AnsiUpperCase(prTreeView.Items[vrVez].Text));

      if ((prTreeView.Items[vrVez].Count = 0) and (Pos(prSearch, vrAux) = 0)) then
        vrDeleteList.AddSimpleObject(prTreeView.Items[vrVez]);
    end;

    for vrVez := vrDeleteList.Count - 1 downto 0 do
      prTreeView.Items.Delete(TTreeNode(vrDeleteList.GetAtIndexAsObject(vrVez)));

    vrDeleteList.ClearListItens;
  finally
    FreeAndNil(vrDeleteList);
  end;
end;

function PercentOfScreen(prTotalSize, prPercent: Integer): Integer;
begin
  Result := Round((prPercent / 100) * prTotalSize);
end;

procedure DrawForm(prComponent: TComponent);
var
  vrVez : Integer;
begin
  for vrVez := 0 to prComponent.ComponentCount - 1 do
  begin
    if prComponent.Components[vrVez] is TLabel then
      TLabel(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

    if prComponent.Components[vrVez] is TEdit then
      TEdit(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

    if prComponent.Components[vrVez] is TComboBox then
      TComboBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

    if prComponent.Components[vrVez] is TCheckBox then
      TCheckBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

    if prComponent.Components[vrVez] is TScrollBox then
    begin
      TScrollBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

      DrawForm(prComponent.Components[vrVez]);
    end;

    if prComponent.Components[vrVez] is TPanel then
    begin
      TPanel(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

      DrawForm(prComponent.Components[vrVez]);
    end;

    if prComponent.Components[vrVez] is TGroupBox then
    begin
      TGroupBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

      DrawForm(prComponent.Components[vrVez]);
    end;
  end;
end;

end.

