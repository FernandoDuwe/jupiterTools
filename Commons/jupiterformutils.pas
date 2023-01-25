unit jupiterformutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ComCtrls, JupiterRoute, JupiterObject, JupiterAction, JupiterConsts,
  SysUtils;

  procedure CopyNodes(prSourceNode, prTargetNode: TTreeNode);

  procedure ShowRouteOnTreeView(prTreeView : TTreeView; prRoute : TJupiterRoute; prList : TJupiterObjectList; prNode : TTreeNode);

  function PercentOfScreen(prTotalSize, prPercent : Integer) : Integer;

implementation

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

function PercentOfScreen(prTotalSize, prPercent: Integer): Integer;
begin
  Result := Round((prPercent / 100) * prTotalSize);
end;

end.

