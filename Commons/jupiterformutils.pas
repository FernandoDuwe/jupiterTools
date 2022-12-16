unit jupiterformutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ComCtrls, JupiterRoute, JupiterObject, JupiterAction, SysUtils;

  procedure CopyNodes(prSourceNode, prTargetNode: TTreeNode);

  procedure ShowRouteOnTreeView(prTreeView : TTreeView; prRoute : TJupiterRoute; prList : TJupiterObjectList; prNode : TTreeNode);

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
  vrVez    : Integer;
  vrNode   : TTreeNode;
  vrAction : TJupiterAction;
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
end;

end.

