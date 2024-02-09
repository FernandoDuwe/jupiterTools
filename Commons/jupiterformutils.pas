unit jupiterformutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ComCtrls, JupiterRoute, JupiterObject, JupiterConsts,
  SysUtils, Forms, Graphics, EditBtn, CheckLst;

  procedure CopyNodes(prSourceNode, prTargetNode: TTreeNode);

  procedure ShowRouteOnTreeView(prTreeView : TTreeView; prRoute : TJupiterRoute; prList : TJupiterObjectList; prNode : TTreeNode);

  procedure SearchOnTreeView(prTreeView : TTreeView; prSearch : String);

  function PercentOfScreen(prTotalSize, prPercent : Integer) : Integer;

  procedure DrawForm(prComponent : TComponent);

  procedure DrawFormInSearch(prComponent : TComponent; prSearch : String; prColor : TColor);

  function IsSameForm(prForm1, prForm2 : TForm; prIgnoreVariables : Array of String) : Boolean;

  function GetTextWidth(prText: String; prFont: TFont): Integer;

implementation

uses JupiterApp, ExtCtrls, Menus, StdCtrls, JupiterForm, JupiterVariable, JupiterAction;

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
  vrOldSortType : TSortType;
begin
  vrOldSortType := prTreeView.SortType;

  prTreeView.SortType := stNone;

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
  vrOldSortType : TSortType;
begin
  vrOldSortType := prTreeView.SortType;

  prTreeView.SortType := stNone;

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

    if prTreeView.Items.Count > 0 then
      prTreeView.Selected := prTreeView.Items[0];

    for vrVez := 0 to prTreeView.Items.Count -1 do
    begin
      if AnsiUpperCase(Trim(prTreeView.Items[vrVez].Text)) = AnsiUpperCase(Trim(prSearch)) then
      begin
        prTreeView.Selected := prTreeView.Items[vrVez];

        Exit;
      end;

      if Copy(AnsiUpperCase(Trim(prTreeView.Items[vrVez].Text)), 1, Length(AnsiUpperCase(Trim(prSearch)))) = AnsiUpperCase(Trim(prSearch)) then
        prTreeView.Selected := prTreeView.Items[vrVez];
    end;
  finally
    prTreeView.SortType := vrOldSortType;

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

    if prComponent.Components[vrVez] is TDateEdit then
      TDateEdit(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

    if prComponent.Components[vrVez] is TComboBox then
      TComboBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

    if prComponent.Components[vrVez] is TCheckBox then
      TCheckBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

    if prComponent.Components[vrVez] is TListView then
      TListView(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

    if prComponent.Components[vrVez] is TCheckListBox then
      TCheckListBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

    if prComponent.Components[vrVez] is TTreeView then
      TTreeView(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

    if prComponent.Components[vrVez] is TMemo then
      TMemo(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

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

procedure DrawFormInSearch(prComponent: TComponent; prSearch: String; prColor : TColor);
var
  vrVez : Integer;
begin
  for vrVez := 0 to prComponent.ComponentCount - 1 do
  begin
    if prComponent.Components[vrVez] is TLabel then
    begin
      TLabel(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

      if SearchIsPartOf(TLabel(prComponent.Components[vrVez]).Caption, prSearch) then
        TLabel(prComponent.Components[vrVez]).Font.Color := prColor;
    end;

    if prComponent.Components[vrVez] is TEdit then
    begin
      TEdit(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

      if SearchIsPartOf(TEdit(prComponent.Components[vrVez]).Text, prSearch) then
        TEdit(prComponent.Components[vrVez]).Font.Color := prColor;
    end;

    if prComponent.Components[vrVez] is TComboBox then
    begin
      TComboBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

      if SearchIsPartOf(TComboBox(prComponent.Components[vrVez]).Text, prSearch) then
        TComboBox(prComponent.Components[vrVez]).Font.Color := prColor;
    end;

    if prComponent.Components[vrVez] is TCheckBox then
    begin
      TCheckBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

      if SearchIsPartOf(TCheckBox(prComponent.Components[vrVez]).Caption, prSearch) then
        TCheckBox(prComponent.Components[vrVez]).Font.Color := prColor;
    end;

    if prComponent.Components[vrVez] is TScrollBox then
      DrawFormInSearch(prComponent.Components[vrVez], prSearch, prColor);

    if prComponent.Components[vrVez] is TPanel then
      DrawFormInSearch(prComponent.Components[vrVez], prSearch, prColor);

    if prComponent.Components[vrVez] is TGroupBox then
      DrawFormInSearch(prComponent.Components[vrVez], prSearch, prColor);
  end;
end;

function IsSameForm(prForm1, prForm2: TForm; prIgnoreVariables : Array of String): Boolean;
var
  vrParams1 : TJupiterVariableList;
  vrParams2 : TJupiterVariableList;
  vrVez     : Integer;
begin
  Result := False;

  vrParams1 := TJupiterVariableList.Create;
  vrParams2 := TJupiterVariableList.Create;
  try
    if prForm1.ClassName <> prForm2.ClassName then
      Exit;

    if prForm1 is TFJupiterForm then
    begin
      vrParams1.CopyValues(TFJupiterForm(prForm1).Params);
      vrParams2.CopyValues(TFJupiterForm(prForm2).Params);

      for vrVez := 0 to Length(prIgnoreVariables) -1 do
      begin
        if vrParams1.Exists(prIgnoreVariables[vrVez]) then
          vrParams1.DeleteVariable(prIgnoreVariables[vrVez]);

        if vrParams2.Exists(prIgnoreVariables[vrVez]) then
          vrParams2.DeleteVariable(prIgnoreVariables[vrVez]);
      end;

      if not vrParams1.IsSame(vrParams2) then
        Exit;
    end;

    Result := True;
  finally
    FreeAndNil(vrParams1);
    FreeAndNil(vrParams2);
  end;
end;

function GetTextWidth(prText: String; prFont: TFont): Integer;
var
  vrBMP : TBitmap;
begin
  Result := 0;

  vrBMP := TBitmap.Create;
  try
    vrBMP.Canvas.Font.Assign(prFont);
    Result := vrBMP.Canvas.TextWidth(prText);
  finally
    vrBMP.Free;
  end;
end;

end.

