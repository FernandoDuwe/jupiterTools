unit jupiterformutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ComCtrls, JupiterObject, JupiterConsts,
  SysUtils, Forms, Graphics, EditBtn, CheckLst, StdCtrls;

  procedure CopyNodes(prSourceNode, prTargetNode: TTreeNode);

  function PercentOfScreen(prTotalSize, prPercent : Integer) : Integer;

  procedure DrawForm(prComponent : TComponent);

  function GetEditByTag(prComponent : TComponent; prTag : Integer) : TEdit;

  procedure DrawFormInSearch(prComponent : TComponent; prSearch : String; prColor : TColor);

  function IsSameForm(prForm1, prForm2 : TForm; prIgnoreVariables : Array of String) : Boolean;

  function GetTextHeight(prText: String; prFont: TFont): Integer;
  function GetTextWidth(prText: String; prFont: TFont): Integer;
  function GetFontSize : Integer;

type

  { TJupiterPosition }

  TJupiterPosition = class(TJupiterObject)
  private
    FTop : Integer;
    FLeft : Integer;
  published
    property Top  : Integer read FTop  write FTop;
    property Left : Integer read FLeft write FLeft;
  public
    constructor Create(prTop, prLeft : Integer);
  end;

  { TJupiterComponentReference }

  TJupiterComponentReference = class(TJupiterPosition)
  private
    FComponent: TComponent;
    FRight : Integer;
    FBottom : Integer;
    FCompoent : TComponent;
  published
    property Right     : Integer    read FRight     write FRight;
    property Bottom    : Integer    read FBottom    write FBottom;
    property Component : TComponent read FComponent write FComponent;
  public
    constructor Create(prTop, prLeft, prRight, prBottom : Integer; prComponent : TComponent);
  end;

implementation

uses JupiterApp, ExtCtrls, Menus, JupiterVariable, DBGrids;

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

function PercentOfScreen(prTotalSize, prPercent: Integer): Integer;
begin
  Result := Round((prPercent / 100) * prTotalSize);
end;

procedure DrawForm(prComponent: TComponent);
var
  vrVez : Integer;
begin
  if not Assigned(vrJupiterApp) then
    Exit;

  if not vrJupiterApp.Params.Exists(FIELD_FONT_SIZE) then
    Exit;

  if prComponent is TForm then
    TForm(prComponent).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

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

    if prComponent.Components[vrVez] is TListBox then
      TListBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

    if prComponent.Components[vrVez] is TMemo then
      TMemo(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

    if prComponent.Components[vrVez] is TStatusBar then
      TStatusBar(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

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

    if prComponent.Components[vrVez] is TDBGrid then
    begin
      TDBGrid(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

      TDBGrid(prComponent.Components[vrVez]).AlternateColor := $00FFEAEA;

      {$IFDEF WINDOWS}
      TDBGrid(prComponent.Components[vrVez]).AlternateColor := $00FFEAEA;
      {$ENDIF}
    end;
  end;
end;

function GetEditByTag(prComponent: TComponent; prTag: Integer): TEdit;
var
  vrVez : Integer;
begin
  Result := nil;

  for vrVez := 0 to prComponent.ComponentCount - 1 do
  begin
    if prComponent.Components[vrVez] is TScrollBox then
      Result := GetEditByTag(prComponent.Components[vrVez], prTag);

    if prComponent.Components[vrVez] is TPanel then
      Result := GetEditByTag(prComponent.Components[vrVez], prTag);

    if prComponent.Components[vrVez] is TGroupBox then
      Result := GetEditByTag(prComponent.Components[vrVez], prTag);

    if Result <> nil then
      Exit;
  end;

  for vrVez := 0 to prComponent.ComponentCount - 1 do
  begin
    if prComponent.Components[vrVez] is TEdit then
      if TEdit(prComponent.Components[vrVez]).Tag = prTag then
      begin
        Result := TEdit(prComponent.Components[vrVez]);
        Exit;
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

    {
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
          }
    Result := True;
  finally
    FreeAndNil(vrParams1);
    FreeAndNil(vrParams2);
  end;
end;

function GetTextHeight(prText: String; prFont: TFont): Integer;
var
  vrBMP : TBitmap;
begin
  Result := 0;

  vrBMP := TBitmap.Create;
  try
    vrBMP.Canvas.Font.Assign(prFont);
    Result := vrBMP.Canvas.TextHeight(prText);
  finally
    vrBMP.Free;
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

function GetFontSize: Integer;
begin
  if not vrJupiterApp.Params.Exists(FIELD_FONT_SIZE) then
    Result := 9;

  Result := vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).AsInteger;
end;

{ TJupiterPosition }

constructor TJupiterPosition.Create(prTop, prLeft: Integer);
begin
  Self.Top  := prTop;
  Self.Left := prLeft;
end;

{ TJupiterComponentReference }

constructor TJupiterComponentReference.Create(prTop, prLeft, prRight, prBottom: Integer; prComponent: TComponent);
begin
  Self.Top       := prTop;
  Self.Left      := prLeft;
  Self.Right     := prRight;
  Self.Bottom    := prBottom;
  Self.Component := prComponent;
end;

end.

