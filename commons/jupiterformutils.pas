unit jupiterformutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ComCtrls, JupiterObject, JupiterConsts, Controls,
  SysUtils, Forms, Graphics, EditBtn, CheckLst, StdCtrls,
  ShellCtrls, SynEdit, DBCtrls, DBDateTimePicker;

  procedure CopyNodes(prSourceNode, prTargetNode: TTreeNode);

  function PercentOfScreen(prTotalSize, prPercent : Integer) : Integer;

  procedure DrawForm(prComponent : TComponent; prHighContrast : Boolean = False);

  function GetEditByTag(prComponent : TComponent; prTag : Integer) : TEdit;

  procedure DrawFormInSearch(prComponent : TComponent; prSearch : String; prColor : TColor);

  procedure RemoveChildren(prComponent : TComponent);

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
    FWinControl : TWinControl;
    FRight : Integer;
    FBottom : Integer;
    FCompoent : TComponent;
    FFieldName : String;
    FMacroID : String;
    FMacroScript : String;
    FParam : String;
  published
    property Right       : Integer     read FRight       write FRight;
    property Bottom      : Integer     read FBottom      write FBottom;
    property Component   : TComponent  read FComponent   write FComponent;
    property FieldName   : String      read FFieldName   write FFieldName;
    property MacroID     : String      read FMacroID     write FMacroID;
    property MacroScript : String      read FMacroScript write FMacroScript;
    property Param       : String      read FParam       write FParam;
    property WinControl  : TWinControl read FWinControl  write FWinControl;
  public
    function RightCalc : Integer;

    constructor Create(prTop, prLeft, prRight, prBottom : Integer; prComponent : TComponent);

    constructor Create(prTop, prLeft, prRight, prBottom : Integer; prComponent : TComponent; prFieldName : String);

    constructor Create(prTop, prLeft, prRight, prBottom : Integer; prComponent : TComponent; prWinControl : TWinControl);
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

procedure DrawForm(prComponent: TComponent; prHighContrast: Boolean);
var
  vrVez : Integer;
begin
  if not Assigned(vrJupiterApp) then
    Exit;

  if not vrJupiterApp.Params.Exists(FIELD_FONT_SIZE) then
    Exit;

  if prComponent is TForm then
  begin
    TForm(prComponent).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
    TForm(prComponent).Font.Name := vrJupiterApp.Params.VariableById(FIELD_FONT_NAME).Value;
  end;

  for vrVez := 0 to prComponent.ComponentCount - 1 do
  begin
    if prComponent.Components[vrVez] is TLabel then
    begin
      TLabel(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TLabel(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_FONT_NAME).Value;

      if prHighContrast then
      begin
        TLabel(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TLabel(prComponent.Components[vrVez]).ParentColor := False;
        TLabel(prComponent.Components[vrVez]).ParentFont := False;

        if Assigned(TLabel(prComponent.Components[vrVez]).OnClick) then
          TLabel(prComponent.Components[vrVez]).Font.Color := clAqua
        else
        begin
          if TLabel(prComponent.Components[vrVez]).Font.Style = [fsBold] then
            TLabel(prComponent.Components[vrVez]).Font.Color := clYellow
          else
            TLabel(prComponent.Components[vrVez]).Font.Color := clWhite;
        end;
      end;
    end;

    if prComponent.Components[vrVez] is TEdit then
    begin
      TEdit(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TEdit(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_EDITABLES_FONT_NAME).Value;

      if prHighContrast then
      begin
        TEdit(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TEdit(prComponent.Components[vrVez]).ParentColor := False;
        TEdit(prComponent.Components[vrVez]).ParentFont := False;

        TEdit(prComponent.Components[vrVez]).Color := $002E2E2E;
        TEdit(prComponent.Components[vrVez]).Font.Color := clWhite;
      end;
    end;

    if prComponent.Components[vrVez] is TDBEdit then
    begin
      TDBEdit(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TDBEdit(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_EDITABLES_FONT_NAME).Value;

      if prHighContrast then
      begin
        TDBEdit(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TDBEdit(prComponent.Components[vrVez]).ParentColor := False;
        TDBEdit(prComponent.Components[vrVez]).ParentFont := False;

        TDBEdit(prComponent.Components[vrVez]).Color := $002E2E2E;
        TDBEdit(prComponent.Components[vrVez]).Font.Color := clWhite;
      end;
    end;

    if prComponent.Components[vrVez] is TDateEdit then
    begin
      TDateEdit(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TDateEdit(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_EDITABLES_FONT_NAME).Value;

      if prHighContrast then
      begin
        TDateEdit(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TDateEdit(prComponent.Components[vrVez]).ParentColor := False;
        TDateEdit(prComponent.Components[vrVez]).ParentFont := False;

        TDateEdit(prComponent.Components[vrVez]).Color := $002E2E2E;
        TDateEdit(prComponent.Components[vrVez]).Font.Color := clWhite;
      end;
    end;

    if prComponent.Components[vrVez] is TDBDateTimePicker then
    begin
      TDBDateTimePicker(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TDBDateTimePicker(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_EDITABLES_FONT_NAME).Value;

      if prHighContrast then
      begin
        TDBDateTimePicker(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TDBDateTimePicker(prComponent.Components[vrVez]).ParentColor := False;
        TDBDateTimePicker(prComponent.Components[vrVez]).ParentFont := False;

        TDBDateTimePicker(prComponent.Components[vrVez]).Color := $002E2E2E;
        TDBDateTimePicker(prComponent.Components[vrVez]).Font.Color := clWhite;
      end;
    end;

    if prComponent.Components[vrVez] is TComboBox then
    begin
      TComboBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TComboBox(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_EDITABLES_FONT_NAME).Value;

      if prHighContrast then
      begin
        TComboBox(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TComboBox(prComponent.Components[vrVez]).ParentColor := False;
        TComboBox(prComponent.Components[vrVez]).ParentFont := False;

        TComboBox(prComponent.Components[vrVez]).Color := $002E2E2E;
        TComboBox(prComponent.Components[vrVez]).Font.Color := clWhite;
      end;
    end;

    if prComponent.Components[vrVez] is TDBComboBox then
    begin
      TDBComboBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TDBComboBox(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_EDITABLES_FONT_NAME).Value;

      if prHighContrast then
      begin
        TDBComboBox(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TDBComboBox(prComponent.Components[vrVez]).ParentColor := False;
        TDBComboBox(prComponent.Components[vrVez]).ParentFont := False;

        TDBComboBox(prComponent.Components[vrVez]).Color := $002E2E2E;
        TDBComboBox(prComponent.Components[vrVez]).Font.Color := clWhite;
      end;
    end;

    if prComponent.Components[vrVez] is TDBLookupComboBox then
    begin
      TDBLookupComboBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TDBLookupComboBox(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_EDITABLES_FONT_NAME).Value;

      if prHighContrast then
      begin
        TDBLookupComboBox(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TDBLookupComboBox(prComponent.Components[vrVez]).ParentColor := False;
        TDBLookupComboBox(prComponent.Components[vrVez]).ParentFont := False;

        TDBLookupComboBox(prComponent.Components[vrVez]).Color := $002E2E2E;
        TDBLookupComboBox(prComponent.Components[vrVez]).Font.Color := clWhite;
      end;
    end;

    if prComponent.Components[vrVez] is TCheckBox then
    begin
      TCheckBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TCheckBox(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_EDITABLES_FONT_NAME).Value;

      if prHighContrast then
      begin
        TCheckBox(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TCheckBox(prComponent.Components[vrVez]).ParentColor := False;
        TCheckBox(prComponent.Components[vrVez]).ParentFont := False;

        TCheckBox(prComponent.Components[vrVez]).Font.Color := clWhite;
      end;
    end;

    if prComponent.Components[vrVez] is TDBCheckBox then
    begin
      TDBCheckBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TDBCheckBox(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_EDITABLES_FONT_NAME).Value;

      if prHighContrast then
      begin
        TDBCheckBox(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TDBCheckBox(prComponent.Components[vrVez]).ParentColor := False;
        TDBCheckBox(prComponent.Components[vrVez]).ParentFont := False;

        TDBCheckBox(prComponent.Components[vrVez]).Font.Color := clWhite;
      end;
    end;

    if prComponent.Components[vrVez] is TListView then
    begin
      TListView(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TListView(prComponent.Components[vrVez]).GridLines := True;
      TListView(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_FONT_NAME).Value;
    end;

    if prComponent.Components[vrVez] is TShellListView then
    begin
      TShellListView(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TShellListView(prComponent.Components[vrVez]).GridLines := True;
      TShellListView(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_FONT_NAME).Value;
    end;

    if prComponent.Components[vrVez] is TCheckListBox then
    begin
      TCheckListBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TCheckListBox(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_FONT_NAME).Value;
    end;

    if prComponent.Components[vrVez] is TTreeView then
    begin
      TTreeView(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TTreeView(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_FONT_NAME).Value;
    end;

    if prComponent.Components[vrVez] is TListBox then
    begin
      TListBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TListBox(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_FONT_NAME).Value;
    end;

    if prComponent.Components[vrVez] is TMemo then
    begin
      TMemo(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TMemo(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_MEMO_FONT_NAME).Value;
    end;

    if prComponent.Components[vrVez] is TDBMemo then
    begin
      TDBMemo(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TDBMemo(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_MEMO_FONT_NAME).Value;
    end;

    if prComponent.Components[vrVez] is TSynEdit then
    begin
      TSynEdit(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TSynEdit(prComponent.Components[vrVez]).RightEdge := vrJupiterApp.Params.VariableById(FORM_EDITOR_RIGHTEDGE).AsInteger;
      TSynEdit(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_SYNEDIT_FONT_NAME).Value;
    end;

    if prComponent.Components[vrVez] is TStatusBar then
    begin
      TStatusBar(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TStatusBar(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_FONT_NAME).Value;
    end;

    if prComponent.Components[vrVez] is TScrollBox then
    begin
      TScrollBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TScrollBox(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_FONT_NAME).Value;

      if prHighContrast then
      begin
        TScrollBox(prComponent.Components[vrVez]).BorderStyle := bsSingle;
        TScrollBox(prComponent.Components[vrVez]).ParentBackground := False;
        TScrollBox(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TScrollBox(prComponent.Components[vrVez]).ParentColor := False;
        TScrollBox(prComponent.Components[vrVez]).ParentFont := False;
        TScrollBox(prComponent.Components[vrVez]).Color := $002E2E2E;
      end;

      DrawForm(prComponent.Components[vrVez], prHighContrast);
    end;

    if prComponent.Components[vrVez] is TPanel then
    begin
      TPanel(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TPanel(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_FONT_NAME).Value;

      DrawForm(prComponent.Components[vrVez], prHighContrast);
    end;

    if prComponent.Components[vrVez] is TGroupBox then
    begin
      TGroupBox(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TGroupBox(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_FONT_NAME).Value;

      DrawForm(prComponent.Components[vrVez], prHighContrast);
    end;

    if prComponent.Components[vrVez] is TDBGrid then
    begin
      TDBGrid(prComponent.Components[vrVez]).Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
      TDBGrid(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_FONT_NAME).Value;
      TDBGrid(prComponent.Components[vrVez]).Flat      := True;

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

procedure RemoveChildren(prComponent: TComponent);
var
  vrVez : Integer;
begin
  for vrVez := prComponent.ComponentCount - 1 downto 0 do
    prComponent.Components[vrVez].Free;
end;

function IsSameForm(prForm1, prForm2: TForm; prIgnoreVariables: array of String
  ): Boolean;
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

function TJupiterComponentReference.RightCalc: Integer;
begin
  Result := Self.Right;

  if Assigned(Self.WinControl) then
    Result := Self.WinControl.Left + Self.WinControl.Width;
end;

constructor TJupiterComponentReference.Create(prTop, prLeft, prRight, prBottom: Integer; prComponent: TComponent);
begin
  Self.Top       := prTop;
  Self.Left      := prLeft;
  Self.Right     := prRight;
  Self.Bottom    := prBottom;
  Self.Component := prComponent;

  Self.MacroID     := EmptyStr;
  Self.MacroScript := EmptyStr;
  Self.Param       := EmptyStr;
end;

constructor TJupiterComponentReference.Create(prTop, prLeft, prRight, prBottom: Integer; prComponent: TComponent; prFieldName: String);
begin
  Self.Create(prTop, prLeft, prRight, prBottom, prComponent);

  Self.FFieldName := prFieldName;
end;

constructor TJupiterComponentReference.Create(prTop, prLeft, prRight, prBottom: Integer; prComponent: TComponent; prWinControl: TWinControl);
begin
  Self.Create(prTop, prLeft, prRight, prBottom, prComponent);

  Self.FWinControl := prWinControl;
end;

end.

