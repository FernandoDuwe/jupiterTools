unit jupiterformutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ComCtrls, JupiterObject, JupiterConsts, Controls,
  SysUtils, Forms, Graphics, EditBtn, CheckLst, StdCtrls,
  ShellCtrls, SynEdit, DBCtrls, DBDateTimePicker, ActnList,
  Menus, ValEdit, SynHighlighterAny, RichMemo;

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

  procedure PopupMenuShortcutsToActionShortcut(prActions : TActionList; prPopupMenu : TPopupMenu);

  function CreateSynHighlighterMarkDown(prComponent : TComponent) : TSynAnySyn;

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

uses JupiterApp, ExtCtrls, JupiterVariable, DBGrids;

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
  vrVez  : Integer;
  vrVez2 : Integer;
  vrFontSize : Integer;
  vrEditableFontName : String;
  vrFieldFontName : String;
begin
  if not Assigned(vrJupiterApp) then
    Exit;

  if not vrJupiterApp.Params.Exists(FIELD_FONT_SIZE) then
    Exit;

  vrFontSize := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
  vrEditableFontName := vrJupiterApp.Params.VariableById(FIELD_EDITABLES_FONT_NAME).Value;
  vrFieldFontName := vrJupiterApp.Params.VariableById(FIELD_FONT_NAME).Value;

  if prComponent is TForm then
  begin
    TForm(prComponent).Font.Size := vrFontSize;
    TForm(prComponent).Font.Name := vrFieldFontName;
  end;

  for vrVez := 0 to prComponent.ComponentCount - 1 do
  begin
    if prComponent.Components[vrVez] is TLabel then
    begin
      TLabel(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TLabel(prComponent.Components[vrVez]).Font.Name := vrFieldFontName;

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

      Continue;
    end;

    if prComponent.Components[vrVez] is TEdit then
    begin
      TEdit(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TEdit(prComponent.Components[vrVez]).Font.Name := vrEditableFontName;

      if prHighContrast then
      begin
        TEdit(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TEdit(prComponent.Components[vrVez]).ParentColor := False;
        TEdit(prComponent.Components[vrVez]).ParentFont := False;

        TEdit(prComponent.Components[vrVez]).Color := $002E2E2E;
        TEdit(prComponent.Components[vrVez]).Font.Color := clWhite;
      end;

      Continue;
    end;

    if prComponent.Components[vrVez] is TDBEdit then
    begin
      TDBEdit(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TDBEdit(prComponent.Components[vrVez]).Font.Name := vrEditableFontName;

      if prHighContrast then
      begin
        TDBEdit(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TDBEdit(prComponent.Components[vrVez]).ParentColor := False;
        TDBEdit(prComponent.Components[vrVez]).ParentFont := False;

        TDBEdit(prComponent.Components[vrVez]).Color := $002E2E2E;
        TDBEdit(prComponent.Components[vrVez]).Font.Color := clWhite;
      end;

      Continue;
    end;

    if prComponent.Components[vrVez] is TDateEdit then
    begin
      TDateEdit(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TDateEdit(prComponent.Components[vrVez]).Font.Name := vrEditableFontName;

      if prHighContrast then
      begin
        TDateEdit(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TDateEdit(prComponent.Components[vrVez]).ParentColor := False;
        TDateEdit(prComponent.Components[vrVez]).ParentFont := False;

        TDateEdit(prComponent.Components[vrVez]).Color := $002E2E2E;
        TDateEdit(prComponent.Components[vrVez]).Font.Color := clWhite;
      end;

      Continue;
    end;

    if prComponent.Components[vrVez] is TDBDateTimePicker then
    begin
      TDBDateTimePicker(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TDBDateTimePicker(prComponent.Components[vrVez]).Font.Name := vrEditableFontName;

      if prHighContrast then
      begin
        TDBDateTimePicker(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TDBDateTimePicker(prComponent.Components[vrVez]).ParentColor := False;
        TDBDateTimePicker(prComponent.Components[vrVez]).ParentFont := False;

        TDBDateTimePicker(prComponent.Components[vrVez]).Color := $002E2E2E;
        TDBDateTimePicker(prComponent.Components[vrVez]).Font.Color := clWhite;
      end;

      Continue;
    end;

    if prComponent.Components[vrVez] is TComboBox then
    begin
      TComboBox(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TComboBox(prComponent.Components[vrVez]).Font.Name := vrEditableFontName;

      if prHighContrast then
      begin
        TComboBox(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TComboBox(prComponent.Components[vrVez]).ParentColor := False;
        TComboBox(prComponent.Components[vrVez]).ParentFont := False;

        TComboBox(prComponent.Components[vrVez]).Color := $002E2E2E;
        TComboBox(prComponent.Components[vrVez]).Font.Color := clWhite;
      end;

      Continue;
    end;

    if prComponent.Components[vrVez] is TDBComboBox then
    begin
      TDBComboBox(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TDBComboBox(prComponent.Components[vrVez]).Font.Name := vrEditableFontName;

      if prHighContrast then
      begin
        TDBComboBox(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TDBComboBox(prComponent.Components[vrVez]).ParentColor := False;
        TDBComboBox(prComponent.Components[vrVez]).ParentFont := False;

        TDBComboBox(prComponent.Components[vrVez]).Color := $002E2E2E;
        TDBComboBox(prComponent.Components[vrVez]).Font.Color := clWhite;
      end;

      Continue;
    end;

    if prComponent.Components[vrVez] is TDBLookupComboBox then
    begin
      TDBLookupComboBox(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TDBLookupComboBox(prComponent.Components[vrVez]).Font.Name := vrEditableFontName;

      if prHighContrast then
      begin
        TDBLookupComboBox(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TDBLookupComboBox(prComponent.Components[vrVez]).ParentColor := False;
        TDBLookupComboBox(prComponent.Components[vrVez]).ParentFont := False;

        TDBLookupComboBox(prComponent.Components[vrVez]).Color := $002E2E2E;
        TDBLookupComboBox(prComponent.Components[vrVez]).Font.Color := clWhite;
      end;

      Continue;
    end;

    if prComponent.Components[vrVez] is TCheckBox then
    begin
      TCheckBox(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TCheckBox(prComponent.Components[vrVez]).Font.Name := vrEditableFontName;

      if prHighContrast then
      begin
        TCheckBox(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TCheckBox(prComponent.Components[vrVez]).ParentColor := False;
        TCheckBox(prComponent.Components[vrVez]).ParentFont := False;

        TCheckBox(prComponent.Components[vrVez]).Font.Color := clWhite;
      end;

      Continue;
    end;

    if prComponent.Components[vrVez] is TDBCheckBox then
    begin
      TDBCheckBox(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TDBCheckBox(prComponent.Components[vrVez]).Font.Name := vrEditableFontName;

      if prHighContrast then
      begin
        TDBCheckBox(prComponent.Components[vrVez]).ParentBiDiMode := False;
        TDBCheckBox(prComponent.Components[vrVez]).ParentColor := False;
        TDBCheckBox(prComponent.Components[vrVez]).ParentFont := False;

        TDBCheckBox(prComponent.Components[vrVez]).Font.Color := clWhite;
      end;

      Continue;
    end;

    if prComponent.Components[vrVez] is TListView then
    begin
      TListView(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TListView(prComponent.Components[vrVez]).GridLines := True;
      TListView(prComponent.Components[vrVez]).Font.Name := vrFieldFontName;

      Continue;
    end;

    if prComponent.Components[vrVez] is TShellListView then
    begin
      TShellListView(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TShellListView(prComponent.Components[vrVez]).GridLines := True;
      TShellListView(prComponent.Components[vrVez]).Font.Name := vrFieldFontName;

      Continue;
    end;

    if prComponent.Components[vrVez] is TCheckListBox then
    begin
      TCheckListBox(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TCheckListBox(prComponent.Components[vrVez]).Font.Name := vrFieldFontName;

      Continue;
    end;

    if prComponent.Components[vrVez] is TTreeView then
    begin
      TTreeView(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TTreeView(prComponent.Components[vrVez]).Font.Name := vrFieldFontName;

      Continue;
    end;

    if prComponent.Components[vrVez] is TListBox then
    begin
      TListBox(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TListBox(prComponent.Components[vrVez]).Font.Name := vrFieldFontName;

      Continue;
    end;

    if prComponent.Components[vrVez] is TMemo then
    begin
      TMemo(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TMemo(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_MEMO_FONT_NAME).Value;

      Continue;
    end;

    if prComponent.Components[vrVez] is TRichMemo then
    begin
      TRichMemo(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TRichMemo(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_RICHEDIT_FONT_NAME).Value;

      Continue;
    end;

    if ((prComponent.Components[vrVez] is TShape) and (prHighContrast)) then
    begin
      TShape(prComponent.Components[vrVez]).Pen.Color := clWhite;

      Continue;
    end;

    if prComponent.Components[vrVez] is TDBMemo then
    begin
      TDBMemo(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TDBMemo(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_MEMO_FONT_NAME).Value;

      Continue;
    end;

    if prComponent.Components[vrVez] is TSynEdit then
    begin
      TSynEdit(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TSynEdit(prComponent.Components[vrVez]).RightEdge := vrJupiterApp.Params.VariableById(FORM_EDITOR_RIGHTEDGE).AsInteger;
      TSynEdit(prComponent.Components[vrVez]).Font.Name := vrJupiterApp.Params.VariableById(FIELD_SYNEDIT_FONT_NAME).Value;

      Continue;
    end;

    if prComponent.Components[vrVez] is TStatusBar then
    begin
      TStatusBar(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TStatusBar(prComponent.Components[vrVez]).Font.Name := vrFieldFontName;

      Continue;
    end;

    if prComponent.Components[vrVez] is TScrollBox then
    begin
      TScrollBox(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TScrollBox(prComponent.Components[vrVez]).Font.Name := vrFieldFontName;

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

      Continue;
    end;

    if prComponent.Components[vrVez] is TPanel then
    begin
      TPanel(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TPanel(prComponent.Components[vrVez]).Font.Name := vrFieldFontName;

      DrawForm(prComponent.Components[vrVez], prHighContrast);

      Continue;
    end;

    if prComponent.Components[vrVez] is TPageControl then
    begin
      for vrVez2 := 0 to TPageControl(prComponent.Components[vrVez]).PageCount - 1 do
        DrawForm(TPageControl(prComponent.Components[vrVez]).Pages[vrVez2]);

      Continue;
    end;

    if prComponent.Components[vrVez] is TGroupBox then
    begin
      TGroupBox(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TGroupBox(prComponent.Components[vrVez]).Font.Name := vrFieldFontName;

      DrawForm(prComponent.Components[vrVez], prHighContrast);

      Continue;
    end;

    if prComponent.Components[vrVez] is TDBGrid then
    begin
      TDBGrid(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TDBGrid(prComponent.Components[vrVez]).Font.Name := vrFieldFontName;
      TDBGrid(prComponent.Components[vrVez]).Flat      := True;
      TDBGrid(prComponent.Components[vrVez]).Options   := [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColumnMove, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgRowHighlight];

      {$IFDEF WINDOWS}
      
	  if vrJupiterApp.Params.Exists('Interface.Form.Color.Alternative') then
	    TDBGrid(prComponent.Components[vrVez]).AlternateColor := vrJupiterApp.Params.VariableById('Interface.Form.Color.Alternative').AsColor
	  else
	    TDBGrid(prComponent.Components[vrVez]).AlternateColor := ALTERNATIVE_COLOR;
	  
      {$ENDIF}

      Continue;
    end;

    if prComponent.Components[vrVez] is TValueListEditor then
    begin
      TValueListEditor(prComponent.Components[vrVez]).Font.Size := vrFontSize;
      TValueListEditor(prComponent.Components[vrVez]).Font.Name := vrFieldFontName;
      TValueListEditor(prComponent.Components[vrVez]).Flat      := True;

      {$IFDEF WINDOWS}
	  
	  if vrJupiterApp.Params.Exists('Interface.Form.Color.Alternative') then
	    TValueListEditor(prComponent.Components[vrVez]).AlternateColor := vrJupiterApp.Params.VariableById('Interface.Form.Color.Alternative').AsColor
	  else
	    TValueListEditor(prComponent.Components[vrVez]).AlternateColor := ALTERNATIVE_COLOR;
      
      {$ENDIF}

      Continue;
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
  vrFontSize : Integer;
  vrEditableFontName : String;
  vrFieldFontName : String;
begin
  vrFontSize := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
  vrEditableFontName := vrJupiterApp.Params.VariableById(FIELD_EDITABLES_FONT_NAME).Value;
  vrFieldFontName := vrJupiterApp.Params.VariableById(FIELD_FONT_NAME).Value;

  for vrVez := 0 to prComponent.ComponentCount - 1 do
  begin
    if prComponent.Components[vrVez] is TLabel then
    begin
      TLabel(prComponent.Components[vrVez]).Font.Size := vrFontSize;

      if SearchIsPartOf(TLabel(prComponent.Components[vrVez]).Caption, prSearch) then
        TLabel(prComponent.Components[vrVez]).Font.Color := prColor;
    end;

    if prComponent.Components[vrVez] is TEdit then
    begin
      TEdit(prComponent.Components[vrVez]).Font.Size := vrFontSize;

      if SearchIsPartOf(TEdit(prComponent.Components[vrVez]).Text, prSearch) then
        TEdit(prComponent.Components[vrVez]).Font.Color := prColor;
    end;

    if prComponent.Components[vrVez] is TComboBox then
    begin
      TComboBox(prComponent.Components[vrVez]).Font.Size := vrFontSize;

      if SearchIsPartOf(TComboBox(prComponent.Components[vrVez]).Text, prSearch) then
        TComboBox(prComponent.Components[vrVez]).Font.Color := prColor;
    end;

    if prComponent.Components[vrVez] is TCheckBox then
    begin
      TCheckBox(prComponent.Components[vrVez]).Font.Size := vrFontSize;

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

procedure PopupMenuShortcutsToActionShortcut(prActions: TActionList; prPopupMenu: TPopupMenu);
var
  vrVez : Integer;
  vrAction : TAction;
begin
  for vrVez := 0 to prPopupMenu.Items.Count -1  do
  begin
    if prPopupMenu.Items[vrVez].ShortCut = 0 then
      Continue;

    vrAction := TAction.Create(prActions);
    vrAction.ActionList := prActions;
    vrAction.Caption := prPopupMenu.Items[vrVez].Caption;
    vrAction.Hint := prPopupMenu.Items[vrVez].Hint;
    vrAction.OnExecute := prPopupMenu.Items[vrVez].OnClick;
    vrAction.ShortCut := prPopupMenu.Items[vrVez].ShortCut;
  end;
end;

function CreateSynHighlighterMarkDown(prComponent : TComponent) : TSynAnySyn;
begin
  Result := TSynAnySyn.Create(prComponent);
//  Result.SymbolAttri.Foreground := clPurple;
//  Result.KeyAttri.Foreground := clPurple;
  Result.StringAttri.Foreground := clMoneyGreen;
  Result.SymbolAttri.Style := [fsBold];
  Result.KeyAttri.Style := [fsBold];
  Result.StringAttri.Style := [fsBold];

  Result.StringDelim := sdSingleQuote;   // código inline
  // Result.Symbols := '#*_-';    // símbolos markdown

  Result.KeyWords.Add('#');
  Result.KeyWords.Add('##');
  Result.KeyWords.Add('###');
  Result.KeyWords.Add('####');
  Result.KeyWords.Add('#####');
  Result.KeyWords.Add('######');
  Result.KeyWords.Add('*');
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

