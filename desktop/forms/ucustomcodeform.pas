unit uCustomCodeForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, uJupiterForm,
  JupiterApp, JupiterObject, jupiterformutils, JupiterConsts, JupiterVariable,
  jupiterDesktopApp, jupiterformcomponenttils, uJupiterAction, ComCtrls,
  ExtCtrls;

type

  { TFCustomCodeForm }

  TFCustomCodeForm = class(TFJupiterForm)
    fpTile: TFlowPanel;
    sbBody: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCustomInterval : Integer;
    FCurrentLine : Integer;
    FCurrentMargin : Integer;
    FReferences : TJupiterObjectList;
    FCustomColor : TColor;
    FListItemCounter : Integer;
    FHighContrast : Boolean;

    procedure Internal_OnCheckBoxChange(Sender: TObject);
    procedure Internal_OnLinkClick(Sender: TObject);
    procedure Internal_OnCopyClick(Sender: TObject);

    procedure Internal_OnFieldChange(Sender : TObject);

    function Internal_OnRequestData : TJupiterVariableList; override;

    procedure Internal_PrepareForm; override;

    procedure Internal_UpdateComponents; override;

    function Internal_GetNextColor : TColor;
  published
    property CurrentLine : Integer read FCurrentLine;

    property References : TJupiterObjectList read FReferences write FReferences;
  public
    procedure AddLabel(prLabelCaption : String);
    procedure AddLabelResultFromScript(prMacroID : String);
    procedure AddLabelBoldResultFromScript(prMacroID : String);
    procedure AddLabelResultFromScriptWithParams(prMacroID, prParam : String);
    procedure AddLabelBoldResultFromScriptWithParams(prMacroID, prParam : String);
    procedure AddLabelBold(prLabelCaption : String);
    procedure AddEdit(prVariableId, prInitialValue : String);
    procedure AddProgressBar(prValue, prMin, prMax : Integer);
    procedure AddAnimatedProgressBar(prValue, prMin, prMax : Integer);
    procedure AddCombBox(prDataProviderID, prColumn, prVariableID : String);
    procedure AddCheckBox(prVariableId, prText : String; prValue : Boolean);
    procedure AddAction(prCaption, prHint : String; prIcon : Integer; prMacroID : String);
    procedure AddLink(prCaption : String; prMacroID : String);
    procedure AddLinkWithParams(prCaption : String; prMacroID, prParam : String);
    procedure AddLinkBoldWithParams(prCaption : String; prMacroID, prParam : String);
    procedure AddLinkBold(prCaption : String; prMacroID : String);
    procedure AddLinkAsScript(prCaption : String; prMacro : TStrings);
    procedure AddLinkBoldAsScript(prCaption : String; prMacro : TStrings);
    procedure AddActionWithScript(prCaption, prHint : String; prIcon : Integer; prMacro : TStrings);
    procedure AddLine(prTop, prLeft, prHeight, prWidth: Integer);
    procedure AddTile(prTitle : String; prCounter : Integer);
    procedure AddErrorTile(prTitle : String; prCounter : Integer);
    procedure AddListItem(prTitle, prSubtitle : String);
    procedure AddListActionItem(prTitle, prSubtitle, prMacroId, prParams : String);
    procedure AddErrorListItem(prTitle, prSubtitle : String);
    procedure AddSuccessListItem(prTitle, prSubtitle : String);
    procedure JumpLine;
    procedure SetCurrentMargin(prMargin : Integer);
    procedure SetCurrentLine(prLine : Integer);
    procedure SetCustomInterval(prInterval : Integer);
    procedure SetFormToHighFocus;
  end;

var
  FCustomCodeForm: TFCustomCodeForm;

implementation

uses Buttons, JupiterEdit, Clipbrd;

{$R *.lfm}

{ TFCustomCodeForm }

procedure TFCustomCodeForm.FormCreate(Sender: TObject);
begin
  Self.FCustomColor  := clMenuHighlight;
  Self.FHighContrast := False;

  Self.FCurrentLine := 0;
  Self.FListItemCounter := 0;
  Self.FCustomInterval := vrJupiterApp.Params.VariableById(FORM_UPDATE_TIME).AsInteger;

  Self.FReferences := TJupiterObjectList.Create;

  if Assigned(vrJupiterApp) then
    if Assigned(TJupiterDesktopApp(vrJupiterApp).FormList) then
      TJupiterDesktopApp(vrJupiterApp).FormList.AddSimpleObject(Self);

  Self.FCurrentMargin := FORM_MARGIN_LEFT;

  inherited;
end;

procedure TFCustomCodeForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Self.FReferences);

  if Assigned(vrJupiterApp) then
    if Assigned(TJupiterDesktopApp(vrJupiterApp).FormList) then
      TJupiterDesktopApp(vrJupiterApp).DeleteFormById(Self.FormID);

  inherited;
end;

procedure TFCustomCodeForm.Internal_OnCheckBoxChange(Sender: TObject);
begin
  //
end;

procedure TFCustomCodeForm.Internal_OnLinkClick(Sender: TObject);
var
  vrReference : TJupiterComponentReference;
begin
  if (Sender is TLabel) then
  begin
    vrReference := (Self.References.GetAtIndex(TLabel(Sender).Tag)) as TJupiterComponentReference;

    if Trim(vrReference.MacroID) <> '' then
      vrJupiterApp.RunMacro(vrReference.MacroID, CreateVariableListOfParam(vrReference.Param));

    if Trim(vrReference.MacroScript) <> '' then
      vrJupiterApp.RunScript(CreateStringList(vrReference.MacroScript), CreateVariableListOfParam(vrReference.Param));

    Self.UpdateForm();
  end;
end;

procedure TFCustomCodeForm.Internal_OnCopyClick(Sender: TObject);
var
  vrReference : TJupiterComponentReference;
begin
  if (Sender is TSpeedButton) then
  begin
    vrReference := (Self.References.GetAtIndex(TEdit(Sender).Tag)) as TJupiterComponentReference;

    Clipboard.AsText := TEdit(vrReference.Component).Text;
  end;
end;

procedure TFCustomCodeForm.Internal_OnFieldChange(Sender: TObject);
var
  vrReference : TJupiterComponentReference;
begin
  if (Sender is TEdit) then
  begin
    vrReference := (Self.References.GetAtIndex(TEdit(Sender).Tag)) as TJupiterComponentReference;

    Self.Params.VariableById(vrReference.FieldName).Value := TEdit(Sender).Text;
  end;

  if (Sender is TComboBox) then
  begin
    vrReference := (Self.References.GetAtIndex(TComboBox(Sender).Tag)) as TJupiterComponentReference;

    Self.Params.VariableById(vrReference.FieldName).Value := TComboBox(Sender).Text;
  end;

  if (Sender is TCheckBox) then
  begin
    vrReference := (Self.References.GetAtIndex(TEdit(Sender).Tag)) as TJupiterComponentReference;

    if TCheckBox(Sender).Checked then
      Self.Params.VariableById(vrReference.FieldName).Value := BOOL_TRUE_STR
    else
      Self.Params.VariableById(vrReference.FieldName).Value := BOOL_FALSE_STR;
  end;
end;

function TFCustomCodeForm.Internal_OnRequestData: TJupiterVariableList;
begin
  Result := inherited Internal_OnRequestData;

  Result.CopyValues(Self.Params);
end;

procedure TFCustomCodeForm.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  tmrAutoUpdater.Interval := Self.FCustomInterval;
end;

procedure TFCustomCodeForm.Internal_UpdateComponents;
var
  vrVez : Integer;
  vrParams : TJupiterVariableList;
begin
  inherited Internal_UpdateComponents;

  for vrVez := 0 to Self.FReferences.Count - 1 do
  begin
    if TJupiterComponentReference(Self.FReferences.GetAtIndex(vrVez)).Tag <> 1000 then
      Continue;

    if TJupiterComponentReference(Self.FReferences.GetAtIndex(vrVez)).Component is TLabel then
    begin
      if TJupiterComponentReference(Self.FReferences.GetAtIndex(vrVez)).Param <> EmptyStr then
        TLabel(TJupiterComponentReference(Self.FReferences.GetAtIndex(vrVez)).Component).Caption := vrJupiterApp.RunMacroAsResult(TJupiterComponentReference(Self.FReferences.GetAtIndex(vrVez)).MacroID, CreateVariableListOfParam(TJupiterComponentReference(Self.FReferences.GetAtIndex(vrVez)).Param))
      else
      begin
        vrParams := TJupiterVariableList.Create;
        vrParams.CopyValues(Self.Params);

        TLabel(TJupiterComponentReference(Self.FReferences.GetAtIndex(vrVez)).Component).Caption := vrJupiterApp.RunMacroAsResult(TJupiterComponentReference(Self.FReferences.GetAtIndex(vrVez)).MacroID, vrParams);
      end;
    end;
  end;

  DrawForm(Self, Self.FHighContrast);
end;

function TFCustomCodeForm.Internal_GetNextColor: TColor;
begin
  try
    if Self.FCustomColor = $00FFC175 then
    begin
      Self.FCustomColor := clSkyBlue;
      Exit;
    end;

    if Self.FCustomColor = clSkyBlue then
    begin
      Self.FCustomColor := clCream;
      Exit;
    end;

    if Self.FCustomColor = clCream then
    begin
      Self.FCustomColor := clMoneyGreen;
      Exit;
    end;

    if Self.FCustomColor = clMoneyGreen then
    begin
      Self.FCustomColor := clSilver;
      Exit;
    end;

    Self.FCustomColor := $00FFC175;
  finally
    Result := Self.FCustomColor;
  end;
end;

procedure TFCustomCodeForm.AddLabel(prLabelCaption: String);
var
  vrReference : TJupiterComponentReference;
begin
  Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

  vrReference := JupiterComponentsNewLabel(prLabelCaption, TJupiterPosition.Create(Self.FCurrentLine, Self.FCurrentMargin), sbBody);

  Self.FCurrentLine := vrReference.Bottom;
end;

procedure TFCustomCodeForm.AddLabelResultFromScript(prMacroID: String);
var
  vrReference : TJupiterComponentReference;
begin
  Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

  vrReference := JupiterComponentsNewLabel('', TJupiterPosition.Create(Self.FCurrentLine, Self.FCurrentMargin), sbBody);

  vrReference.MacroID := prMacroID;
  vrReference.Tag := 1000;

  Self.FCurrentLine := vrReference.Bottom;

  Self.FReferences.Add(vrReference);
end;

procedure TFCustomCodeForm.AddLabelBoldResultFromScript(prMacroID: String);
var
  vrReference : TJupiterComponentReference;
begin
  Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

  vrReference := JupiterComponentsNewLabel('', TJupiterPosition.Create(Self.FCurrentLine, Self.FCurrentMargin), sbBody);

  vrReference.MacroID := prMacroID;
  vrReference.Tag := 1000;

  TLabel(vrReference.Component).Font.Style := [fsBold];

  Self.FCurrentLine := vrReference.Bottom;

  Self.FReferences.Add(vrReference);
end;

procedure TFCustomCodeForm.AddLabelResultFromScriptWithParams(prMacroID, prParam: String);
var
  vrReference : TJupiterComponentReference;
begin
  Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

  vrReference := JupiterComponentsNewLabel('', TJupiterPosition.Create(Self.FCurrentLine, Self.FCurrentMargin), sbBody);

  vrReference.MacroID := prMacroID;
  vrReference.Param := prParam;
  vrReference.Tag := 1000;

  Self.FCurrentLine := vrReference.Bottom;

  Self.FReferences.Add(vrReference);
end;

procedure TFCustomCodeForm.AddLabelBoldResultFromScriptWithParams(prMacroID, prParam: String);
var
  vrReference : TJupiterComponentReference;
begin
  Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

  vrReference := JupiterComponentsNewLabel('', TJupiterPosition.Create(Self.FCurrentLine, Self.FCurrentMargin), sbBody);

  vrReference.MacroID := prMacroID;
  vrReference.Param   := prParam;

  vrReference.Tag := 1000;

  TLabel(vrReference.Component).Font.Style := [fsBold];

  Self.FCurrentLine := vrReference.Bottom;

  Self.FReferences.Add(vrReference);
end;

procedure TFCustomCodeForm.AddLabelBold(prLabelCaption: String);
var
  vrReference : TJupiterComponentReference;
begin
  Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

  vrReference := JupiterComponentsNewLabel(prLabelCaption, TJupiterPosition.Create(Self.FCurrentLine, Self.FCurrentMargin), sbBody);

  TLabel(vrReference.Component).Font.Style := [fsBold];

  Self.FCurrentLine := vrReference.Bottom;
end;

procedure TFCustomCodeForm.AddEdit(prVariableId, prInitialValue : String);
var
  vrReference : TJupiterComponentReference;
  vrAction : TJupiterComponentReference;
begin
  try
    Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

    vrReference := JupiterComponentsNewEdit(EmptyStr, TJupiterPosition.Create(Self.FCurrentLine, Self.FCurrentMargin), sbBody);

    Self.FCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;

    vrReference.Tag := 0;
  finally
    Self.References.Add(vrReference);

    vrReference.FieldName := prVariableId;
    TEdit(vrReference.Component).Tag := Self.References.Count - 1;
    TEdit(vrReference.Component).OnChange := @Internal_OnFieldChange;
    TEdit(vrReference.Component).Text := prInitialValue;

    Self.Params.AddVariable(prVariableId, prInitialValue);

    vrAction := JupiterComponentsAddAction(vrReference, ICON_COPY, sbBody);
    TSpeedButton(vrAction.Component).Tag := Self.References.Count - 1;
    TSpeedButton(vrAction.Component).OnClick := @Internal_OnCopyClick;
    TSpeedButton(vrAction.Component).Hint := 'Clique aqui para copiar o conteúdo do campo';
    TSpeedButton(vrAction.Component).ShowHint := True;
  end;
end;

procedure TFCustomCodeForm.AddProgressBar(prValue, prMin, prMax : Integer);
var
  vrReference : TJupiterComponentReference;
begin
  Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

  vrReference := JupiterComponentsNewProgressBar(prValue, prMin, prMax, TJupiterPosition.Create(Self.FCurrentLine, Self.FCurrentMargin), sbBody);

  Self.FCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;
end;

procedure TFCustomCodeForm.AddAnimatedProgressBar(prValue, prMin, prMax: Integer);
var
  vrReference : TJupiterComponentReference;
begin
  Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

  vrReference := JupiterComponentsNewProgressBar(prValue, prMin, prMax, TJupiterPosition.Create(Self.FCurrentLine, Self.FCurrentMargin), sbBody);

  TProgressBar(vrReference.Component).Style := pbstMarquee;

  Self.FCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;
end;

procedure TFCustomCodeForm.AddCombBox(prDataProviderID, prColumn, prVariableID: String);
var
  vrReference : TJupiterComponentReference;
begin
  try
    Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

    vrReference := JupiterComponentsNewComboBox(prDataProviderID, prColumn, TJupiterPosition.Create(Self.FCurrentLine, Self.FCurrentMargin), sbBody);

    Self.FCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;
  finally
    Self.References.Add(vrReference);

    vrReference.FieldName := prVariableId;
    TComboBox(vrReference.Component).Tag := Self.References.Count - 1;
    TComboBox(vrReference.Component).OnChange := @Internal_OnFieldChange;

    Self.Params.AddVariable(prVariableId, EmptyStr);
  end;
end;

procedure TFCustomCodeForm.AddCheckBox(prVariableId, prText: String; prValue: Boolean);
var
  vrReference : TJupiterComponentReference;
begin
  try
    Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

    vrReference := JupiterComponentsNewCheckBox(prText, prValue, TJupiterPosition.Create(Self.FCurrentLine, Self.FCurrentMargin), sbBody, @Internal_OnCheckBoxChange);

    Self.FCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;
  finally
    Self.References.Add(vrReference);

    vrReference.FieldName := prVariableId;
    TCheckBox(vrReference.Component).Tag := Self.References.Count - 1;

    TCheckBox(vrReference.Component).OnChange := @Internal_OnFieldChange;

    if TCheckBox(vrReference.Component).Checked then
      Self.Params.AddVariable(prVariableId, BOOL_TRUE_STR)
    else
      Self.Params.AddVariable(prVariableId, BOOL_FALSE_STR);
  end;
end;

procedure TFCustomCodeForm.AddAction(prCaption, prHint : String; prIcon : Integer; prMacroID : String);
begin
  Self.ActionGroup.AddAction(TJupiterAction.Create(prCaption, prHint, prIcon, prMacroID));
end;

procedure TFCustomCodeForm.AddLink(prCaption: String; prMacroID: String);
var
  vrReference : TJupiterComponentReference;
begin
  try
    Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

    vrReference := JupiterComponentsNewLink(prCaption, TJupiterPosition.Create(Self.FCurrentLine, Self.FCurrentMargin), sbBody);

    Self.FCurrentLine := vrReference.Bottom;
  finally
    vrReference.MacroID := prMacroID;
    vrReference.FieldName := EmptyStr;

    Self.References.Add(vrReference);

    TLabel(vrReference.Component).Tag := Self.References.Count - 1;

    TLabel(vrReference.Component).OnClick := @Internal_OnLinkClick;
  end;
end;

procedure TFCustomCodeForm.AddLinkWithParams(prCaption: String; prMacroID, prParam: String);
var
  vrReference : TJupiterComponentReference;
begin
  try
    Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

    vrReference := JupiterComponentsNewLink(prCaption, TJupiterPosition.Create(Self.FCurrentLine, Self.FCurrentMargin), sbBody);

    Self.FCurrentLine := vrReference.Bottom;
  finally
    vrReference.MacroID := prMacroID;
    vrReference.FieldName := EmptyStr;
    vrReference.Param := prParam;

    Self.References.Add(vrReference);

    TLabel(vrReference.Component).Tag := Self.References.Count - 1;

    TLabel(vrReference.Component).OnClick := @Internal_OnLinkClick;
  end;
end;

procedure TFCustomCodeForm.AddLinkBoldWithParams(prCaption: String; prMacroID, prParam: String);
var
  vrReference : TJupiterComponentReference;
begin
  try
    Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

    vrReference := JupiterComponentsNewLink(prCaption, TJupiterPosition.Create(Self.FCurrentLine, Self.FCurrentMargin), sbBody);

    Self.FCurrentLine := vrReference.Bottom;
  finally
    vrReference.MacroID := prMacroID;
    vrReference.FieldName := EmptyStr;
    vrReference.Param := prParam;

    Self.References.Add(vrReference);

    TLabel(vrReference.Component).Font.Style := [fsBold];

    TLabel(vrReference.Component).Tag := Self.References.Count - 1;

    TLabel(vrReference.Component).OnClick := @Internal_OnLinkClick;
  end;
end;

procedure TFCustomCodeForm.AddLinkBold(prCaption: String; prMacroID: String);
var
  vrReference : TJupiterComponentReference;
begin
  try
    Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

    vrReference := JupiterComponentsNewLink(prCaption, TJupiterPosition.Create(Self.FCurrentLine, Self.FCurrentMargin), sbBody);

    Self.FCurrentLine := vrReference.Bottom;
  finally
    vrReference.MacroID := prMacroID;
    vrReference.FieldName := EmptyStr;

    Self.References.Add(vrReference);

    TLabel(vrReference.Component).Font.Style := [fsBold];

    TLabel(vrReference.Component).Tag := Self.References.Count - 1;

    TLabel(vrReference.Component).OnClick := @Internal_OnLinkClick;
  end;
end;

procedure TFCustomCodeForm.AddLinkAsScript(prCaption: String; prMacro: TStrings);
var
  vrReference : TJupiterComponentReference;
begin
  try
    Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

    vrReference := JupiterComponentsNewLink(prCaption, TJupiterPosition.Create(Self.FCurrentLine, Self.FCurrentMargin), sbBody);

    Self.FCurrentLine := vrReference.Bottom;
  finally
    vrReference.MacroID := EmptyStr;
    vrReference.MacroScript := prMacro.Text;
    vrReference.FieldName := EmptyStr;

    Self.References.Add(vrReference);

    TLabel(vrReference.Component).Tag := Self.References.Count - 1;

    TLabel(vrReference.Component).OnClick := @Internal_OnLinkClick;
  end;
end;

procedure TFCustomCodeForm.AddLinkBoldAsScript(prCaption: String; prMacro: TStrings);
var
  vrReference : TJupiterComponentReference;
begin
  try
    Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

    vrReference := JupiterComponentsNewLink(prCaption, TJupiterPosition.Create(Self.FCurrentLine, Self.FCurrentMargin), sbBody);

    Self.FCurrentLine := vrReference.Bottom;
  finally
    vrReference.MacroID := EmptyStr;
    vrReference.MacroScript := prMacro.Text;
    vrReference.FieldName := EmptyStr;

    Self.References.Add(vrReference);

    TLabel(vrReference.Component).Font.Style := [fsBold];

    TLabel(vrReference.Component).Tag := Self.References.Count - 1;

    TLabel(vrReference.Component).OnClick := @Internal_OnLinkClick;
  end;
end;

procedure TFCustomCodeForm.AddActionWithScript(prCaption, prHint: String; prIcon: Integer; prMacro: TStrings);
begin
  Self.ActionGroup.AddAction(TJupiterAction.Create(prCaption, prHint, prIcon, prMacro));
end;

procedure TFCustomCodeForm.AddLine(prTop, prLeft, prHeight, prWidth: Integer);
var
  vrReference : TJupiterComponentReference;
begin
  Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

  vrReference := JupiterComponentsAddLine(TJupiterPosition.Create(prTop, prLeft), prHeight, prWidth, sbBody);

  Self.FCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;
end;

procedure TFCustomCodeForm.AddTile(prTitle: String; prCounter: Integer);
var
  vrReference : TJupiterComponentReference;
begin
  vrReference := JupiterComponentsNewTile(prTitle, prCounter, 100, PercentOfScreen(fpTile.Width, 22), TJupiterPosition.Create(0, 0), fpTile);

  TPanel(vrReference.Component).BevelOuter := bvRaised;

  TPanel(vrReference.Component).BorderSpacing.Around := 2;

  TPanel(vrReference.Component).Color := Self.Internal_GetNextColor;

  fpTile.BorderSpacing.Bottom := FORM_MARGIN_BOTTOM;
end;

procedure TFCustomCodeForm.AddErrorTile(prTitle: String; prCounter: Integer);
var
  vrReference : TJupiterComponentReference;
begin
  vrReference := JupiterComponentsNewTile(prTitle, prCounter, 100, PercentOfScreen(fpTile.Width, 22), TJupiterPosition.Create(0, 0), fpTile);

  TPanel(vrReference.Component).BevelOuter := bvRaised;

  TPanel(vrReference.Component).BorderSpacing.Around := 2;

  TPanel(vrReference.Component).Color := Self.Internal_GetNextColor;
  TPanel(vrReference.Component).Font.Color := clRed;

  fpTile.BorderSpacing.Bottom := FORM_MARGIN_BOTTOM;
end;

procedure TFCustomCodeForm.AddListItem(prTitle, prSubtitle: String);
var
  vrReference : TJupiterComponentReference;
begin
  try
    vrReference := JupiterComponentsNewListItem(prTitle, prSubtitle, sbBody);

//    TPanel(vrReference.Component).BevelOuter := bvRaised;

    if (Self.FListItemCounter mod 2) = 0 then
    begin
      TPanel(vrReference.Component).ParentBackground := False;
      TPanel(vrReference.Component).ParentColor := False;
      TPanel(vrReference.Component).Color := ALTERNATIVE_COLOR;
    end;
  finally
    Self.FListItemCounter := Self.FListItemCounter + 1;
  end;
end;

procedure TFCustomCodeForm.AddListActionItem(prTitle, prSubtitle, prMacroId, prParams: String);
var
  vrReference : TJupiterComponentReference;
begin
  try
    vrReference := JupiterComponentsNewListActionItem(prTitle, prSubtitle, prMacroId, prParams, sbBody);

//    TPanel(vrReference.Component).BevelOuter := bvRaised;

    if (Self.FListItemCounter mod 2) = 0 then
    begin
      TPanel(vrReference.Component).ParentBackground := False;
      TPanel(vrReference.Component).ParentColor := False;
      TPanel(vrReference.Component).Color := ALTERNATIVE_COLOR;
    end;
  finally
    Self.FListItemCounter := Self.FListItemCounter + 1;
  end;
end;

procedure TFCustomCodeForm.AddErrorListItem(prTitle, prSubtitle: String);
var
  vrReference : TJupiterComponentReference;
begin
  try
    vrReference := JupiterComponentsNewListItem(prTitle, prSubtitle, sbBody);

    if (Self.FListItemCounter mod 2) = 0 then
    begin
      TPanel(vrReference.Component).ParentBackground := False;
      TPanel(vrReference.Component).ParentColor := False;
      TPanel(vrReference.Component).Color := ALTERNATIVE_COLOR;
    end;

    TPanel(vrReference.Component).Font.Color := clRed;
  finally
    Self.FListItemCounter := Self.FListItemCounter + 1;
  end;
end;

procedure TFCustomCodeForm.AddSuccessListItem(prTitle, prSubtitle: String);
var
  vrReference : TJupiterComponentReference;
begin
  try
    vrReference := JupiterComponentsNewListItem(prTitle, prSubtitle, sbBody);

    if (Self.FListItemCounter mod 2) = 0 then
    begin
      TPanel(vrReference.Component).ParentBackground := False;
      TPanel(vrReference.Component).ParentColor := False;
      TPanel(vrReference.Component).Color := ALTERNATIVE_COLOR;
    end;

    TPanel(vrReference.Component).Font.Color := clOlive;
  finally
    Self.FListItemCounter := Self.FListItemCounter + 1;
  end;
end;

procedure TFCustomCodeForm.JumpLine;
begin
  Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;
end;

procedure TFCustomCodeForm.SetCurrentMargin(prMargin: Integer);
begin
  Self.FCurrentMargin := prMargin;
end;

procedure TFCustomCodeForm.SetCurrentLine(prLine: Integer);
begin
  Self.FCurrentLine := prLine;
end;

procedure TFCustomCodeForm.SetCustomInterval(prInterval: Integer);
begin
  Self.FCustomInterval := prInterval;
end;

procedure TFCustomCodeForm.SetFormToHighFocus;
begin
  Self.FHighContrast := True;

  UpdateForm();
end;

end.

