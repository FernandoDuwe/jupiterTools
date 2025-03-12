unit uJupiterFormDesktopAppScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, jupiterScript, JupiterConsts, JupiterApp,
  SysUtils, PascalScript, uPSComponent, Forms, SQLDB;

type

  { TJupiterFormDesktopAppScript }

  TJupiterFormDesktopAppScript = class(TJupiterScriptLibrary)
  protected
    function Internal_GetName : String; override;
  public
    procedure DoCompile(prSender: TPSScript); override;
    function AnalyseCode: TJupiterScriptAnalyserList; override;
  end;

  procedure JupiterFormDesktopAppScriptSetCaption(prFormID, prCaption : String);
  procedure JupiterFormDesktopAppScriptSetHint(prFormID, prHint : String);
  procedure JupiterFormDesktopAppScriptAddLabel(prFormID, prCaption : String);
  procedure JupiterFormDesktopAppScriptAddLabelResultFromMacro(prFormID, prMacroId : String);
  procedure JupiterFormDesktopAppScriptAddLabelBoldResultFromMacro(prFormID, prMacroId : String);
  procedure JupiterFormDesktopAppScriptAddLabelResultFromMacroWithParams(prFormID, prMacroId, prParam : String);
  procedure JupiterFormDesktopAppScriptAddLabelBoldResultFromMacroWithParams(prFormID, prMacroId, prParam : String);
  procedure JupiterFormDesktopAppScriptAddLabelBold(prFormID, prCaption : String);
  procedure JupiterFormDesktopAppScriptAddLink(prFormID, prCaption, prMacroId : String);
  procedure JupiterFormDesktopAppScriptAddLinkWithParams(prFormID, prCaption, prMacroId, prParams : String);
  procedure JupiterFormDesktopAppScriptAddLinkBoldWithParams(prFormID, prCaption, prMacroId, prParams : String);
  procedure JupiterFormDesktopAppScriptAddLinkBold(prFormID, prCaption, prMacroId : String);
  procedure JupiterFormDesktopAppScriptAddLinkWithScript(prFormID, prCaption : String; prMacro : TStrings);
  procedure JupiterFormDesktopAppScriptAddLinkBoldWithScript(prFormID, prCaption : String; prMacro : TStrings);
  procedure JupiterFormDesktopAppScriptAddEdit(prFormID, prVariableId, prInitialValue : String);
  procedure JupiterFormDesktopAppScriptAddCombo(prFormID, prVariableId, prDataProvider, prColumn : String);
  procedure JupiterFormDesktopAppScriptAddCheckBox(prFormID, prVariableId, prText: String; prValue: Boolean);
  procedure JupiterFormDesktopAppScriptAddAction(prFormID, prCaption, prHint : String; prIcon : Integer; prMacroID : String);
  procedure JupiterFormDesktopAppScriptAddActionWithScript(prFormID, prCaption, prHint : String; prIcon : Integer; prMacro : TStrings);
  procedure JupiterFormDesktopAppScriptJumpLine(prFormID : String);
  procedure JupiterFormDesktopAppScriptSetCurrentMargin(prFormID : String; prMargin : Integer);
  procedure JupiterFormDesktopAppScriptSetCurrentLine(prFormID : String; prLine : Integer);
  procedure JupiterFormDesktopAppScriptSetCustomInterval(prFormID : String; prInterval : Integer);
  procedure JupiterFormDesktopAppScriptSetFormToHighFocus(prFormID : String);

implementation

uses uCustomCodeForm, jupiterDesktopApp, uJupiterForm;

procedure JupiterFormDesktopAppScriptSetCaption(prFormID, prCaption: String);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  vrForm.Caption := prCaption;
end;

procedure JupiterFormDesktopAppScriptSetHint(prFormID, prHint: String);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  TFJupiterForm(vrForm).Hint := prHint;
  TFJupiterForm(vrForm).FHint := prHint;
end;

procedure JupiterFormDesktopAppScriptAddLabel(prFormID, prCaption: String);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).AddLabel(prCaption);
end;

procedure JupiterFormDesktopAppScriptAddLabelResultFromMacro(prFormID, prMacroId: String);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).AddLabelResultFromScript(prMacroId);
end;

procedure JupiterFormDesktopAppScriptAddLabelBoldResultFromMacro(prFormID, prMacroId: String);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).AddLabelBoldResultFromScript(prMacroId);
end;

procedure JupiterFormDesktopAppScriptAddLabelResultFromMacroWithParams(prFormID, prMacroId, prParam: String);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).AddLabelResultFromScriptWithParams(prMacroId, prParam);
end;

procedure JupiterFormDesktopAppScriptAddLabelBoldResultFromMacroWithParams(prFormID, prMacroId, prParam: String);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).AddLabelBoldResultFromScriptWithParams(prMacroId, prParam);
end;

procedure JupiterFormDesktopAppScriptAddLabelBold(prFormID, prCaption: String);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).AddLabelBold(prCaption);
end;

procedure JupiterFormDesktopAppScriptAddLink(prFormID, prCaption, prMacroId: String);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).AddLink(prCaption, prMacroId);
end;

procedure JupiterFormDesktopAppScriptAddLinkWithParams(prFormID, prCaption, prMacroId, prParams: String);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).AddLinkWithParams(prCaption, prMacroId, prParams);
end;

procedure JupiterFormDesktopAppScriptAddLinkBoldWithParams(prFormID, prCaption, prMacroId, prParams: String);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).AddLinkBoldWithParams(prCaption, prMacroId, prParams);
end;

procedure JupiterFormDesktopAppScriptAddLinkBold(prFormID, prCaption, prMacroId: String);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).AddLinkBold(prCaption, prMacroId);
end;

procedure JupiterFormDesktopAppScriptAddLinkWithScript(prFormID, prCaption : String; prMacro : TStrings);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).AddLinkAsScript(prCaption, prMacro);
end;

procedure JupiterFormDesktopAppScriptAddLinkBoldWithScript(prFormID, prCaption: String; prMacro: TStrings);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).AddLinkBoldAsScript(prCaption, prMacro);
end;

procedure JupiterFormDesktopAppScriptAddEdit(prFormID, prVariableId, prInitialValue: String);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).AddEdit(prVariableId,prInitialValue);
end;

procedure JupiterFormDesktopAppScriptAddCombo(prFormID, prVariableId, prDataProvider, prColumn : String);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).AddCombBox(prDataProvider, prColumn, prVariableId);
end;

procedure JupiterFormDesktopAppScriptAddCheckBox(prFormID, prVariableId, prText: String; prValue: Boolean);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).AddCheckBox(prVariableId, prText, prValue);
end;

procedure JupiterFormDesktopAppScriptAddAction(prFormID, prCaption, prHint: String; prIcon: Integer; prMacroID: String);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).AddAction(prCaption, prHint, prIcon, prMacroID);
end;

procedure JupiterFormDesktopAppScriptAddActionWithScript(prFormID, prCaption, prHint: String; prIcon: Integer; prMacro: TStrings);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).AddActionWithScript(prCaption, prHint, prIcon, prMacro);
end;

procedure JupiterFormDesktopAppScriptJumpLine(prFormID: String);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).JumpLine();
end;

procedure JupiterFormDesktopAppScriptSetCurrentMargin(prFormID: String;
  prMargin: Integer);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).SetCurrentMargin(prMargin);
end;

procedure JupiterFormDesktopAppScriptSetCurrentLine(prFormID: String;
  prLine: Integer);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).SetCurrentLine(prLine);
end;

procedure JupiterFormDesktopAppScriptSetCustomInterval(prFormID: String;
  prInterval: Integer);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).SetCustomInterval(prInterval);
end;

procedure JupiterFormDesktopAppScriptSetFormToHighFocus(prFormID: String);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  TFCustomCodeForm(vrForm).SetFormToHighFocus();
end;

{ TJupiterFormDesktopAppScript }

function TJupiterFormDesktopAppScript.Internal_GetName: String;
begin
  Result := 'Jupiter.FormDesktopAppScript';
end;

procedure TJupiterFormDesktopAppScript.DoCompile(prSender: TPSScript);
begin
  inherited DoCompile(prSender);

  prSender.AddFunction(@JupiterFormDesktopAppScriptSetCaption, 'procedure Form_SetCaption(prFormID, prCaption : String);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptSetHint, 'procedure Form_SetHint(prFormID, prHint : String);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptJumpLine, 'procedure Form_JumpLine(prFormID : String);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptSetCurrentLine, 'procedure Form_SetCurrentLine(prFormID : String; prLine : Integer);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptSetCurrentMargin, 'procedure Form_SetCurrentMargin(prFormID : String; prMargin : Integer);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptSetCustomInterval, 'procedure Form_SetCustomInterval(prFormID : String; prInterval : Integer);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptSetFormToHighFocus, 'procedure Form_SetFormToHighFocus(prFormID : String);');

  prSender.AddFunction(@JupiterFormDesktopAppScriptAddLink, 'procedure Form_AddLink(prFormID, prCaption, prMacroId: String);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptAddLinkWithParams, 'procedure Form_AddLinkWithParams(prFormID, prCaption, prMacroId, prParam : String);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptAddLinkBoldWithParams, 'procedure Form_AddLinkBoldWithParams(prFormID, prCaption, prMacroId, prParam : String);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptAddLinkWithScript, 'procedure Form_AddLinkWithScript(prFormID, prCaption : String; prMacro : TStrings);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptAddLinkBold, 'procedure Form_AddLinkBold(prFormID, prCaption, prMacroId: String);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptAddLinkBoldWithScript, 'procedure Form_AddLinkBoldWithScript(prFormID, prCaption : String; prMacro : TStrings);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptAddLabel, 'procedure Form_AddLabel(prFormID, prCaption : String);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptAddLabelResultFromMacro, 'procedure Form_AddLabelResultFromMacro(prFormID, prMacroId : String);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptAddLabelBoldResultFromMacro, 'procedure Form_AddLabelBoldResultFromMacro(prFormID, prMacroId : String);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptAddLabelResultFromMacroWithParams, 'procedure Form_AddLabelResultFromMacroWithParams(prFormID, prMacroId, prParam : String);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptAddLabelBoldResultFromMacroWithParams, 'procedure Form_AddLabelBoldResultFromMacroWithParams(prFormID, prMacroId, prParam : String);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptAddLabelBold, 'procedure Form_AddLabelBold(prFormID, prCaption : String);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptAddEdit, 'procedure Form_AddEdit(prFormID, prVariableId, prInitialValue : String);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptAddCombo, 'procedure Form_AddCombo(prFormID, prVariableId, prDataProvider, prColumn: String);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptAddCheckBox, 'procedure Form_AddCheckBox(prFormID, prVariableId, prText: String; prValue: Boolean);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptAddAction, 'procedure Form_AddAction(prFormID, prCaption, prHint: String; prIcon: Integer; prMacroID: String);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptAddActionWithScript, 'procedure Form_AddActionWithScript(prFormID, prCaption, prHint: String; prIcon: Integer; prMacro: TStrings);');
end;

function TJupiterFormDesktopAppScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result:=inherited AnalyseCode;

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_SetCaption(prFormID, prCaption : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_SetHint(prFormID, prHint : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_JumpLine(prFormID : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_SetCurrentMargin(prFormID : String; prMargin : Integer);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_SetCurrentLine(prFormID : String; prLine : Integer);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_SetCustomInterval(prFormID : String; prInterval : Integer);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_SetFormToHighFocus(prFormID : String);'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddLink(prFormID, prCaption, prMacroId: String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddLinkWithParams(prFormID, prCaption, prMacroId, prParam : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddLinkBoldWithParams(prFormID, prCaption, prMacroId, prParam : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddLinkWithScript(prFormID, prCaption : String; prMacro : TStrings);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddLinkBold(prFormID, prCaption, prMacroId: String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddLinkBoldWithScript(prFormID, prCaption : String; prMacro : TStrings);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddLabel(prFormID, prCaption : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddLabelResultFromMacro(prFormID, prMacroId : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddLabelBoldResultFromMacro(prFormID, prMacroId : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddLabelResultFromMacroWithParams(prFormID, prMacroId, prParam : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddLabelBoldResultFromMacroWithParams(prFormID, prMacroId, prParam : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddLabelBold(prFormID, prCaption : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddEdit(prFormID, prVariableId, prInitialValue : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddCombo(prFormID, prVariableId, prDataProvider, prColumn : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddCheckBox(prFormID, prVariableId, prText: String; prValue: Boolean);'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddAction(prFormID, prCaption, prHint: String; prIcon: Integer; prMacroID: String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddActionWithScript(prFormID, prCaption, prHint: String; prIcon: Integer; prMacro: TStrings);'));
end;

end.

