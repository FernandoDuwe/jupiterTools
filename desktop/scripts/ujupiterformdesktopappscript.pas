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
  procedure JupiterFormDesktopAppScriptAddLink(prFormID, prCaption, prMacroId : String);
  procedure JupiterFormDesktopAppScriptAddLinkWithScript(prFormID, prCaption : String; prMacro : TStrings);
  procedure JupiterFormDesktopAppScriptAddEdit(prFormID, prVariableId, prInitialValue : String);
  procedure JupiterFormDesktopAppScriptAddCombo(prFormID, prVariableId, prDataProvider, prColumn : String);
  procedure JupiterFormDesktopAppScriptAddCheckBox(prFormID, prVariableId, prText: String; prValue: Boolean);
  procedure JupiterFormDesktopAppScriptAddAction(prFormID, prCaption, prHint : String; prIcon : Integer; prMacroID : String);
  procedure JupiterFormDesktopAppScriptAddActionWithScript(prFormID, prCaption, prHint : String; prIcon : Integer; prMacro : TStrings);
  procedure JupiterFormDesktopAppScriptJumpLine(prFormID : String);

implementation

uses uCustomCodeForm, jupiterDesktopApp;

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

  if not (vrForm is TFCustomCodeForm) then
    Exit;

  vrForm.Hint := prHint;
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

  prSender.AddFunction(@JupiterFormDesktopAppScriptAddLink, 'procedure Form_AddLink(prFormID, prCaption, prMacroId: String);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptAddLinkWithScript, 'procedure Form_AddLinkWithScript(prFormID, prCaption : String; prMacro : TStrings);');
  prSender.AddFunction(@JupiterFormDesktopAppScriptAddLabel, 'procedure Form_AddLabel(prFormID, prCaption : String);');
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

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddLink(prFormID, prCaption, prMacroId: String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddLinkWithScript(prFormID, prCaption : String; prMacro : TStrings);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddLabel(prFormID, prCaption : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddEdit(prFormID, prVariableId, prInitialValue : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddCombo(prFormID, prVariableId, prDataProvider, prColumn : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddCheckBox(prFormID, prVariableId, prText: String; prValue: Boolean);'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddAction(prFormID, prCaption, prHint: String; prIcon: Integer; prMacroID: String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddActionWithScript(prFormID, prCaption, prHint: String; prIcon: Integer; prMacro: TStrings);'));
end;

end.

