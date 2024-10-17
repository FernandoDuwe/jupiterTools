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
  procedure JupiterFormDesktopAppScriptAddLabel(prFormID, prCaption : String);

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

{ TJupiterFormDesktopAppScript }

function TJupiterFormDesktopAppScript.Internal_GetName: String;
begin
  Result := 'Jupiter.FormDesktopAppScript';
end;

procedure TJupiterFormDesktopAppScript.DoCompile(prSender: TPSScript);
begin
  inherited DoCompile(prSender);

  prSender.AddFunction(@JupiterFormDesktopAppScriptSetCaption, 'procedure Form_SetCaption(prFormID, prCaption : String);');

  prSender.AddFunction(@JupiterFormDesktopAppScriptAddLabel, 'procedure Form_AddLabel(prFormID, prCaption : String);');
end;

function TJupiterFormDesktopAppScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result:=inherited AnalyseCode;

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_SetCaption(prFormID, prCaption : String);'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure Form_AddLabel(prFormID, prCaption : String);'));
end;

end.

