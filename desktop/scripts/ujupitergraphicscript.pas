unit uJupiterGraphicScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, jupiterScript, JupiterApp, JupiterConsts, PascalScript,
  uPSComponent;

type

  { TJupiterGraphicScript }

  TJupiterGraphicScript = class(TJupiterScriptLibrary)
  protected
    function Internal_GetName : String; override;
  public
    procedure DoCompile(prSender: TPSScript); override;
    function AnalyseCode: TJupiterScriptAnalyserList; override;
  end;

  procedure JupiterGraphicScript_DrawLine(prFormId : String; prX, prY : Integer);
  procedure JupiterGraphicScript_DrawLineTo(prFormId : String; prX1, prX2, prY1, prY2 : Integer);

implementation

uses jupiterDesktopApp, Forms, uPaintForm;

procedure JupiterGraphicScript_DrawLine(prFormId: String; prX, prY: Integer);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFPaintForm) then
    Exit;

  TFPaintForm(vrForm).DrawLine(prX, prY);
end;

procedure JupiterGraphicScript_DrawLineTo(prFormId: String; prX1, prX2, prY1, prY2: Integer);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if not (vrForm is TFPaintForm) then
    Exit;

  TFPaintForm(vrForm).DrawLineTo(prX1, prX2, prY1, prY2);
end;

{ TJupiterGraphicScript }

function TJupiterGraphicScript.Internal_GetName: String;
begin
  Result := 'Jupiter.Graphic';
end;

procedure TJupiterGraphicScript.DoCompile(prSender: TPSScript);
begin
  inherited DoCompile(prSender);

  prSender.AddFunction(@JupiterGraphicScript_DrawLine, 'procedure DrawLine(prFormId : String; prX, prY : Integer);');
  prSender.AddFunction(@JupiterGraphicScript_DrawLineTo, 'procedure DrawLineTo(prFormId: String; prX1, prX2, prY1, prY2: Integer);');
end;

function TJupiterGraphicScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result := inherited AnalyseCode;

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure DrawLine(prFormId : String; prX, prY : Integer);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure DrawLineTo(prFormId: String; prX1, prX2, prY1, prY2: Integer);'));
end;

end.

