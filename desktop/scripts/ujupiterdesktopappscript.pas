unit uJupiterDesktopAppScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, jupiterScript, JupiterConsts, JupiterApp,
  SysUtils, PascalScript, uPSComponent, Forms;

type

  { TJupiterDesktopAppScript }

  TJupiterDesktopAppScript = class(TJupiterScriptLibrary)
  protected
    function Internal_GetName : String; override;
  public
    procedure DoCompile(prSender: TPSScript); override;
    function AnalyseCode: TJupiterScriptAnalyserList; override;
  end;

  procedure JupiterAppDesktopOpenForm(prForm : String);
  procedure JupiterAppDesktopClose;

implementation

uses uJupiterForm, uMain, jupiterDesktopApp;

procedure JupiterAppDesktopOpenForm(prForm: String);
begin
  TJupiterDesktopApp(vrJupiterApp).OpenForm(prForm);
end;

procedure JupiterAppDesktopClose;
begin
  Application.Terminate;
end;

{ TJupiterDesktopAppScript }

function TJupiterDesktopAppScript.Internal_GetName: String;
begin
  Result := 'Jupiter.AppDesktopScript';
end;

procedure TJupiterDesktopAppScript.DoCompile(prSender: TPSScript);
begin
  prSender.AddFunction(@JupiterAppDesktopOpenForm, 'procedure OpenForm(Form: String);');
  prSender.AddFunction(@JupiterAppDesktopClose, 'procedure CloseApp();');
end;

function TJupiterDesktopAppScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure OpenForm(Form: String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure CloseApp();'));
end;

end.

