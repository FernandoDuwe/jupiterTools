unit uJupiterDesktopAppScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, jupiterScript, JupiterConsts, JupiterApp,
  uCustomDatabaseForm, SysUtils, PascalScript, uPSComponent, Forms, SQLDB;

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
  procedure JupiterAppDesktopOpenFormQuery(prQuery : TSQLQuery);
  procedure JupiterAppDesktopOpenFormFromTableId(prTableName : String; prID : Integer);
  procedure JupiterAppDesktopClose;

implementation

uses uJupiterForm, uMain, jupiterDesktopApp, jupiterDatabaseWizard;

procedure JupiterAppDesktopOpenForm(prForm: String);
begin
  TJupiterDesktopApp(vrJupiterApp).OpenForm(prForm);
end;

procedure JupiterAppDesktopOpenFormQuery(prQuery: TSQLQuery);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).NewFormByRoute(CUSTOMDATABASE_PATH);

  TFCustomDatabaseForm(vrForm).QueryOrigin := prQuery;

  TJupiterDesktopApp(vrJupiterApp).OpenForm(vrForm as TFCustomDatabaseForm);
end;

procedure JupiterAppDesktopOpenFormFromTableId(prTableName: String; prID: Integer);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).NewFormByRoute(CUSTOMDATABASE_PATH);

  TFCustomDatabaseForm(vrForm).FromReference(TJupiterDatabaseReference.Create(prTableName, prID));

  TJupiterDesktopApp(vrJupiterApp).OpenForm(vrForm as TFCustomDatabaseForm);
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

