unit uJupiterDesktopAppScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, jupiterScript, JupiterConsts, JupiterApp, JupiterVariable,
  uCustomDatabaseForm, ucustomdatabasegrid, SysUtils, PascalScript,
  uPSComponent, Forms, SQLDB;

type

  { TJupiterDesktopAppScript }

  TJupiterDesktopAppScript = class(TJupiterScriptLibrary)
  protected
    function Internal_GetName : String; override;
  public
    procedure DoCompile(prSender: TPSScript); override;
    function AnalyseCode: TJupiterScriptAnalyserList; override;
  end;

  function JupiterAppDesktopOpenForm(prForm : String) : String;
  procedure JupiterAppDesktopOpenFormWithParams(prForm, prParams : String);
  procedure JupiterAppDesktopShowMessage(prMessage : String);
  procedure JupiterAppDesktopOpenFormQuery(prQuery : TSQLQuery);
  procedure JupiterAppDesktopOpenFormFromTableId(prTableName : String; prID : Integer);
  procedure JupiterAppDesktopOpenGridFromTable(prTableName : String);
  procedure JupiterAppDesktopOpenGridFromTableWithWhere(prTableName : String; prWhere : String; prOrderBy : String);
  procedure JupiterAppDesktopOpenFileExplorerForm(prPath : String);
  procedure JupiterAppDesktopCloseForm(prFormID : String);
  procedure JupiterAppDesktopUpdateForms;
  procedure JupiterAppDesktopIncFont;
  procedure JupiterAppDesktopDecFont;
  procedure JupiterAppDesktopClose;

implementation

uses uJupiterForm, uMain, jupiterDesktopApp, jupiterDatabaseWizard;

function JupiterAppDesktopOpenForm(prForm: String) : String;
begin
  Result := TJupiterDesktopApp(vrJupiterApp).OpenForm(prForm, EmptyStr);
end;

procedure JupiterAppDesktopOpenFormWithParams(prForm, prParams: String);
begin
  TJupiterDesktopApp(vrJupiterApp).OpenForm(prForm, prParams);
end;

procedure JupiterAppDesktopShowMessage(prMessage: String);
begin
  Application.MessageBox(PAnsiChar(prMessage), 'Aviso');
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

procedure JupiterAppDesktopOpenGridFromTableWithWhere(prTableName: String; prWhere : String; prOrderBy : String);
var
  vrForm : TForm;
  vrVariables : TJupiterVariableList;
begin
  vrVariables := TJupiterVariableList.Create;
  try
    if prWhere <> '' then
      vrVariables.AddVariable('where', prWhere, 'Where');

    if prOrderBy <> '' then
      vrVariables.AddVariable('orderBy', prOrderBy, 'Order By');

    vrForm := TJupiterDesktopApp(vrJupiterApp).NewFormByRoute(CUSTOMGRIDDATABASE_PATH);

    TFCustomDatabaseGrid(vrForm).Params.CopyValues(vrVariables);
    TFCustomDatabaseGrid(vrForm).FromReference(TJupiterDatabaseReference.Create(prTableName, NULL_KEY));

    TJupiterDesktopApp(vrJupiterApp).OpenForm(vrForm as TFCustomDatabaseGrid);
  finally
    FreeAndNil(vrVariables);
  end;
end;

procedure JupiterAppDesktopOpenGridFromTable(prTableName : String);
begin
  JupiterAppDesktopOpenGridFromTableWithWhere(prTableName, EmptyStr, EmptyStr);
end;

procedure JupiterAppDesktopOpenFileExplorerForm(prPath: String);
var
  vrVariables : TJupiterVariableList;
begin
  vrVariables := TJupiterVariableList.Create;
  try
    vrVariables.AddVariable('path', prPath, 'path');

    TJupiterDesktopApp(vrJupiterApp).OpenForm(FILEEXPLORER_PATH, vrVariables);
  finally
    FreeAndNil(vrVariables);
  end;
end;

procedure JupiterAppDesktopCloseForm(prFormID: String);
var
  vrForm : TForm;
begin
  vrForm := TJupiterDesktopApp(vrJupiterApp).GetFormById(prFormID);

  if not Assigned(vrForm) then
    Exit;

  if (vrForm is TFJupiterForm) then
  begin
    TFJupiterForm(vrForm).DoSecureClose;

    Exit;
  end;

  vrForm.Free;
end;

procedure JupiterAppDesktopUpdateForms;
begin
  if Application.MainForm is TFJupiterForm then
    TFJupiterForm(Application.MainForm).UpdateForm();
end;

procedure JupiterAppDesktopIncFont;
begin
  try
    if vrJupiterApp.Params.Exists(FIELD_FONT_SIZE) then
      vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value := IntToStr(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).AsInteger + 1);
  finally
    JupiterAppDesktopUpdateForms();
  end;
end;

procedure JupiterAppDesktopDecFont;
begin
  try
    if vrJupiterApp.Params.Exists(FIELD_FONT_SIZE) then
      vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value := IntToStr(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).AsInteger - 1);
  finally
    JupiterAppDesktopUpdateForms();
  end;
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
  inherited DoCompile(prSender);

  prSender.AddFunction(@JupiterAppDesktopOpenForm, 'function OpenForm(Form: String) : String;');
  prSender.AddFunction(@JupiterAppDesktopOpenFormWithParams, 'procedure OpenFormWithParams(Form, Params : String);');
  prSender.AddFunction(@JupiterAppDesktopOpenGridFromTable, 'procedure OpenGridFromTable(prTableName : String);');
  prSender.AddFunction(@JupiterAppDesktopOpenGridFromTableWithWhere, 'procedure OpenGridFromTableWithWhere(prTableName : String; prWhere : String; prOrderBy : String);');
  prSender.AddFunction(@JupiterAppDesktopOpenFileExplorerForm, 'procedure OpenFileExplorerForm(prPath : String);');
  prSender.AddFunction(@JupiterAppDesktopCloseForm, 'procedure CloseForm(prFormID : String)');

  prSender.AddFunction(@JupiterAppDesktopClose, 'procedure CloseApp();');
  prSender.AddFunction(@JupiterAppDesktopUpdateForms, 'procedure UpdateForms();');
  prSender.AddFunction(@JupiterAppDesktopIncFont, 'procedure IncFont();');
  prSender.AddFunction(@JupiterAppDesktopDecFont, 'procedure DecFont();');
  prSender.AddFunction(@JupiterAppDesktopShowMessage, 'procedure ShowMessage(prMessage: String);');
end;

function TJupiterDesktopAppScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result := inherited AnalyseCode;

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'function OpenForm(Form: String) : String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure OpenFormWithParams(Form, Params : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure OpenGridFromTable(prTableName : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure OpenGridFromTableWithWhere(prTableName : String; prWhere : String; prOrderBy : String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure OpenFileExplorerForm(prPath: String);'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure CloseForm(prFormID: String) : String;'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure CloseApp();'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure UpdateForms();'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure IncFont();'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure DecFont();'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, 'procedure ShowMessage(prMessage: String);'));
end;

end.

