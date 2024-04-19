unit usqlEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  DBGrids, StdCtrls, Menus, SynEdit, SynHighlighterSQL, SynCompletion,
  JupiterForm, jupiterformutils, JupiterAction, JupiterConsts,
  JupiterEnviroment, JupiterApp, JupiterSystemMessage, JupiterDialogForm,
  jupiterautocompletesql, udm, jupiterdatabase, SQLDB, DB;

type

  { TFSQLEditor }

  TFSQLEditor = class(TFJupiterForm)
    dsQuery: TDataSource;
    dbGridResult: TDBGrid;
    lbTables: TListBox;
    lbFields: TListBox;
    miUpperCase: TMenuItem;
    miNickNameTable: TMenuItem;
    Separator1: TMenuItem;
    mmMessages: TMemo;
    pcPages: TPageControl;
    pnFooter: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    sqlQuery: TSQLQuery;
    sqlScript: TSQLScript;
    sqlTransaction: TSQLTransaction;
    syEditor: TSynEdit;
    SynAutoComplete1: TSynAutoComplete;
    SynCompletion1: TSynCompletion;
    SynSQLSyn1: TSynSQLSyn;
    tsMessages: TTabSheet;
    tsStructure: TTabSheet;
    tsResult: TTabSheet;
    procedure dbGridResultDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbFieldsKeyPress(Sender: TObject; var Key: char);
    procedure lbTablesClick(Sender: TObject);
    procedure syEditorChange(Sender: TObject);
    procedure Internal_ChangeAutoComplete(prNewList : TStrings);
  private
    FAutoComplete : TJupiterAutoCompleteSQL;

    procedure Internal_Executar(Sender : TObject);
    procedure Internal_ExecutarScript(Sender : TObject);
    procedure Internal_Commit(Sender : TObject);
    procedure Internal_Rollback(Sender : TObject);
    procedure Internal_Export(Sender : TObject);

    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
  public
    function GetQueryText : String;
  end;

var
  FSQLEditor: TFSQLEditor;

implementation

{$R *.lfm}

{ TFSQLEditor }

procedure TFSQLEditor.FormDestroy(Sender: TObject);
begin
  if sqlQuery.Active then
    sqlQuery.Close;

  if sqlTransaction.Active then
    sqlTransaction.RollbackRetaining;

  FreeAndNil(Self.FAutoComplete);

  inherited;
end;

procedure TFSQLEditor.lbFieldsKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    syEditor.Lines.Clear;
    syEditor.Lines.Add('SELECT');
    syEditor.Lines.Add('FROM  A');
  end;
end;

procedure TFSQLEditor.dbGridResultDblClick(Sender: TObject);
var
  vrDialog : TJupiterDialogForm;
  vrVez : Integer;
begin
  if not sqlQuery.Active then
    Exit;

  if sqlQuery.EOF then
    Exit;

  vrDialog := TJupiterDialogForm.Create;
  try
    vrDialog.Title := 'Registro';
    vrDialog.OnlyShow := True;

    for vrVez := 0 to sqlQuery.Fields.Count - 1 do
    begin
      if sqlQuery.Fields[vrVez].IsNull then
        vrDialog.Fields.AddField(sqlQuery.Fields[vrVez].FieldName, sqlQuery.Fields[vrVez].FieldName, '<NULL>', False, True)
      else
      begin
        vrDialog.Fields.AddField(sqlQuery.Fields[vrVez].FieldName, sqlQuery.Fields[vrVez].FieldName, sqlQuery.Fields[vrVez].Value, False, True);

        vrDialog.Fields.VariableFormById(sqlQuery.Fields[vrVez].FieldName).CopyButton := True;
      end;
    end;

    vrDialog.Show;
  finally
    FreeAndNil(vrDialog);
  end;
end;

procedure TFSQLEditor.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FAutoComplete := TJupiterAutoCompleteSQL.Create;
end;

procedure TFSQLEditor.lbTablesClick(Sender: TObject);
var
  vrTable : String;
  vrList  : TStrings;
begin
  vrTable := EmptyStr;

  if lbTables.ItemIndex = NULL_KEY then
    Exit;

  vrTable := lbTables.Items[lbTables.ItemIndex];

  vrList := TStringList.Create;
  try
    DMMain.sqlConnector.GetFieldNames(vrTable, vrList);

    lbFields.Items.Clear;
    lbFields.Items.AddStrings(vrList);
  finally
    FreeAndNil(vrList);
  end;
end;

procedure TFSQLEditor.syEditorChange(Sender: TObject);
begin
  Self.FAutoComplete.SetScript(syEditor.Lines);
end;

procedure TFSQLEditor.Internal_ChangeAutoComplete(prNewList: TStrings);
begin
  SynAutoComplete1.AutoCompleteList.Clear;
  SynAutoComplete1.AutoCompleteList.AddStrings(prNewList);

  SynCompletion1.ItemList.Clear;
  SynCompletion1.ItemList.AddStrings(prNewList);
end;

procedure TFSQLEditor.Internal_Executar(Sender: TObject);
var
  vrEnviroment    : TJupiterEnviroment;
  vrMessageSystem : TJupiterSystemMessage;
begin
  sqlQuery.Close;
  sqlQuery.DataBase := DMMain.sqlConnector;
  sqlTransaction.DataBase := DMMain.sqlConnector;

  if not sqlTransaction.Active then
    sqlTransaction.Active := True;

  sqlQuery.SQL.Clear;
  sqlQuery.SQL.Text := Self.GetQueryText;

  mmMessages.Lines.Clear;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    try
      sqlQuery.Prepare;
      sqlQuery.Open;

      vrMessageSystem := vrJupiterApp.AddMessage('SQL executado', SQLEDITOR_FORM_PATH);
      vrMessageSystem.Details.AddStrings(sqlQuery.SQL);

      mmMessages.Lines.Add('Query executada');

      if not Self.Params.Exists('SQL') then
        syEditor.Lines.SaveToFile(vrEnviroment.FullPath('/temp/sqlEditor.sql'));
    except
      mmMessages.Lines.Add('Erro ao executar a query: ');
      mmMessages.Lines.Add(Exception(ExceptObject).Message);
    end;
  finally
    Self.UpdateForm();

    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFSQLEditor.Internal_ExecutarScript(Sender: TObject);
var
  vrEnviroment    : TJupiterEnviroment;
  vrMessageSystem : TJupiterSystemMessage;
begin
  sqlQuery.Close;
  sqlScript.Terminator := TJupiterDatabaseModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Database')).GetTerminator;
  sqlTransaction.DataBase := DMMain.sqlConnector;
  sqlScript.DataBase := DMMain.sqlConnector;

  if not sqlTransaction.Active then
    sqlTransaction.Active := True;

  sqlScript.Script.Clear;
  sqlScript.Script.Text := Self.GetQueryText;

  mmMessages.Lines.Clear;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    try
      sqlScript.ExecuteScript;

      vrMessageSystem := vrJupiterApp.AddMessage('Script executado', SQLEDITOR_FORM_PATH);
      vrMessageSystem.Details.AddStrings(sqlScript.Script);

      mmMessages.Lines.Add('Script executado');

      if not Self.Params.Exists('SQL') then
        syEditor.Lines.SaveToFile(vrEnviroment.FullPath('/temp/scriptEditor.sql'));
    except
      mmMessages.Lines.Add('Erro ao executar script: ');
      mmMessages.Lines.Add(Exception(ExceptObject).Message);
    end;
  finally
    Self.UpdateForm();

    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFSQLEditor.Internal_Commit(Sender: TObject);
begin
  try
    sqlTransaction.CommitRetaining;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFSQLEditor.Internal_Rollback(Sender: TObject);
begin
  try
    sqlTransaction.RollbackRetaining;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFSQLEditor.Internal_Export(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.CreateFile('/temp/export.csv', EmptyStr);

    vrJupiterApp.Popup('Arquivo criado', CreateStringList('Arquivo /temp/export.csv criado'));
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFSQLEditor.Internal_PrepareForm;
var
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Internal_PrepareForm;

  Self.Actions.Add(TJupiterAction.Create('SQL', @Internal_Executar));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Executar a consulta atual ou o texto selecionado';
    Icon := ICON_PLAY;
  end;

  Self.Actions.Add(TJupiterAction.Create('Script', @Internal_ExecutarScript));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Executar a consulta atual ou o texto selecionado, como script';
    Icon := ICON_LIBRARY;
  end;

  Self.Actions.Add(TJupiterAction.Create('Commit', @Internal_Commit));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Confirmar alterações';
    Icon := ICON_SAVE;
  end;

  Self.Actions.Add(TJupiterAction.Create('Rollback', @Internal_Rollback));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Cancelar alterações';
    Icon := ICON_DELETE;
  end;

  Self.Actions.Add(TJupiterAction.Create('Exportar', @Internal_Export));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Exportar resultado atual em .csv';
    Icon := ICON_COPY;
  end;

  pnFooter.Height := PercentOfScreen(Self.Height, 30);

  syEditor.Lines.Clear;
  mmMessages.Lines.Clear;

  if Self.Params.Exists('SQL') then
    syEditor.Lines.Text := Self.Params.VariableById('SQL').Value
  else
  begin
    vrEnviroment := TJupiterEnviroment.Create;
    try
      if vrEnviroment.Exists(vrEnviroment.FullPath('/temp/sqlEditor.sql')) then
        syEditor.Lines.LoadFromFile(vrEnviroment.FullPath('/temp/sqlEditor.sql'));
    finally
      FreeAndNil(vrEnviroment);
    end;
  end;
end;

procedure TFSQLEditor.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if sqlQuery.Active then
    pcPages.ActivePageIndex := 0
  else
    pcPages.ActivePageIndex := 1;

  SynCompletion1.Width := PercentOfScreen(syEditor.Width, 50);
  lbTables.Width       := PercentOfScreen(tsStructure.Width, 30);

  syEditor.Font.Size     := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);
  dbGridResult.Font.Size := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);
  mmMessages.Font.Size   := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);
  lbTables.Font.Size     := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);
  lbFields.Font.Size     := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);

  if Self.Actions.Count > 2 then
  begin
    Self.Actions.GetActionButton(2, sbActions).Enabled := sqlTransaction.Active;
    Self.Actions.GetMenuItem(2).Enabled := sqlTransaction.Active;
  end;

  if Self.Actions.Count > 3 then
  begin
    Self.Actions.GetActionButton(3, sbActions).Enabled := sqlTransaction.Active;
    Self.Actions.GetMenuItem(3).Enabled := sqlTransaction.Active;
  end;
end;

procedure TFSQLEditor.Internal_UpdateDatasets;
var
  vrList : TStrings;
begin
  inherited Internal_UpdateDatasets;

  vrList := TStringList.Create;
  try
    DMMain.sqlConnector.GetTableNames(vrList, False);

    SynAutoComplete1.AutoCompleteList.Clear;
    SynAutoComplete1.AutoCompleteList.AddStrings(vrList);

    SynCompletion1.ItemList.Clear;
    SynCompletion1.ItemList.AddStrings(vrList);

    lbTables.Items.Clear;
    lbTables.Items.AddStrings(vrList);
  finally
    FreeAndNil(vrList);
  end;
end;

function TFSQLEditor.GetQueryText: String;
begin
  if Trim(syEditor.SelText) <> EmptyStr then
  begin
    Result := syEditor.SelText;
    Exit;
  end;

  Result := syEditor.Lines.Text;
end;

end.

