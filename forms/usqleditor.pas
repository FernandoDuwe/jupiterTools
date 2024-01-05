unit usqlEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  DBGrids, StdCtrls, SynEdit, SynHighlighterSQL, SynCompletion, JupiterForm,
  jupiterformutils, JupiterAction, JupiterConsts, JupiterEnviroment, JupiterApp,
  JupiterSystemMessage, udm, jupiterdatabase, SQLDB, DB;

type

  { TFSQLEditor }

  TFSQLEditor = class(TFJupiterForm)
    dsQuery: TDataSource;
    dbGridResult: TDBGrid;
    lbTables: TListBox;
    lbFields: TListBox;
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
    procedure FormDestroy(Sender: TObject);
    procedure lbTablesClick(Sender: TObject);
  private
    procedure Internal_Executar(Sender : TObject);
    procedure Internal_ExecutarScript(Sender : TObject);
    procedure Internal_Commit(Sender : TObject);
    procedure Internal_Rollback(Sender : TObject);

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

  inherited;
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
    Self.Actions.GetActionButton(2, sbActions).Enabled := sqlTransaction.Active;

  if Self.Actions.Count > 3 then
    Self.Actions.GetActionButton(3, sbActions).Enabled := sqlTransaction.Active;
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

