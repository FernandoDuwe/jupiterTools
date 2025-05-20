unit uSQLEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, ExtCtrls,
  ComCtrls, StdCtrls, SynEdit, SynHighlighterSQL, SynCompletion, uJupiterForm,
  jupiterformutils, JupiterConsts, JupiterEnviroment, jupiterDatabaseWizard,
  JupiterApp, JupiterVariable, uJupiterRunnableScript, uJupiterAppScript,
  uJupiterAction, uMain, SQLDB, DB;

type

  { TFSQLEditor }

  TFSQLEditor = class(TFJupiterForm)
    dbGridQueryResult: TDBGrid;
    dsQuery: TDataSource;
    InternalQuery: TSQLQuery;
    mmColumns: TMemo;
    pcBottom: TPageControl;
    pnBody: TPanel;
    pnLeft: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SynAutoComplete1: TSynAutoComplete;
    SynCompletion1: TSynCompletion;
    SynEdit1: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    tsBottom: TTabSheet;
    tsColumns: TTabSheet;
    tvLibrary: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure spDivisorMoved(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
  private
    FWizard : TJupiterDatabaseWizard;

    FShowResults : Boolean;

    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_PrepareForm; override;

    procedure Internal_OnRunQuery(Sender: TObject);
    procedure Internal_OnRunScript(Sender: TObject);
    procedure Internal_OnCSVExport(Sender: TObject);

    function Internal_GetText : String;

    procedure Internal_GetFieldFromTable(prTable : String; prTreeOwner : TTreeNode);
  protected
    function Internal_GetConnection  : TSQLConnection; virtual;
    function Internal_GetTransaction : TSQLTransaction; virtual;
  public

  end;

var
  FSQLEditor: TFSQLEditor;

implementation

uses LCLType;

{$R *.lfm}

{ TFSQLEditor }

procedure TFSQLEditor.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FWizard := vrJupiterApp.NewWizard;

  InternalQuery.DataBase := Self.FWizard.Connection;
  InternalQuery.Transaction := Self.FWizard.Transaction;
end;

procedure TFSQLEditor.FormDestroy(Sender: TObject);
begin
  inherited;

  FreeAndNil(Self.FWizard);
end;

procedure TFSQLEditor.spDivisorMoved(Sender: TObject);
begin
  try
    miLookColumn.Checked := False;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFSQLEditor.Splitter2Moved(Sender: TObject);
begin

end;

procedure TFSQLEditor.Internal_UpdateComponents;
var
  vrVez : Integer;
begin
  inherited Internal_UpdateComponents;

  tvLibrary.Images := FMain.ilIconFamily;

  if miLookColumn.Checked then
  begin
    pnLeft.Width    := PercentOfScreen(Self.Width, Self.PercentDivisor);;
    pcBottom.Height := PercentOfScreen(Self.Height, Self.PercentDivisor);
  end;

  pcBottom.Visible := Self.FShowResults;

  if Self.FShowResults then
    Self.ActionGroup.GetActionAtIndex(1).Enable
  else
    Self.ActionGroup.GetActionAtIndex(1).Disable;

    if pcBottom.Visible then
      for vrVez := 0 to dbGridQueryResult.Columns.Count - 1 do
        dbGridQueryResult.Columns[vrVez].Width := PercentOfScreen(Self.Width, 20);
end;

procedure TFSQLEditor.Internal_UpdateDatasets;
var
  vrVez :  Integer;
  vrVez2 : Integer;
  vrStr : String;
  vrTreeNode : TTreeNode;
  vrTableList : TStrings;
begin
  inherited Internal_UpdateDatasets;

  Self.FWizard.Connection  := Self.Internal_GetConnection;
  Self.FWizard.Transaction := Self.Internal_GetTransaction;

  vrTableList := TStringList.Create;
  try
    vrTableList.Clear;

    SynAutoComplete1.AutoCompleteList.Clear;
    SynCompletion1.ItemList.Clear;

    Self.FWizard.Connection.GetTableNames(SynAutoComplete1.AutoCompleteList);
    Self.FWizard.Connection.GetTableNames(SynCompletion1.ItemList);
    Self.FWizard.Connection.GetTableNames(vrTableList);

    for vrVez := 0 to vrJupiterApp.Params.Count - 1 do
    begin
      SynAutoComplete1.AutoCompleteList.Add('{' + vrJupiterApp.Params.VariableByIndex(vrVez).ID + '}');
      SynCompletion1.ItemList.Add('{' + vrJupiterApp.Params.VariableByIndex(vrVez).ID + '}');
    end;

    for vrVez := 0 to vrJupiterApp.Params.ChildList.Count - 1 do
      for vrVez2 := 0 to TJupiterVariableList(vrJupiterApp.Params.ChildList.GetAtIndex(vrVez)).Count - 1 do
      begin
        vrStr := TJupiterVariableList(vrJupiterApp.Params.ChildList.GetAtIndex(vrVez)).VariableByIndex(vrVez2).ID;

        SynAutoComplete1.AutoCompleteList.Add('{' + vrStr + '}');
        SynCompletion1.ItemList.Add('{' + vrStr + '}');
      end;

    mmColumns.Lines.Clear;

    for vrVez := 0 to dbGridQueryResult.Columns.Count - 1 do
      mmColumns.Lines.Add(dbGridQueryResult.Columns[vrVez].FieldName);

    if tvLibrary.Items.Count = 0 then
    begin
      for vrVez := 0 to vrTableList.Count - 1 do
      begin
        if Trim(vrTableList[vrVez]) = EmptyStr then
          Continue;

        vrTreeNode := tvLibrary.Items.Add(nil, vrTableList[vrVez]);
        vrTreeNode.ImageIndex := ICON_GRID;
        vrTreeNode.SelectedIndex := ICON_GRID;

        Self.Internal_GetFieldFromTable(vrTableList[vrVez], vrTreeNode);
      end;
    end;
  finally
    FreeAndNil(vrTableList);
  end;
end;

procedure TFSQLEditor.Internal_PrepareForm;
var
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Internal_PrepareForm;

  SynCompletion1.Width := PercentOfScreen(Self.Width, 50);

  Self.FShowResults := False;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Query', 'Clique aqui para executar uma query', ICON_PLAY, @Internal_OnRunQuery));
  Self.ActionGroup.AddAction(TJupiterAction.Create('Para .csv', 'Clique aqui para exportar os dados atuais para .CSV', ICON_DOWN, @Internal_OnCSVExport));
  Self.ActionGroup.AddAction(TJupiterAction.Create('Script', 'Clique aqui para executar um script', ICON_PLAY, @Internal_OnRunScript));

  vrEnviroment := TJupiterEnviroment.Create;
  try
    SynEdit1.Lines.Clear;

    if vrEnviroment.Exists(vrEnviroment.FullPath('/temp/sql.sql')) then
      SynEdit1.Lines.LoadFromFile(vrEnviroment.FullPath('/temp/sql.sql'));
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFSQLEditor.Internal_OnRunQuery(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
begin
  Self.FShowResults := True;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    InternalQuery.Close;
    InternalQuery.SQL.Clear;
    InternalQuery.SQL.Text := JupiterAppScript_ResolveGlobal(Self.Internal_GetText);

    SynEdit1.Lines.SaveToFile(vrEnviroment.FullPath('/temp/sql.sql')); ;

    try
      InternalQuery.Open;

      Self.UpdateForm();
    except
      Application.MessageBox(PAnsiChar('Erro ao executar query: ' + Exception(ExceptObject).Message), PAnsiChar(Self.Caption), MB_ICONERROR + MB_OK);
    end;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFSQLEditor.Internal_OnRunScript(Sender: TObject);
begin
  Self.FShowResults := False;

  try
    Self.FWizard.ExecuteScript(CreateStringList(JupiterAppScript_ResolveGlobal(Self.Internal_GetText)));
  except
    Application.MessageBox(PAnsiChar('Erro ao executar script: ' + Exception(ExceptObject).Message), PAnsiChar(Self.Caption), MB_ICONERROR + MB_OK);
  end;
end;

procedure TFSQLEditor.Internal_OnCSVExport(Sender: TObject);
var
  vrFile : TStrings;
  vrVez : Integer;
  vrLine : String;
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  vrFile := TStringList.Create;
  try
    vrFile.Clear;

    vrLine := EmptyStr;

    for vrVez := 0 to Self.InternalQuery.Fields.Count - 1 do
      vrLine := vrLine + Self.InternalQuery.Fields[vrVez].FieldName + ';';

    vrFile.Add(vrLine);

    Self.InternalQuery.First;

    while not Self.InternalQuery.EOF do
    begin
      vrLine := EmptyStr;

      for vrVez := 0 to Self.InternalQuery.Fields.Count - 1 do
      begin
        if Self.InternalQuery.Fields[vrVez].IsNull then
          vrLine := vrLine + EmptyStr + ';'
        else
          vrLine := vrLine + Self.InternalQuery.Fields[vrVez].AsString + ';';
      end;

      vrFile.Add(vrLine);

      Self.InternalQuery.Next;
    end;

    vrFile.SaveToFile(vrEnviroment.FullPath('/temp/DataExtract.csv'));

    JupiterRunnableScript_RunCommandOnJupiter(vrEnviroment.FullPath('/temp/DataExtract.csv'));
  finally
    vrFile.Clear;
    FreeAndNil(vrFile);
    FreeAndNil(vrEnviroment);
  end;
end;

function TFSQLEditor.Internal_GetText: String;
begin
  Result := SynEdit1.Lines.Text;

  if Trim(SynEdit1.SelText) <> EmptyStr then
    Result := SynEdit1.SelText;
end;

procedure TFSQLEditor.Internal_GetFieldFromTable(prTable: String; prTreeOwner: TTreeNode);
var
  vrFieldList : TStrings;
  vrVez : Integer;
  vrTreeNode : TTreeNode;
begin
  vrFieldList := TStringList.Create;
  try
    vrFieldList.Clear;

    Self.FWizard.Connection.GetFieldNames(prTable, vrFieldList);

    for vrVez := 0 to vrFieldList.Count - 1 do
    begin
      vrTreeNode := tvLibrary.Items.AddChild(prTreeOwner, vrFieldList[vrVez]);
      vrTreeNode.ImageIndex := NULL_KEY;
      vrTreeNode.SelectedIndex := NULL_KEY;
    end;
  finally
    FreeAndNil(vrFieldList);
  end;
end;

function TFSQLEditor.Internal_GetConnection: TSQLConnection;
begin
  Result := vrJupiterApp.InternalDatabase;
end;

function TFSQLEditor.Internal_GetTransaction: TSQLTransaction;
begin
  Result := vrJupiterApp.InternalDatabase.Transaction;
end;

end.

