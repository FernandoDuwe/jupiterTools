unit uSQLEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, ExtCtrls,
  ComCtrls, StdCtrls, DBCtrls, ValEdit, SynEdit, SynHighlighterSQL,
  SynCompletion, uJupiterForm, jupiterformutils, JupiterConsts,
  JupiterEnviroment, jupiterDatabaseWizard, JupiterApp, JupiterVariable,
  uJupiterRunnableScript, uJupiterAppScript, uJupiterAction, uMain,
  uJupiterDesktopAppScript, SQLDB, DB, jupiterDatabaseAutoComplete,
  jupiterthread, JupiterCSVDataProvider, jupitersqldataprovider, Types, LCLType;

type

  { TFSQLEditor }

  TFSQLEditor = class(TFJupiterForm)
    dbGridQueryResult: TDBGrid;
    dbNav: TDBNavigator;
    dsQuery: TDataSource;
    InternalQuery: TSQLQuery;
    mmColumns: TMemo;
    mmField: TDBMemo;
    pcBottom: TPageControl;
    pnGrid: TPanel;
    pnBody: TPanel;
    pnLeft: TPanel;
    pnNav: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SynAutoComplete1: TSynAutoComplete;
    SynCompletion1: TSynCompletion;
    SynEdit1: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    tsBottom: TTabSheet;
    tsCampos: TTabSheet;
    tsColumns: TTabSheet;
    tsTexto: TTabSheet;
    tvLibrary: TTreeView;
    vlFields: TValueListEditor;
    procedure dbGridQueryResultCellClick(Column: TColumn);
    procedure dsQueryDataChange(Sender: TObject; Field: TField);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure spDivisorMoved(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
    procedure SynCompletion1BeforeExecute(ASender: TSynBaseCompletion; var ACurrentString: String; var APosition: Integer; var AnX, AnY: Integer; var AnResult: TOnBeforeExeucteFlags);
    procedure SynCompletion1CodeCompletion(var Value: string;
      SourceValue: string; var SourceStart, SourceEnd: TPoint;
      KeyChar: TUTF8Char; Shift: TShiftState);
    procedure SynEdit1Change(Sender: TObject);
    procedure tvLibraryDblClick(Sender: TObject);
  private
    FWizard : TJupiterDatabaseWizard;
    FTableList : TStrings;
    FDatabaseAuto : TJupiterDatabaseAutoComplete;

    FShowResults : Boolean;

    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_PrepareForm; override;

    procedure Internal_OnRunQuery(Sender: TObject);
    procedure Internal_OnRunScript(Sender: TObject);
    procedure Internal_OnCSVExport(Sender: TObject);
    procedure Internal_OnSandwich(Sender: TObject);
    procedure Internal_OnDataProvider(Sender: TObject);

    function Internal_GetText : String;

    procedure Internal_GetFieldFromTable(prTable : String; prTreeOwner : TTreeNode);
    procedure Internal_GetFieldFromTableToAutoComplete(prTable, prAlias : String);
  protected
    function Internal_GetConnection  : TSQLConnection; virtual;
    function Internal_GetTransaction : TSQLTransaction; virtual;

    function Internal_RenderFields : Boolean; virtual;

    function Internal_GetLastEditedFile : String; virtual;

    procedure Internal_OnExecuted(prId, prThreadId : Integer; prParams : String);
  public

  end;

  { TJupiterGetTableThread }


  TJupiterGetTableThread = class (TJupiterThread)
  public
    TableList : TStrings;
    Connection : TSQLConnection;
  protected
    procedure Internal_Execute; override;

    constructor Create(CreateSuspended : Boolean); override;
    destructor Destroy; override;
  end;


var
  FSQLEditor: TFSQLEditor;

implementation

{$R *.lfm}

{ TFSQLEditor }

procedure TFSQLEditor.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FTableList := TStringList.Create;

  Self.FWizard := vrJupiterApp.NewWizard;

  Self.FWizard.Connection  := Self.Internal_GetConnection;
  Self.FWizard.Transaction := Self.Internal_GetTransaction;

  InternalQuery.DataBase := Self.Internal_GetConnection;
  InternalQuery.Transaction := Self.Internal_GetTransaction;

  Self.FDatabaseAuto := TJupiterDatabaseAutoComplete.Create(Self.Internal_GetConnection);
end;

procedure TFSQLEditor.dbGridQueryResultCellClick(Column: TColumn);
begin
  if Column.Field is TBlobField then
  begin
    mmField.DataField := Column.Field.FieldName;

    tsTexto.TabVisible := True;
    tsTexto.Caption    := 'Texto: ' + Column.Field.FieldName;
  end
  else
    tsTexto.TabVisible := False;
end;

procedure TFSQLEditor.dsQueryDataChange(Sender: TObject; Field: TField);
var
  vrVez : Integer;
  vrValue : String;
begin
  vlFields.Strings.Clear;

  if InternalQuery.IsEmpty then
    Exit;

  for vrVez := 0 to InternalQuery.Fields.Count - 1 do
  begin
    vrValue := EmptyStr;

    if InternalQuery.Fields[vrVez].IsNull then
      vrValue := 'NULL'
    else
      vrValue := InternalQuery.Fields[vrVez].AsString;

    vlFields.Strings.Add(InternalQuery.Fields[vrVez].FieldName + '=' + vrValue);
  end;
end;

procedure TFSQLEditor.FormDestroy(Sender: TObject);
begin
  inherited;

  Self.FTableList.Clear;
  FreeAndNil(Self.FTableList);

  FreeAndNil(Self.FWizard);
  FreeAndNil(Self.FDatabaseAuto);
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

procedure TFSQLEditor.SynCompletion1BeforeExecute(ASender: TSynBaseCompletion; var ACurrentString: String; var APosition: Integer; var AnX, AnY: Integer; var AnResult: TOnBeforeExeucteFlags);
begin
  SynCompletion1.ItemList.Clear;
  SynCompletion1.ItemList.AddStrings(Self.FDatabaseAuto.GenerateList(SynEdit1.Lines.Text, ACurrentString));
end;

procedure TFSQLEditor.SynCompletion1CodeCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
begin

end;

procedure TFSQLEditor.SynEdit1Change(Sender: TObject);
begin

end;

procedure TFSQLEditor.tvLibraryDblClick(Sender: TObject);
begin
  if not Assigned(tvLibrary.Selected) then
    Exit;

  if tvLibrary.Selected.ImageIndex <> ICON_GRID then
    Exit;

  if tvLibrary.Selected.Count <> 0 then
    Exit;

  Self.Internal_GetFieldFromTable(tvLibrary.Selected.Text, tvLibrary.Selected);
end;

procedure TFSQLEditor.Internal_UpdateComponents;
var
  vrVez : Integer;
begin
  inherited Internal_UpdateComponents;

  tsBottom.Caption := 'Registros (' + IntToStr(InternalQuery.RecordCount) + ')';

  tvLibrary.Images := FMain.ilIconFamily;

  if miLookColumn.Checked then
    pnLeft.Width    := PercentOfScreen(Self.Width, Self.PercentDivisor);

  pnGrid.Visible := Self.FShowResults;

  if Self.FShowResults then
  begin
    Self.ActionGroup.GetActionAtIndex(1).Enable;
    Self.ActionGroup.GetActionAtIndex(4).Enable;
  end
  else
  begin
    Self.ActionGroup.GetActionAtIndex(1).Disable;
    Self.ActionGroup.GetActionAtIndex(4).Disable;
  end;

  vlFields.DefaultColWidth := PercentOfScreen(vlFields.Width, 50);

  if pnGrid.Visible then
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

  vrTableList := TStringList.Create;
  try
    mmColumns.Lines.Clear;

    for vrVez := 0 to dbGridQueryResult.Columns.Count - 1 do
      mmColumns.Lines.Add(dbGridQueryResult.Columns[vrVez].FieldName);

    vrTableList.Clear;

    Self.FDatabaseAuto.VariableList.Clear;

    SynAutoComplete1.AutoCompleteList.Clear;

    vrTableList.AddStrings(Self.FTableList);

    SynAutoComplete1.AutoCompleteList.AddStrings(vrTableList);

    for vrVez := 0 to vrJupiterApp.Params.Count - 1 do
      SynAutoComplete1.AutoCompleteList.Add('{' + vrJupiterApp.Params.VariableByIndex(vrVez).ID + '}');

    for vrVez := 0 to vrJupiterApp.Params.ChildList.Count - 1 do
      for vrVez2 := 0 to TJupiterVariableList(vrJupiterApp.Params.ChildList.GetAtIndex(vrVez)).Count - 1 do
      begin
        vrStr := TJupiterVariableList(vrJupiterApp.Params.ChildList.GetAtIndex(vrVez)).VariableByIndex(vrVez2).ID;

        SynCompletion1.ItemList.Add('{' + vrStr + '}');

        Self.FDatabaseAuto.VariableList.Add(vrStr);
      end;

    if tvLibrary.Items.Count = 0 then
    begin
      for vrVez := 0 to vrTableList.Count - 1 do
      begin
        if Trim(vrTableList[vrVez]) = EmptyStr then
          Continue;

        vrTreeNode := tvLibrary.Items.Add(nil, vrTableList[vrVez]);
        vrTreeNode.ImageIndex := ICON_GRID;
        vrTreeNode.SelectedIndex := ICON_GRID;

        if Self.Internal_RenderFields then
          Self.Internal_GetFieldFromTable(vrTableList[vrVez], vrTreeNode);
      end;
    end;
  finally
    TStringList(SynAutoComplete1.AutoCompleteList).Sort;
    TStringList(SynCompletion1.ItemList).Sort;

    FreeAndNil(vrTableList);
  end;
end;

procedure TFSQLEditor.Internal_PrepareForm;
var
  vrEnviroment : TJupiterEnviroment;
  vrThread : TJupiterGetTableThread;
begin
  inherited Internal_PrepareForm;

  Self.LateralPanel := pnLeft;

  Self.FTableList.Clear;

  vrThread := TJupiterGetTableThread.Create(True);
  vrThread.FreeOnTerminate := True;
  vrThread.Connection := Self.Internal_GetConnection;
  vrThread.OnExecuted := @Internal_OnExecuted;

  Self.ThreadController.AddThread(vrThread);

  pnGrid.Height := PercentOfScreen(Self.Height, 50);

  SynCompletion1.Width := PercentOfScreen(Self.Width, 50);

  Self.FShowResults := False;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Query', 'Clique aqui para executar uma query', ICON_PLAY, @Internal_OnRunQuery));
  Self.ActionGroup.AddAction(TJupiterAction.Create('Para .csv', 'Clique aqui para exportar os dados atuais para .CSV', ICON_DOWN, @Internal_OnCSVExport));
  Self.ActionGroup.AddAction(TJupiterAction.Create('Script', 'Clique aqui para executar um script', ICON_PLAY, @Internal_OnRunScript));
  Self.ActionGroup.AddAction(TJupiterAction.Create('Estrutura', 'Clique aqui para visualizar ou esconder a estrutura de banco de dados', ICON_RECORDS, @Internal_OnSandwich));
  Self.ActionGroup.AddAction(TJupiterAction.Create('DataProvider', 'Clique aqui para gerar um DataProvider a partir dos dados atuais', ICON_TECHFILE, @Internal_OnDataProvider));

  vrEnviroment := TJupiterEnviroment.Create;
  try
    SynEdit1.Lines.Clear;

    if vrEnviroment.Exists(vrEnviroment.FullPath(Self.Internal_GetLastEditedFile)) then
      SynEdit1.Lines.LoadFromFile(vrEnviroment.FullPath(Self.Internal_GetLastEditedFile));
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

    SynEdit1.Lines.SaveToFile(vrEnviroment.FullPath(Self.Internal_GetLastEditedFile));

    try
      InternalQuery.Open;

      pcBottom.TabIndex := 0;

      Self.UpdateForm();
    except
      mmColumns.Lines.Clear;
      mmColumns.Lines.Add('Erro ao executar query:');
      mmColumns.Lines.Add(Exception(ExceptObject).Message);

     pcBottom.TabIndex := 2;
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

procedure TFSQLEditor.Internal_OnSandwich(Sender: TObject);
begin
  pnLeft.Visible := not pnLeft.Visible;
end;

procedure TFSQLEditor.Internal_OnDataProvider(Sender: TObject);
var
  vrDataProvider : TJupiterSQLDataProvider;
begin
  vrDataProvider := TJupiterSQLDataProvider.Create;
  try
    vrDataProvider.ProvideFromQuery(InternalQuery);
  finally
    JupiterAppDesktopShowMessage('DataProvider: ' + vrDataProvider.ProviderID);
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
      vrTreeNode.ImageIndex := ICON_EDIT;
      vrTreeNode.SelectedIndex := ICON_EDIT;
    end;
  finally
    FreeAndNil(vrFieldList);
  end;
end;

procedure TFSQLEditor.Internal_GetFieldFromTableToAutoComplete(prTable, prAlias : String);
var
  vrFieldList : TStrings;
  vrVez : Integer;
  vrField : String;
begin
  vrFieldList := TStringList.Create;
  try
    vrFieldList.Clear;

    Self.FWizard.Connection.GetFieldNames(prTable, vrFieldList);

    for vrVez := 0 to vrFieldList.Count - 1 do
    begin
      vrField := prAlias + '.' + vrFieldList[vrVez];

      if SynAutoComplete1.AutoCompleteList.IndexOf(vrField) = NULL_KEY then
        SynAutoComplete1.AutoCompleteList.Add(vrField);

      if SynCompletion1.ItemList.IndexOf(vrField) = NULL_KEY then
        SynCompletion1.ItemList.Add(vrField);
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

function TFSQLEditor.Internal_RenderFields: Boolean;
begin
  Result := False;
end;

function TFSQLEditor.Internal_GetLastEditedFile: String;
begin
  Result := '/temp/sql.sql';
end;

procedure TFSQLEditor.Internal_OnExecuted(prId, prThreadId: Integer; prParams: String);
begin
  Self.FTableList.AddStrings(TJupiterGetTableThread(Self.ThreadController.ThreadByD(prId)).TableList);

  Self.FDatabaseAuto.TableList.Clear;
  Self.FDatabaseAuto.TableList.AddStrings(Self.FTableList);
end;

{ TJupiterGetTableThread }

procedure TJupiterGetTableThread.Internal_Execute;
begin
  inherited Internal_Execute;

  Self.Connection.GetTableNames(Self.TableList);
end;

constructor TJupiterGetTableThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);

  Self.TableList := TStringList.Create;
  Self.TableList.Clear;
end;

destructor TJupiterGetTableThread.Destroy;
begin
  Self.TableList.Clear;
  FreeAndNil(Self.TableList);

  inherited Destroy;
end;

end.

