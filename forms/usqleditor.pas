unit usqlEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  DBGrids, StdCtrls, SynEdit, SynHighlighterSQL, SynCompletion, JupiterForm,
  jupiterformutils, JupiterAction, JupiterConsts, udm, SQLDB, DB;

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
    sqlQuery.Active := False;

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
begin
  sqlQuery.Close;
  sqlQuery.DataBase := DMMain.sqlConnector;
  sqlQuery.Transaction := DMMain.sqlTransaction;
  sqlQuery.SQL.Clear;
  sqlQuery.SQL.Text := Self.GetQueryText;

  mmMessages.Lines.Clear;

  try
    try
      sqlQuery.Prepare;
      sqlQuery.Open;

      mmMessages.Lines.Add('Query executada');
    except
      mmMessages.Lines.Add('Erro ao executar a query: ');
      mmMessages.Lines.Add(Exception(ExceptObject).Message);
    end;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFSQLEditor.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.Actions.Add(TJupiterAction.Create('Executar', @Internal_Executar));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Executar a consulta atual ou o texto selecionado';
    Icon := ICON_PLAY;
  end;

  pnFooter.Height := PercentOfScreen(Self.Height, 30);

  syEditor.Lines.Clear;
  mmMessages.Lines.Clear;
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

