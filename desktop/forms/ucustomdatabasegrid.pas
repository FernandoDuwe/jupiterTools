unit ucustomdatabasegrid;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, uJupiterForm,
  jupiterDatabaseWizard, JupiterApp, jupiterStringUtils, jupiterformutils,
  JupiterConsts, JupiterVariable, uJupiterAction, DB, SQLDB;

type

  { TFCustomDatabaseGrid }

  TFCustomDatabaseGrid = class(TFJupiterForm)
    InternalDataSource: TDataSource;
    dbMainGrid: TDBGrid;
    InternalQuery: TSQLQuery;
    procedure dbMainGridColEnter(Sender: TObject);
    procedure dbMainGridDblClick(Sender: TObject);
    procedure dbMainGridEnter(Sender: TObject);
    procedure dbMainGridExit(Sender: TObject);
    procedure edSearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FReference : TJupiterDatabaseReference;

    procedure Internal_UpdateComponents; override;
    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateDatasets; override;

    procedure Internal_OnNew(Sender: TObject);

    function Internal_OnRequestData :  TJupiterVariableList; override;
  public
    procedure FromReference(prReference : TJupiterDatabaseReference);
  end;

var
  FCustomDatabaseGrid: TFCustomDatabaseGrid;

implementation

uses uJupiterDesktopAppScript;

{$R *.lfm}

{ TFCustomDatabaseGrid }

procedure TFCustomDatabaseGrid.FormCreate(Sender: TObject);
var
  vrWizard : TJupiterDatabaseWizard;
begin
  inherited;

  vrWizard := vrJupiterApp.NewWizard;
  try
    InternalQuery.DataBase    := vrWizard.Connection;
    InternalQuery.Transaction := vrWizard.Transaction;
  finally
    FreeAndNil(vrWizard);
  end;
end;

procedure TFCustomDatabaseGrid.dbMainGridDblClick(Sender: TObject);
begin
  if InternalQuery.EOF then
    Exit;

  JupiterAppDesktopOpenFormFromTableId(Self.FReference.TableName, InternalQuery.FieldByName('ID').AsInteger);
end;

procedure TFCustomDatabaseGrid.dbMainGridEnter(Sender: TObject);
begin
  Self.UpdateForm(False);
end;

procedure TFCustomDatabaseGrid.dbMainGridExit(Sender: TObject);
begin
  Self.UpdateForm(False);
end;

procedure TFCustomDatabaseGrid.edSearchChange(Sender: TObject);
begin
  //
end;

procedure TFCustomDatabaseGrid.dbMainGridColEnter(Sender: TObject);
begin
  Self.UpdateForm(False);
end;

procedure TFCustomDatabaseGrid.Internal_UpdateComponents;
var
  vrVez : Integer;
begin
  inherited Internal_UpdateComponents;

  for vrVez := 0 to dbMainGrid.Columns.Count - 1 do
  begin
    dbMainGrid.Columns[vrVez].Visible := dbMainGrid.Columns[vrVez].FieldName <> 'ID';
    dbMainGrid.Columns[vrVez].Title.Caption := JupiterStringUtilsNormalizeToPresent(dbMainGrid.Columns[vrVez].FieldName);
    dbMainGrid.Columns[vrVez].Width := PercentOfScreen(dbMainGrid.Width, 40);
  end;
end;

procedure TFCustomDatabaseGrid.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.ShowSearchBar := True;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Novo', 'Clique aqui para criar um novo registro', ICON_NEW, @Internal_OnNew));

  Self.ActionGroup.TableName := Self.FReference.TableName;

  if not Self.Params.Exists('where') then
    Self.Params.AddVariable('where', EmptyStr, 'Where');

  if not Self.Params.Exists('orderBy') then
    Self.Params.AddVariable('orderBy', EmptyStr, 'Order By');
end;

procedure TFCustomDatabaseGrid.Internal_UpdateDatasets;
var
  vrId : Integer;
  vrWizard : TJupiterDatabaseWizard;
  vrStringList : TStrings;
  vrVez : Integer;
begin
  inherited Internal_UpdateDatasets;

  vrId := NULL_KEY;

  vrWizard := vrJupiterApp.NewWizard;
  try
    if InternalQuery.Active then
      if ((not InternalQuery.EOF) and (not InternalQuery.FieldByName('ID').IsNull)) then
        vrId := InternalQuery.FieldByName('ID').AsInteger;

    vrStringList := CreateStringList('');

    for vrVez := 0 to InternalQuery.Fields.Count - 1 do
      if InternalQuery.Fields[vrVez] is TStringField then
        vrStringList.Add(InternalQuery.Fields[vrVez].FieldName);

    // Não existem campos possíveis para fazer a pesquisa
    if vrStringList.Count = 0 then
      Self.ShowSearchBar := False;

    InternalQuery.Close;
    InternalQuery.SQL.Clear;

    if edSearch.Text = EmptyStr then
      InternalQuery.SQL.AddStrings(vrWizard.NewQueryFromReference(Self.FReference, Self.Params.VariableById('where').Value, Self.Params.VariableById('orderBy').Value).SQL)
    else
    begin
      if vrStringList.Count > 0 then
        InternalQuery.SQL.AddStrings(vrWizard.NewQueryFromReferenceWithSearch(Self.FReference, vrStringList, edSearch.Text, Self.Params.VariableById('where').Value, Self.Params.VariableById('orderBy').Value).SQL)
      else
        InternalQuery.SQL.AddStrings(vrWizard.NewQueryFromReference(Self.FReference, Self.Params.VariableById('where').Value, Self.Params.VariableById('orderBy').Value).SQL)
    end;

    InternalQuery.Open;

    if vrId <> NULL_KEY then
      InternalQuery.Locate('ID', vrId,[]);
  finally
    FreeAndNil(vrWizard);

    Self.Hint := 'Tabela: ' + Self.FReference.TableName + '   Total de registros em tela: ' + IntToStr(InternalQuery.RecordCount);

    if ((Self.Params.Exists('where')) and (not Self.Params.VariableById('where').IsEmpty)) then
      Self.Hint := Self.Hint + '. Existem filtros aplicados nesta consulta';


  //  FreeAndNil(vrStringList);
  end;
end;

procedure TFCustomDatabaseGrid.Internal_OnNew(Sender: TObject);
begin
  JupiterAppDesktopOpenFormFromTableId(Self.FReference.TableName, NULL_KEY);
end;

function TFCustomDatabaseGrid.Internal_OnRequestData: TJupiterVariableList;
var
  vrVez : Integer;
begin
  Result := inherited Internal_OnRequestData;
end;

procedure TFCustomDatabaseGrid.FromReference(prReference: TJupiterDatabaseReference);
var
  vrWizard : TJupiterDatabaseWizard;
begin
  inherited;

  if not Self.Params.Exists('where') then
    Self.Params.AddVariable('where', EmptyStr, 'Where');

  if not Self.Params.Exists('orderBy') then
    Self.Params.AddVariable('orderBy', EmptyStr, 'Order By');

  Self.FReference := prReference;

  Self.Caption := JupiterStringUtilsNormalizeToPresent(prReference.TableName);

  vrWizard := vrJupiterApp.NewWizard;
  try
    InternalQuery.Close;
    InternalQuery.SQL.Clear;
    InternalQuery.SQL.AddStrings(vrWizard.NewQueryFromReference(prReference, Self.Params.VariableById('where').Value, Self.Params.VariableById('orderBy').Value).SQL);
    InternalQuery.Open;
    InternalQuery.First;
  finally
    FreeAndNil(vrWizard);
  end;
end;

end.

