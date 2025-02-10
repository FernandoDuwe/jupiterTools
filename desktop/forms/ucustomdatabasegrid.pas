unit ucustomdatabasegrid;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, uJupiterForm,
  jupiterDatabaseWizard, JupiterApp, jupiterStringUtils, jupiterformutils,
  JupiterConsts, JupiterVariable, uJupiterDatabaseScript, uJupiterAction, DB,
  SQLDB;

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
    procedure edSearchKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
  private
    FLimit : Integer;
    FUseLimit : Boolean;
    FReference : TJupiterDatabaseReference;

    procedure Internal_UpdateComponents; override;
    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateDatasets; override;

    procedure Internal_OnNew(Sender: TObject);
    procedure Internal_OnDelete(Sender: TObject);
    procedure Internal_OnIncLimit(Sender: TObject);
    procedure Internal_OnShowAll(Sender: TObject);

    function Internal_OnRequestData :  TJupiterVariableList; override;
  public
    procedure FromReference(prReference : TJupiterDatabaseReference);
  end;

var
  FCustomDatabaseGrid: TFCustomDatabaseGrid;

implementation

uses uJupiterDesktopAppScript, LCLType;

{$R *.lfm}

{ TFCustomDatabaseGrid }

procedure TFCustomDatabaseGrid.FormCreate(Sender: TObject);
var
  vrWizard : TJupiterDatabaseWizard;
begin
  inherited;

  Self.FUseLimit := True;

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

procedure TFCustomDatabaseGrid.edSearchKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Self.UpdateForm();

    Key := #0;
  end;
end;

procedure TFCustomDatabaseGrid.dbMainGridColEnter(Sender: TObject);
begin
  Self.UpdateForm(False);
end;

procedure TFCustomDatabaseGrid.Internal_UpdateComponents;
var
  vrVez : Integer;
  vrCountVisble : Integer;
begin
  inherited Internal_UpdateComponents;

  vrCountVisble := 0;

  for vrVez := 0 to dbMainGrid.Columns.Count - 1 do
  begin
    dbMainGrid.Columns[vrVez].Visible := dbMainGrid.Columns[vrVez].FieldName <> 'ID';

    if dbMainGrid.Columns[vrVez].Visible then
      if dbMainGrid.Columns[vrVez].Field is TBlobField then
        dbMainGrid.Columns[vrVez].Visible := False;

    if dbMainGrid.Columns[vrVez].Visible then
      vrCountVisble := vrCountVisble + 1;
  end;

  for vrVez := 0 to dbMainGrid.Columns.Count - 1 do
  begin

    dbMainGrid.Columns[vrVez].Title.Caption := JupiterStringUtilsNormalizeToPresent(dbMainGrid.Columns[vrVez].FieldName);

    if vrCountVisble >= 4 then
      dbMainGrid.Columns[vrVez].Width := PercentOfScreen(dbMainGrid.Width, 30)
    else
      dbMainGrid.Columns[vrVez].Width := PercentOfScreen(dbMainGrid.Width, 40);
  end;

  if Self.ActionGroup.Count > 1 then
    if Self.InternalQuery.EOF then
      Self.ActionGroup.GetActionAtIndex(1).Disable
    else
      Self.ActionGroup.GetActionAtIndex(1).Enable;

  if Self.ActionGroup.Count > 2 then
    if not Self.FUseLimit then
      Self.ActionGroup.GetActionAtIndex(2).Disable
    else
      Self.ActionGroup.GetActionAtIndex(2).Enable;
end;

procedure TFCustomDatabaseGrid.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.ShowSearchBar := True;

  Self.FLimit := vrJupiterApp.Params.VariableById(FORM_GRID_LIMIT).AsInteger;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Novo', 'Clique aqui para criar um novo registro', ICON_NEW, @Internal_OnNew));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Excluir', 'Clique aqui para excluir o registro atual', ICON_DELETE, @Internal_OnDelete));

  Self.ActionGroup.AddAction(TJupiterAction.Create(IntToStr(Self.FLimit) + ' registros', 'Clique aqui exibir mais registros', ICON_ADD, @Internal_OnIncLimit));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Todos os registros', 'Clique aqui exibir todos os registros', ICON_VIEW, @Internal_OnShowAll));

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
  vrFields : String;
  vrLimit : String;
begin
  inherited Internal_UpdateDatasets;

  vrFields := '*';
  vrLimit := '';

  if Self.FUseLimit then
    vrLimit := Format(' limit %0:d', [Self.FLimit]);

  vrId := NULL_KEY;

  InternalQuery.DisableControls;

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
      InternalQuery.SQL.AddStrings(vrWizard.NewQueryFromReference(Self.FReference, Self.Params.VariableById('where').Value, Self.Params.VariableById('orderBy').Value, vrFields).SQL)
    else
    begin
      if vrStringList.Count > 0 then
        InternalQuery.SQL.AddStrings(vrWizard.NewQueryFromReferenceWithSearch(Self.FReference, vrStringList, edSearch.Text, Self.Params.VariableById('where').Value, Self.Params.VariableById('orderBy').Value, vrFields).SQL)
      else
        InternalQuery.SQL.AddStrings(vrWizard.NewQueryFromReference(Self.FReference, Self.Params.VariableById('where').Value, Self.Params.VariableById('orderBy').Value, vrFields).SQL)
    end;

    InternalQuery.Open;

    InternalQuery.Last;
    InternalQuery.First;

    if vrId <> NULL_KEY then
      InternalQuery.Locate('ID', vrId,[]);
  finally
    FreeAndNil(vrWizard);

    Self.Hint := 'Tabela: ' + Self.FReference.TableName + '   Total de registros em tela: ' + IntToStr(InternalQuery.RecordCount);

    if ((Self.Params.Exists('where')) and (not Self.Params.VariableById('where').IsEmpty)) then
      Self.Hint := Self.Hint + '. Existem filtros aplicados nesta consulta';

    InternalQuery.EnableControls;
  //  FreeAndNil(vrStringList);
  end;
end;

procedure TFCustomDatabaseGrid.Internal_OnNew(Sender: TObject);
begin
  JupiterAppDesktopOpenFormFromTableId(Self.FReference.TableName, NULL_KEY);
end;

procedure TFCustomDatabaseGrid.Internal_OnDelete(Sender: TObject);
begin
  if Application.MessageBox('Deseja realmente excluir?', 'Confirmar', MB_ICONQUESTION + MB_YESNO) = ID_YES then
  begin
    try
      JupiterDatabaseScript_RunScript(' DELETE FROM  ' + Self.FReference.TableName + ' WHERE ID = ' + Self.InternalQuery.FieldByName('ID').AsString);
    finally
      Self.UpdateForm();
    end;
  end;
end;

procedure TFCustomDatabaseGrid.Internal_OnIncLimit(Sender: TObject);
begin
  try
    Self.FLimit := Self.FLimit + vrJupiterApp.Params.VariableById(FORM_GRID_LIMIT).AsInteger;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFCustomDatabaseGrid.Internal_OnShowAll(Sender: TObject);
begin
  try
    Self.FUseLimit := False;
  finally
    Self.UpdateForm();
  end;
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

