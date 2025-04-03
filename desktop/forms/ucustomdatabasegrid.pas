unit ucustomdatabasegrid;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, ExtCtrls,
  Menus, uJupiterForm, jupiterDatabaseWizard, JupiterApp, jupiterStringUtils,
  jupiterformutils, JupiterConsts, JupiterVariable, uJupiterDatabaseScript,
  uJupiterAction, jupiterformcomponenttils, DB, SQLDB;

type

  { TFCustomDatabaseGrid }

  TFCustomDatabaseGrid = class(TFJupiterForm)
    InternalDataSource: TDataSource;
    dbMainGrid: TDBGrid;
    InternalQuery: TSQLQuery;
    miShowMiniForm: TMenuItem;
    pnMiniForm: TPanel;
    pmActions: TPopupMenu;
    sbMiniForm: TScrollBox;
    Splitter1: TSplitter;
    tmrExecution: TTimer;
    procedure dbMainGridColEnter(Sender: TObject);
    procedure dbMainGridDblClick(Sender: TObject);
    procedure dbMainGridEnter(Sender: TObject);
    procedure dbMainGridExit(Sender: TObject);
    procedure edSearchChange(Sender: TObject);
    procedure edSearchKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure InternalDataSourceDataChange(Sender: TObject; Field: TField);
    procedure miShowMiniFormClick(Sender: TObject);
    procedure tmrExecutionTimer(Sender: TObject);
  private
    FLimit : Integer;
    FUseLimit : Boolean;
    FReference : TJupiterDatabaseReference;

    procedure Internal_UpdateComponents; override;
    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_RenderActions;

    procedure Internal_OnNew(Sender: TObject);
    procedure Internal_OnDelete(Sender: TObject);
    procedure Internal_OnIncLimit(Sender: TObject);
    procedure Internal_OnShowAll(Sender: TObject);
    procedure Internal_RenderMiniForm;
    procedure Internal_ClickRecord(Sender: TObject);
    procedure Internal_ClickOwnerRecord(Sender: TObject);
    procedure Internal_OnLinkClick(Sender : TObject);

    function Internal_OnRequestData :  TJupiterVariableList; override;
    function Internal_OnPopupRequestData :  TJupiterVariableList;
  public
    procedure FromReference(prReference : TJupiterDatabaseReference);
  end;

var
  FCustomDatabaseGrid: TFCustomDatabaseGrid;

implementation

uses LCLType, StdCtrls, uJupiterDesktopAppScript;

{$R *.lfm}

{ TFCustomDatabaseGrid }

procedure TFCustomDatabaseGrid.FormCreate(Sender: TObject);
var
  vrWizard : TJupiterDatabaseWizard;
begin
  inherited;

  Self.FUseLimit := True;

  Self.ActionGroup.PopupMenu := pmActions;
  Self.ActionGroup.OnPopupRequestData := @Internal_OnPopupRequestData;

  vrWizard := vrJupiterApp.NewWizard;
  try
    InternalQuery.DataBase    := vrWizard.Connection;
    InternalQuery.Transaction := vrWizard.Transaction;
  finally
    FreeAndNil(vrWizard);
  end;
end;

procedure TFCustomDatabaseGrid.InternalDataSourceDataChange(Sender: TObject; Field: TField);
begin
  if pnMiniForm.Visible then
    Self.Internal_RenderMiniForm;
end;

procedure TFCustomDatabaseGrid.miShowMiniFormClick(Sender: TObject);
begin
  try
    miShowMiniForm.Checked := not miShowMiniForm.Checked;

    if miShowMiniForm.Checked then
      vrJupiterApp.Params.VariableById('Interface.Grid.ShowMiniForm').Value := BOOL_TRUE_STR
    else
      vrJupiterApp.Params.VariableById('Interface.Grid.ShowMiniForm').Value := BOOL_FALSE_STR;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFCustomDatabaseGrid.tmrExecutionTimer(Sender: TObject);
begin
  vrJupiterApp.RunAction(tmrExecution.Tag, Self.Internal_OnPopupRequestData);

  tmrExecution.Enabled := False;
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

    if vrCountVisble > 5 then
      dbMainGrid.Columns[vrVez].Width := PercentOfScreen(dbMainGrid.Width, 20)
    else
      dbMainGrid.Columns[vrVez].Width := PercentOfScreen(dbMainGrid.Width, Round(100 / vrCountVisble));
  end;

  if Self.ActionGroup.Count > 1 then
  begin
    if (Self.InternalQuery.EOF)  then
    begin
      Self.ActionGroup.GetActionAtIndex(1).Disable;
      Self.ActionGroup.GetActionAtIndex(1).DisablePopup;
    end
    else
    begin
      Self.ActionGroup.GetActionAtIndex(1).Enable;
      Self.ActionGroup.GetActionAtIndex(1).EnablePopup;
    end;
  end;

  if Self.ActionGroup.Count > 2 then
    if ((not Self.FUseLimit)  or (Self.InternalQuery.RecordCount < vrJupiterApp.Params.VariableById(FORM_GRID_LIMIT).AsInteger)) then
    begin
      Self.ActionGroup.GetActionAtIndex(2).Disable;
      Self.ActionGroup.GetActionAtIndex(2).DisablePopup;
    end
    else
    begin
      Self.ActionGroup.GetActionAtIndex(2).Enable;
      Self.ActionGroup.GetActionAtIndex(2).EnablePopup;
    end;

  if Self.ActionGroup.Count >= 3 then
    if (Self.InternalQuery.RecordCount < vrJupiterApp.Params.VariableById(FORM_GRID_LIMIT).AsInteger) then
    begin
      Self.ActionGroup.GetActionAtIndex(3).Disable;
      Self.ActionGroup.GetActionAtIndex(3).DisablePopup;
    end
    else
    begin
      Self.ActionGroup.GetActionAtIndex(3).Enable;
      Self.ActionGroup.GetActionAtIndex(3).EnablePopup;
    end;
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

  miShowMiniForm.Checked := vrJupiterApp.Params.VariableById('Interface.Grid.ShowMiniForm').AsBool;
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
      InternalQuery.SQL.AddStrings(vrWizard.NewQueryFromReference(Self.FReference, Self.Params.VariableById('where').Value, Self.Params.VariableById('orderBy').Value, vrFields, vrLimit).SQL)
    else
    begin
      if vrStringList.Count > 0 then
        InternalQuery.SQL.AddStrings(vrWizard.NewQueryFromReferenceWithSearch(Self.FReference, vrStringList, edSearch.Text, Self.Params.VariableById('where').Value, Self.Params.VariableById('orderBy').Value, vrFields, vrLimit).SQL)
      else
        InternalQuery.SQL.AddStrings(vrWizard.NewQueryFromReference(Self.FReference, Self.Params.VariableById('where').Value, Self.Params.VariableById('orderBy').Value, vrFields, vrLimit).SQL)
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

  pnMiniForm.Visible := miShowMiniForm.Checked;

  if miLookColumn.Checked then
    pnMiniForm.Width := PercentOfScreen(Self.Width, Self.PercentDivisor);

  if pnMiniForm.Visible then
    Self.Internal_RenderMiniForm;
end;

procedure TFCustomDatabaseGrid.Internal_RenderActions;
begin
  //
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

procedure TFCustomDatabaseGrid.Internal_RenderMiniForm;
var
  vrVez : Integer;
  vrCurrentLine : Integer;
  vrReference : TJupiterComponentReference;
  vrReferenceLink : TJupiterComponentReference;
  vrWizard : TJupiterDatabaseWizard;
  vrForeignKey : TJupiterDatabaseForeignKeyReference;
  vrActionCount : Integer;
begin
  vrActionCount := 0;

  pnMiniForm.Caption := 'Selecione um registro para continuar';

  RemoveChildren(sbMiniForm);

  if Self.InternalQuery.EOF then
    Exit;

  vrCurrentLine := FORM_MARGIN_TOP;

  vrWizard := vrJupiterApp.NewWizard;
  try
    vrReference := JupiterComponentsNewLabel('Registro ', TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbMiniForm);

    vrReferenceLink := JupiterComponentsNewLink('#' + Self.InternalQuery.FieldByName('ID').AsString, TJupiterPosition.Create(vrCurrentLine, vrReference.Right), sbMiniForm);

    TLabel(vrReferenceLink.Component).OnClick := @Internal_ClickRecord;
    TLabel(vrReferenceLink.Component).Hint := 'Visualizar o registro atual';

    vrCurrentLine := vrReferenceLink.Bottom + FORM_MARGIN_BOTTOM_TONEXT;

    vrReference := JupiterComponentsNewLabel('Ações de registro', TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbMiniForm);

    TLabel(vrReference.Component).Font.Style := [fsBold];

    vrCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;

    for vrVez := 0 to pmActions.Items.Count - 1 do
    begin
      if pmActions.Items[vrVez].Tag = 0 then
        Continue;

      vrActionCount := vrActionCount + 1;

      if pmActions.Items[vrVez].Enabled then
      begin
        vrReference := JupiterComponentsNewLink(pmActions.Items[vrVez].Caption , TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT + FORM_MARGIN_LEFT), sbMiniForm);

        TLabel(vrReference.Component).Tag := pmActions.Items[vrVez].Tag;
        TLabel(vrReference.Component).OnClick := @Internal_OnLinkClick;
      end
      else
        vrReference := JupiterComponentsNewLabel(pmActions.Items[vrVez].Caption , TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT + FORM_MARGIN_LEFT), sbMiniForm);

      vrCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM_TONEXT;
    end;

    if vrActionCount = 0 then
    begin
      vrReference := JupiterComponentsNewLabel('Nenhuma ação cadastrada', TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT + FORM_MARGIN_LEFT), sbMiniForm);

      TLabel(vrReference.Component).Font.Style := [fsItalic];
    end;

    vrCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM_TONEXT;

    vrReference := JupiterComponentsNewLabel('Campos', TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbMiniForm);

    TLabel(vrReference.Component).Font.Style := [fsBold];

    vrCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;

    for vrVez := 0 to dbMainGrid.Columns.Count - 1 do
    begin
      if not dbMainGrid.Columns[vrVez].Visible then
        Continue;

      vrReference := JupiterComponentsNewLabel(dbMainGrid.Columns[vrVez].Title.Caption, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT + FORM_MARGIN_LEFT), sbMiniForm);

      TLabel(vrReference.Component).Font.Style := [fsBold];

      vrCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;

      if not dbMainGrid.Columns[vrVez].Field.IsNull then
      begin
        vrReference := JupiterComponentsNewLabel(dbMainGrid.Columns[vrVez].Field.AsString + ' ', TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT + FORM_MARGIN_LEFT), sbMiniForm);

        if vrWizard.IsForeignKeyField(Self.FReference.TableName, dbMainGrid.Columns[vrVez].Field.FieldName) then
        begin
          vrCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;

          vrForeignKey := vrWizard.GetForeignKeyData(Self.FReference.TableName, dbMainGrid.Columns[vrVez].Field.FieldName);

          vrReferenceLink := JupiterComponentsNewLink('Ver mais', TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT + FORM_MARGIN_LEFT), sbMiniForm);

          TLabel(vrReferenceLink.Component).OnClick := @Internal_ClickOwnerRecord;
          TLabel(vrReferenceLink.Component).Tag := vrVez;
          TLabel(vrReferenceLink.Component).Hint := 'Acessar o registro pai';

          vrReference := vrReferenceLink;
        end;
      end
      else
      begin
        vrReference := JupiterComponentsNewLabel('Nulo', TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT + FORM_MARGIN_LEFT), sbMiniForm);

        TLabel(vrReference.Component).Font.Style := [fsItalic];
      end;

      vrCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;

      vrCurrentLine := vrCurrentLine + FORM_MARGIN_BOTTOM_TONEXT;
    end;
  finally
    FreeAndNil(vrWizard);

    pnMiniForm.Caption := EmptyStr;
  end;
end;

procedure TFCustomDatabaseGrid.Internal_ClickRecord(Sender: TObject);
begin
  JupiterAppDesktopOpenFormFromTableId(Self.FReference.TableName, Self.InternalQuery.FieldByName('ID').AsInteger);
end;

procedure TFCustomDatabaseGrid.Internal_ClickOwnerRecord(Sender: TObject);
var
  vrForeignKey : TJupiterDatabaseForeignKeyReference;
begin
  vrForeignKey := vrJupiterApp.NewWizard.GetForeignKeyData(Self.FReference.TableName, dbMainGrid.Columns[TLabel(Sender).Tag].FieldName);

  JupiterAppDesktopOpenFormFromTableId(vrForeignKey.TableDestinyName, dbMainGrid.Columns[TLabel(Sender).Tag].Field.AsInteger);
end;

procedure TFCustomDatabaseGrid.Internal_OnLinkClick(Sender: TObject);
var
  vrVez : Integer;
begin
  for vrVez := 0 to pmActions.Items.Count - 1 do
    if TLabel(Sender).Tag = pmActions.Items[vrVez].Tag then
    begin
      tmrExecution.Enabled := False;
      tmrExecution.Tag     := TLabel(Sender).Tag;
      tmrExecution.Enabled := True;

      Exit;
    end;
end;

function TFCustomDatabaseGrid.Internal_OnRequestData: TJupiterVariableList;
begin
  Result := inherited Internal_OnRequestData;
end;

function TFCustomDatabaseGrid.Internal_OnPopupRequestData: TJupiterVariableList;
var
  vrVez : Integer;
begin
  Result := inherited Internal_OnRequestData;

  if not Self.InternalQuery.EOF then
    for vrVez := 0 to Self.InternalQuery.FieldCount - 1 do
      if not Result.Exists(Self.InternalQuery.Fields[vrVez].FieldName) then
        Result.AddVariable(Self.InternalQuery.Fields[vrVez].FieldName, Self.InternalQuery.Fields[vrVez].AsString, EmptyStr);
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

