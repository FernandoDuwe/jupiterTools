unit ucustomdatabasegrid;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, ExtCtrls,
  Menus, uJupiterForm, jupiterDatabaseWizard, JupiterApp, jupiterStringUtils,
  jupiterformutils, JupiterConsts, JupiterVariable, JupiterModule,
  uJupiterDatabaseScript, uJupiterStringUtilsScript, uJupiterAction,
  jupiterformcomponenttils, DB, SQLDB, Grids, ValEdit;

type

  { TFCustomDatabaseGrid }

  TFCustomDatabaseGrid = class(TFJupiterForm)
    InternalDataSource: TDataSource;
    dbMainGrid: TDBGrid;
    InternalQuery: TSQLQuery;
    miExibirColunaID: TMenuItem;
    miShowMiniForm: TMenuItem;
    pnMiniForm: TPanel;
    pmActions: TPopupMenu;
    sbMiniForm: TScrollBox;
    Splitter1: TSplitter;
    tmrExecution: TTimer;
    ValueListEditor1: TValueListEditor;
    procedure dbMainGridColEnter(Sender: TObject);
    procedure dbMainGridDblClick(Sender: TObject);
    procedure dbMainGridDrawColumnTitle(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure dbMainGridEnter(Sender: TObject);
    procedure dbMainGridExit(Sender: TObject);
    procedure dbMainGridGetCellHint(Sender: TObject; Column: TColumn;
      var AText: String);
    procedure dbMainGridTitleClick(Column: TColumn);
    procedure edSearchChange(Sender: TObject);
    procedure edSearchKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure InternalDataSourceDataChange(Sender: TObject; Field: TField);
    procedure InternalQueryCalcFields(DataSet: TDataSet);
    procedure miExibirColunaIDClick(Sender: TObject);
    procedure miShowMiniFormClick(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure tmrExecutionTimer(Sender: TObject);
  private
    FLimit : Integer;
    FUseLimit : Boolean;
    FReference : TJupiterDatabaseReference;

    procedure Internal_UpdateComponents; override;
    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_RenderActions;
    procedure Internal_SetCalculatedFields;
    procedure Internal_OnGetText(Sender: TField; var aText: string; DisplayText: Boolean);

    procedure Internal_OnNew(Sender: TObject);
    procedure Internal_OnDelete(Sender: TObject);
    procedure Internal_OnIncLimit(Sender: TObject);
    procedure Internal_OnShowAll(Sender: TObject);
    procedure Internal_RenderMiniForm;
    procedure Internal_RenderMiniFormAsValueList;
    procedure Internal_ClickRecord(Sender: TObject);
    procedure Internal_ClickOwnerRecord(Sender: TObject);
    procedure Internal_OnLinkClick(Sender : TObject);

    function Internal_OnRequestData :  TJupiterVariableList; override;
    function Internal_OnPopupRequestData :  TJupiterVariableList;

    function Internal_EnableWorkMenu : Boolean; override;
    procedure Internal_AddToWorkMenu; override;

    function Internal_GetFieldSize(prField : TField) : Integer;

    function Internal_GetRouteName : String;
    function Internal_GetMacroName : String;

    procedure Internal_SetColumnName(prColumn : TColumn);
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
var
  vrVez : Integer;
  vrStr : String;
begin
  vrStr := EmptyStr;

  if pnMiniForm.Visible then
    Self.Internal_RenderMiniFormAsValueList;

  for vrVez := 0 to InternalQuery.Fields.Count - 1 do
  begin
    if InternalQuery.Fields[vrVez] is TBlobField then
      Continue;

    if vrVez > 0 then
      vrStr := vrStr + #13#10;

    vrStr := vrStr + InternalQuery.Fields[vrVez].FieldName + ': ';

    if not InternalQuery.Fields[vrVez].IsNull then
      vrStr := vrStr + InternalQuery.Fields[vrVez].AsString;
  end;

  dbMainGrid.ShowHint := Trim(vrStr) <> EmptyStr;
  dbMainGrid.Hint := vrStr;
end;

procedure TFCustomDatabaseGrid.InternalQueryCalcFields(DataSet: TDataSet);
var
  vrVez : Integer;
  vrWizard : TJupiterDatabaseWizard;
begin
  vrWizard := vrJupiterApp.NewWizard;
  try
    for vrVez := 0 to InternalQuery.Fields.Count - 1 do
    begin
      if InternalQuery.Fields[vrVez].FieldKind <> fkCalculated then
        Continue;

      if InternalQuery.FieldByName(InternalQuery.Fields[vrVez].FieldName + '_ID').IsNull then
        Continue;

      InternalQuery.Fields[vrVez].AsString := JupiterDatabaseScript_ResolveRecordTable(vrWizard.GetForeignKeyData(Self.FReference.TableName, InternalQuery.Fields[vrVez].FieldName).TableDestinyName, InternalQuery.FieldByName(InternalQuery.Fields[vrVez].FieldName + '_ID').AsInteger);
    end;
  finally
    FreeAndNil(vrWizard);
  end;
end;

procedure TFCustomDatabaseGrid.miExibirColunaIDClick(Sender: TObject);
begin
  miExibirColunaID.Checked := not miExibirColunaID.Checked;

  Self.UpdateForm();
end;

procedure TFCustomDatabaseGrid.miShowMiniFormClick(Sender: TObject);
begin
  try
    miShowMiniForm.Checked := not miShowMiniForm.Checked;

    ValueListEditor1.DefaultColWidth := PercentOfScreen(ValueListEditor1.Width, 50);

    if miShowMiniForm.Checked then
      vrJupiterApp.Params.VariableById('Interface.Grid.ShowMiniForm').Value := BOOL_TRUE_STR
    else
      vrJupiterApp.Params.VariableById('Interface.Grid.ShowMiniForm').Value := BOOL_FALSE_STR;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFCustomDatabaseGrid.Splitter1Moved(Sender: TObject);
begin
  miLookColumn.Checked := False;
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

procedure TFCustomDatabaseGrid.dbMainGridDrawColumnTitle(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
end;

procedure TFCustomDatabaseGrid.dbMainGridEnter(Sender: TObject);
begin
  Self.UpdateForm(False);
end;

procedure TFCustomDatabaseGrid.dbMainGridExit(Sender: TObject);
begin
  Self.UpdateForm(False);
end;

procedure TFCustomDatabaseGrid.dbMainGridGetCellHint(Sender: TObject; Column: TColumn; var AText: String);
begin
  if InternalQuery.EOF then
    Exit;

  if not Column.Field.IsNull then
    AText := Column.Field.AsString;
end;

procedure TFCustomDatabaseGrid.dbMainGridTitleClick(Column: TColumn);
begin
  try
    if Self.Params.VariableById('orderBy').Value = Column.FieldName then
      Self.Params.VariableById('orderBy').Value := Column.FieldName + ' DESC'
    else
      Self.Params.VariableById('orderBy').Value := Column.FieldName;
  finally
    Self.UpdateForm();
  end;
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
  vrWidth : Integer;
  vrRemainingWidth : Integer;
begin
  inherited Internal_UpdateComponents;

  vrCountVisble := 0;

  for vrVez := 0 to dbMainGrid.Columns.Count - 1 do
  begin
    dbMainGrid.Columns[vrVez].Visible := True;

    if not miExibirColunaID.Checked then
      dbMainGrid.Columns[vrVez].Visible := dbMainGrid.Columns[vrVez].FieldName <> 'ID';

    if dbMainGrid.Columns[vrVez].Visible then
      if dbMainGrid.Columns[vrVez].Field is TBlobField then
        dbMainGrid.Columns[vrVez].Visible := False;

    if dbMainGrid.Columns[vrVez].Visible then
      vrCountVisble := vrCountVisble + 1;

    if dbMainGrid.Columns[vrVez].Field is TDateField then
      dbMainGrid.Columns[vrVez].DisplayFormat := FORMAT_DATE;

    if dbMainGrid.Columns[vrVez].Field is TTimeField then
      dbMainGrid.Columns[vrVez].DisplayFormat := FORMAT_TIME;
  end;

  vrRemainingWidth := dbMainGrid.Width - (dbMainGrid.Columns.Count * 5);

  for vrVez := 0 to dbMainGrid.Columns.Count - 1 do
  begin
    if dbMainGrid.Columns[vrVez].Visible then
      dbMainGrid.Columns[vrVez].Title.Caption := JupiterDatabaseScript_GetDescription(Self.FReference.TableName, dbMainGrid.Columns[vrVez].FieldName);

    vrWidth := Internal_GetFieldSize(dbMainGrid.Columns[vrVez].Field);

    if not Assigned(dbMainGrid.Columns[vrVez].Field.OnGetText) then
    begin
      if GetTextWidth(dbMainGrid.Columns[vrVez].Title.Caption, dbMainGrid.Font) > vrWidth then
         dbMainGrid.Columns[vrVez].Width := GetTextWidth(dbMainGrid.Columns[vrVez].Title.Caption + '   ', dbMainGrid.Font)
      else
         dbMainGrid.Columns[vrVez].Width := vrWidth;
    end;

    vrRemainingWidth := vrRemainingWidth - dbMainGrid.Columns[vrVez].Width;

    Self.Internal_SetColumnName(dbMainGrid.Columns[vrVez]);
  end;

  for vrVez := 0 to dbMainGrid.Columns.Count - 1 do
  begin
    if vrRemainingWidth <= 0 then
      Continue;

    if dbMainGrid.Columns[vrVez].Field is TStringField then
    begin
      dbMainGrid.Columns[vrVez].Width := dbMainGrid.Columns[vrVez].Width + vrRemainingWidth;
      vrRemainingWidth := 0;
    end;
  end;

  if vrRemainingWidth > 0 then
    dbMainGrid.Columns[0].Width := dbMainGrid.Columns[0].Width + vrRemainingWidth;

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

  Self.InternalQuery.PacketRecords := vrJupiterApp.Params.VariableById(FORM_GRID_LIMIT).AsInteger;

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
  vrCount : Integer;
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
    vrFields := vrWizard.GetSelectGridFields(Self.FReference.TableName);

    if InternalQuery.Active then
      if ((not InternalQuery.EOF) and (not InternalQuery.FieldByName('ID').IsNull)) then
        vrId := InternalQuery.FieldByName('ID').AsInteger;

    vrStringList := CreateStringList('');

    for vrVez := 0 to InternalQuery.Fields.Count - 1 do
    begin
      if (InternalQuery.Fields[vrVez] is TStringField) then
      begin
        vrStringList.Add(InternalQuery.Fields[vrVez].FieldName);
        Continue;
      end;

     if vrJupiterApp.Params.VariableById('TableGrid.Search.BlobFields').AsBool then
       if (InternalQuery.Fields[vrVez] is TBlobField) then
       begin
         vrStringList.Add(InternalQuery.Fields[vrVez].FieldName);
         Continue;
       end;
    end;

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

    InternalQuery.Prepare;
    InternalQuery.Open;

    Self.Internal_SetCalculatedFields;

    if vrId <> NULL_KEY then
      InternalQuery.Locate('ID', vrId, []);
  finally
    if ((Self.Params.Exists('where')) and (not Self.Params.VariableById('where').IsEmpty)) then
      vrCount := vrWizard.Count(Self.FReference.TableName, Self.Params.VariableById('where').Value)
    else
      vrCount := vrWizard.Count(Self.FReference.TableName, EmptyStr);

    if Self.FUseLimit then
      if Self.FLimit < vrCount then
        vrCount := Self.FLimit;

    Self.Hint := 'Tabela: ' + Self.FReference.TableName + '   Total de registros em tela: ' + FormatFloat(FORMAT_INTEGER_MASK, vrCount);

    FreeAndNil(vrWizard);

    if ((Self.Params.Exists('where')) and (not Self.Params.VariableById('where').IsEmpty)) then
      Self.Hint := Self.Hint + '. Existem filtros aplicados nesta consulta';

    InternalQuery.EnableControls;
  //  FreeAndNil(vrStringList);
  end;

  pnMiniForm.Visible := miShowMiniForm.Checked;

  if miLookColumn.Checked then
    pnMiniForm.Width := PercentOfScreen(Self.Width, Self.PercentDivisor);

  if pnMiniForm.Visible then
    Self.Internal_RenderMiniFormAsValueList;
end;

procedure TFCustomDatabaseGrid.Internal_RenderActions;
begin
  //
end;

procedure TFCustomDatabaseGrid.Internal_SetCalculatedFields;
var
  vrVez : Integer;
  vrVez2 : Integer;
  vrWizard : TJupiterDatabaseWizard;
begin
  vrWizard := vrJupiterApp.NewWizard;
  try
    for vrVez := 0 to InternalQuery.Fields.Count - 1 do
      if vrWizard.IsForeignKeyField(Self.FReference.TableName, InternalQuery.Fields[vrVez].FieldName) then
      begin
        InternalQuery.Fields[vrVez].OnGetText := @Internal_OnGetText;

        for vrVez2 := 0 to dbMainGrid.Columns.Count - 1 do
          if dbMainGrid.Columns[vrVez2].FieldName = InternalQuery.Fields[vrVez].FieldName then
            if dbMainGrid.Columns[vrVez2].Width < PercentOfScreen(dbMainGrid.Width, 40) then
              dbMainGrid.Columns[vrVez2].Width := PercentOfScreen(dbMainGrid.Width, 40);
      end;
  finally
    FreeAndNil(vrWizard);
  end;
end;

procedure TFCustomDatabaseGrid.Internal_OnGetText(Sender: TField; var aText: string; DisplayText: Boolean);
var
  vrWizard : TJupiterDatabaseWizard;
begin
  DisplayText := True;

  vrWizard := vrJupiterApp.NewWizard;
  try
    if InternalQuery.FieldByName(Sender.FieldName + '_ID').IsNull then
    begin
      DisplayText := False;
      Exit;
    end;

    aText := JupiterDatabaseScript_ResolveRecordTable(vrWizard.GetForeignKeyData(Self.FReference.TableName, Sender.FieldName).TableDestinyName, InternalQuery.FieldByName(Sender.FieldName + '_ID').AsInteger);
  finally
    FreeAndNil(vrWizard);
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
                                            {
          vrReferenceLink := JupiterComponentsNewLink('Ver mais', TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT + FORM_MARGIN_LEFT), sbMiniForm);

          TLabel(vrReferenceLink.Component).OnClick := @Internal_ClickOwnerRecord;
          TLabel(vrReferenceLink.Component).Tag := vrVez;
          TLabel(vrReferenceLink.Component).Hint := 'Acessar o registro pai';

          vrReference := vrReferenceLink;    }
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

procedure TFCustomDatabaseGrid.Internal_RenderMiniFormAsValueList;
var
  vrVez : Integer;
begin
  ValueListEditor1.Strings.Clear;

  ValueListEditor1.DefaultColWidth := PercentOfScreen(ValueListEditor1.Width, 50);

  for vrVez := 0 to dbMainGrid.Columns.Count - 1 do
  begin
    if not dbMainGrid.Columns[vrVez].Visible then
      Continue;

    if dbMainGrid.Columns[vrVez].Field.IsNull then
      ValueListEditor1.Strings.Add(dbMainGrid.Columns[vrVez].Title.Caption + '=NULO')
    else
      ValueListEditor1.Strings.Add(dbMainGrid.Columns[vrVez].Title.Caption + '=' + dbMainGrid.Columns[vrVez].Field.AsString);
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

function TFCustomDatabaseGrid.Internal_EnableWorkMenu: Boolean;
begin
  Result := True;
end;

procedure TFCustomDatabaseGrid.Internal_AddToWorkMenu;
var
  vrModule : TJupiterModule;
  vrWizard : TJupiterDatabaseWizard;
begin
  inherited Internal_AddToWorkMenu;

  vrWizard := vrJupiterApp.NewWizard;
  vrModule := TJupiterModule.Create;
  try
    if vrModule.CreateMacroIfDontExists(Self.Internal_GetMacroName, 'Clique do item de menu ' + Self.FReference.TableName, CreateStringListToMacro(' OpenGridFromTableWithWhere(''' + Self.FReference.TableName + ''', ''' + Self.Params.VariableById('where').Value + ''', ''' + Self.Params.VariableById('orderBy').Value + '''); ')) then
      vrModule.CreateRouteIfDontExists(Self.Caption, Self.Internal_GetRouteName, vrWizard.GetLastID('MACROS'), ICON_GRID, 1000);
  finally
    FreeAndNil(vrModule);
    FreeAndNil(vrWizard);
  end;
end;

function TFCustomDatabaseGrid.Internal_GetFieldSize(prField: TField): Integer;
begin
  Result := PercentOfScreen(dbMainGrid.Width, 25);

  if prField is TIntegerField then
  begin
    Result := GetTextWidth('10000   ', dbMainGrid.Font);
    Exit;
  end;

  if prField is TFloatField then
  begin
    Result := GetTextWidth('10000,00   ', dbMainGrid.Font);
    Exit;
  end;

  if prField is TDateField then
  begin
    Result := GetTextWidth('00/00/0000   ', dbMainGrid.Font);
    Exit;
  end;

  if prField is TTimeField then
  begin
    Result := GetTextWidth('00:00:00   ', dbMainGrid.Font);
    Exit;
  end;

  if prField is TDateTimeField then
  begin
    Result := GetTextWidth('00/00/00 00:00:00   ', dbMainGrid.Font);
    Exit;
  end;
end;

function TFCustomDatabaseGrid.Internal_GetRouteName: String;
begin
  Result := vrJupiterApp.Params.VariableById('Menus.Work.Route').Value + AnsiLowerCase(Self.FReference.TableName) + '/' + FormatDateTime('ddmmyyyy_hhnnss', Now);
end;

function TFCustomDatabaseGrid.Internal_GetMacroName: String;
begin
  Result := JupiterStringUtilsScript_Replace(Copy(Self.Internal_GetRouteName, 2), '/', '.');
end;

procedure TFCustomDatabaseGrid.Internal_SetColumnName(prColumn: TColumn);
const
  ARROW_UP = '   ↑';
  ARROW_DOWN = '   ↓';
var
  vrStr : String;
  vrOrder : String;
begin
  vrStr   := prColumn.FieldName;
  vrOrder := EmptyStr;

  if Self.Params.Exists('orderBy') then
    vrOrder := Self.Params.VariableById('orderBy').Value;

  if vrOrder = EmptyStr then
    Exit;

  if Pos(vrStr, vrOrder) = 0 then
    Exit;

  vrStr := Copy(vrOrder, Pos(prColumn.FieldName, Self.Params.VariableById('orderBy').Value));

  if ((JupiterStringUtilsScript_GetNextWord(prColumn.FieldName, vrStr) = 'DESC') or (JupiterStringUtilsScript_GetNextWord(prColumn.FieldName, vrStr) = 'DESC,')) then
    prColumn.Title.Caption := prColumn.Title.Caption + ARROW_DOWN
  else
    prColumn.Title.Caption := prColumn.Title.Caption + ARROW_UP;
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

