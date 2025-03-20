unit uCustomDatabaseForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBCtrls,
  DBDateTimePicker, SQLDB, DB, uJupiterForm, jupiterformutils,
  jupiterStringUtils, jupiterDatabaseWizard, JupiterApp, JupiterVariable,
  JupiterObject, JupiterConsts, uJupiterStringUtilsScript,
  jupiterformcomponenttils, jupiterDesktopApp;

type

  { TFCustomDatabaseForm }

  TFCustomDatabaseForm = class(TFJupiterForm)
    InternalDataSource: TDataSource;
    sbBody: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InternalDataSourceDataChange(Sender: TObject; Field: TField);
    procedure InternalDataSourceStateChange(Sender: TObject);
    procedure Internal_ClickMenuClick(Sender: TObject);
    procedure Internal_OnCopyClick(Sender: TObject);
    procedure Internal_OnViewClick(Sender: TObject);

    procedure Internal_OnMarkPinClick(Sender: TObject);
    procedure Internal_OnUnMarkPinClick(Sender: TObject);
  private
    FObjectList : TJupiterObjectList;

    FTableName : String;
    FID        : Integer;
    FQueryOrigin : TSQLQuery;

    procedure Internal_PrepareForm; override;
    procedure Internal_BuildForm;

    procedure Internal_OnSave(Sender: TObject);
    procedure Internal_OnCancel(Sender: TObject);

    procedure Internal_UpdateComponents; override;

    procedure Internal_ListForeignTables;

    function Internal_OnRequestData : TJupiterVariableList; override;
  published
    property QueryOrigin : TSQLQuery read FQueryOrigin write FQueryOrigin;

    property TableName : String read FTableName;
    property Id : Integer read FID;
  public
    procedure FromReference(prReference : TJupiterDatabaseReference);
  end;

var
  FCustomDatabaseForm: TFCustomDatabaseForm;

implementation

uses uJupiterAction, Menus, Clipbrd, Buttons, uJupiterDesktopAppScript;

{$R *.lfm}

{ TFCustomDatabaseForm }

procedure TFCustomDatabaseForm.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FObjectList := TJupiterObjectList.Create;

  Self.FTableName := EmptyStr;
  Self.FID        := NULL_KEY;
end;

procedure TFCustomDatabaseForm.FormDestroy(Sender: TObject);
begin
  if Self.FID <> NULL_KEY then
    vrJupiterApp.RemoveReference(Self.FTableName, Self.FID);

  inherited;

  FreeAndNil(FObjectList);
end;

procedure TFCustomDatabaseForm.InternalDataSourceDataChange(Sender: TObject; Field: TField);
begin
  Self.UpdateForm();
end;

procedure TFCustomDatabaseForm.InternalDataSourceStateChange(Sender: TObject);
begin
  Self.UpdateForm();
end;

procedure TFCustomDatabaseForm.Internal_ClickMenuClick(Sender: TObject);
var
  vrReference : TJupiterDatabaseForeignKeyReference;
  vrWhere : String;
begin
  if not (Sender is TMenuItem) then
    Exit;

  vrReference := TJupiterDatabaseForeignKeyReference(Self.FObjectList.GetAtIndex(TMenuItem(Sender).Tag));

  if Self.QueryOrigin.FieldByName(vrReference.FieldDestinyName).IsNull then
     vrWhere := vrReference.FieldName + ' IS NULL '
   else
     vrWhere := vrReference.FieldName + ' = ' + IntToStr(Self.QueryOrigin.FieldByName(vrReference.FieldDestinyName).AsInteger);

  vrJupiterApp.RunScript(CreateStringListToMacro(' OpenGridFromTableWithWhere(''' + vrReference.TableName + ''', ''' + vrWhere + ''', ''''); '), TJupiterVariableList.Create);
end;

procedure TFCustomDatabaseForm.Internal_OnCopyClick(Sender: TObject);
var
  vrField : Integer;
begin
  if (Sender is TSpeedButton) then
  begin
    vrField := TSpeedButton(Sender).Tag;

    if not Self.QueryOrigin.Fields[vrField].IsNull then
      Clipboard.AsText := Self.QueryOrigin.Fields[vrField].AsString;
  end;
end;

procedure TFCustomDatabaseForm.Internal_OnViewClick(Sender: TObject);
var
  vrField : Integer;
begin
  if (Sender is TSpeedButton) then
  begin
    vrField := TSpeedButton(Sender).Tag;

    if Self.QueryOrigin.Fields[vrField].IsNull then
      Exit;

    JupiterAppDesktopOpenFormFromTableId(TJupiterDesktopApp(vrJupiterApp).NewWizard.GetForeignKeyData(Self.TableName, Self.QueryOrigin.Fields[vrField].FieldName).TableDestinyName,
                                         Self.QueryOrigin.Fields[vrField].AsInteger);
  end;
end;

procedure TFCustomDatabaseForm.Internal_OnMarkPinClick(Sender: TObject);
begin
  try
    TJupiterDesktopApp(vrJupiterApp).CreatePin(Self.TableName, Self.Id);
  finally
    Self.UpdateForm;
  end;
end;

procedure TFCustomDatabaseForm.Internal_OnUnMarkPinClick(Sender: TObject);
begin
  try
    TJupiterDesktopApp(vrJupiterApp).RemovePin(Self.TableName, Self.Id);
  finally
    Self.UpdateForm;
  end;
end;

procedure TFCustomDatabaseForm.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Salvar', 'Clique aqui para salvar', ICON_SAVE, @Internal_OnSave));
  Self.ActionGroup.AddAction(TJupiterAction.Create('Cancelar', 'Clique aqui para cancelar', ICON_CANCEL, @Internal_OnCancel));

  Self.ActionGroup.AddAction(TJupiterAction.Create(EmptyStr, 'Clique aqui para marcar esse registro como fixo', ICON_PIN, @Internal_OnMarkPinClick));
  Self.ActionGroup.AddAction(TJupiterAction.Create(EmptyStr, 'Clique aqui para desmarcar esse registro como fixo', ICON_UNPIN, @Internal_OnUnMarkPinClick));


  Self.ActionGroup.TableName := Self.TableName;

  Self.Internal_BuildForm;
end;

procedure TFCustomDatabaseForm.Internal_BuildForm;
var
  vrCurrentLine : Integer;
  vrVez : Integer;
  vrHeight : Integer;
  vrReference : TJupiterComponentReference;
  vrWizard : TJupiterDatabaseWizard;
  vrAction : TJupiterComponentReference;
  vrField : TField;
begin
  vrCurrentLine := FORM_MARGIN_TOP;

  if not Assigned(Self.QueryOrigin) then
    Exit;

  InternalDataSource.DataSet := Self.QueryOrigin;

  vrWizard := vrJupiterApp.NewWizard;
  try
    for vrVez := 0 to Self.QueryOrigin.Fields.Count - 1 do
    begin
      if Self.QueryOrigin.Fields[vrVez].FieldName = 'ID' then
        Continue;

      vrField := Self.QueryOrigin.Fields[vrVez];

      if not (Self.QueryOrigin.Fields[vrVez] is TBooleanField) then
      begin
        vrReference := JupiterComponentsNewLabel(JupiterStringUtilsNormalizeToPresent(Self.QueryOrigin.Fields[vrVez].DisplayName),
                                                 TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody);

        // Pulando linha
        vrCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;
      end;

      if vrWizard.IsForeignKeyField(Self.TableName, Self.QueryOrigin.Fields[vrVez].FieldName) then
      begin
        vrReference := JupiterComponentsNewDBComboBox(Self.QueryOrigin.Fields[vrVez],
                                                      InternalDataSource,
                                                      TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT),
                                                      sbBody,
                                                      vrWizard.GetForeignKeyData(Self.TableName, Self.QueryOrigin.Fields[vrVez].FieldName));

        vrAction := JupiterComponentsAddAction(vrReference, ICON_VIEW, sbBody);

        TSpeedButton(vrAction.Component).Tag := vrVez;
        TSpeedButton(vrAction.Component).OnClick := @Internal_OnViewClick;
        TSpeedButton(vrAction.Component).Hint := 'Clique aqui para visulizar o cadastro';
        TSpeedButton(vrAction.Component).ShowHint := True;

     //   JupiterComponentsAddAction(vrReference, ICON_SEARCH, sbBody);
      end
      else
        if ((Self.QueryOrigin.Fields[vrVez] is TDateField) or (Self.QueryOrigin.Fields[vrVez] is TDateTimeField)) then
          vrReference := JupiterComponentsNewDBDatePicker(Self.QueryOrigin.Fields[vrVez], InternalDataSource, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody)
        else
          if Self.QueryOrigin.Fields[vrVez] is TBlobField then
          begin
            vrReference := JupiterComponentsNewDBMemo(Self.QueryOrigin.Fields[vrVez], InternalDataSource, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody);

            // Se é o último componente
            if (vrVez = (Self.QueryOrigin.Fields.Count - 1)) then
            begin
              vrHeight := sbBody.Height;
              vrHeight := vrHeight - TDBMemo(vrReference.Component).Top - TDBMemo(vrReference.Component).Height;

              if vrHeight > 0 then
                TDBMemo(vrReference.Component).Height := (TDBMemo(vrReference.Component).Height + vrHeight) - FORM_MARGIN_BOTTOM_TONEXT - FORM_MARGIN_BOTTOM_TONEXT;
            end;
          end
          else
            if Self.QueryOrigin.Fields[vrVez] is TBooleanField then
              vrReference := JupiterComponentsNewDBCheckBox(Self.QueryOrigin.Fields[vrVez], InternalDataSource, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody)
            else
            begin
              vrReference := JupiterComponentsNewDBEdit(Self.QueryOrigin.Fields[vrVez], InternalDataSource, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody);

              vrAction := JupiterComponentsAddAction(vrReference, ICON_COPY, sbBody);
              TSpeedButton(vrAction.Component).Tag := vrVez;
              TSpeedButton(vrAction.Component).OnClick := @Internal_OnCopyClick;
              TSpeedButton(vrAction.Component).Hint := 'Clique aqui para copiar o conteúdo do campo';
              TSpeedButton(vrAction.Component).ShowHint := True;
            end;

      // Pulando linha
          vrCurrentLine := vrReference.Bottom + FORM_MARGIN_TOP + FORM_MARGIN_BOTTOM;
    end;
  finally
    FreeAndNil(vrWizard);
  end;
end;

procedure TFCustomDatabaseForm.Internal_OnSave(Sender: TObject);
var
  vrDatabase : TJupiterDatabaseWizard;
begin
  vrDatabase := vrJupiterApp.NewWizard;
  try
    vrDatabase.StartTransaction;

    try
      Self.QueryOrigin.Post;
      Self.QueryOrigin.ApplyUpdates(-1);

      vrDatabase.Commit;
    except
      vrDatabase.Rollback;

      raise;
    end;
  finally
    FreeAndNil(vrDatabase);
  end;

  Self.DoSecureClose;
end;

procedure TFCustomDatabaseForm.Internal_OnCancel(Sender: TObject);
begin
  Self.QueryOrigin.Cancel;
end;

procedure TFCustomDatabaseForm.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  Self.ActionGroup.GetActionAtIndex(2).SetInvisibility;
  Self.ActionGroup.GetActionAtIndex(3).SetInvisibility;

  if not (InternalDataSource.State in [dsInsert]) then
  begin
    if TJupiterDesktopApp(vrJupiterApp).PinExists(Self.TableName, Self.Id) then
      Self.ActionGroup.GetActionAtIndex(3).SetVisibility
    else
      Self.ActionGroup.GetActionAtIndex(2).SetVisibility;
  end;

  if InternalDataSource.State in [dsEdit, dsInsert] then
  begin
    if InternalDataSource.State in [dsInsert] then
      Self.Hint := 'Criando um novo registro em ' + Self.FTableName
    else
      Self.Hint := 'Editando registro #' + FQueryOrigin.FieldByName('ID').AsString + ', da tabela ' + Self.FTableName;

    Self.ActionGroup.GetActionAtIndex(0).Enable;
    Self.ActionGroup.GetActionAtIndex(1).Enable;

    Exit;
  end
  else
    Self.Hint := 'Visualizando registro #' + FQueryOrigin.FieldByName('ID').AsString + ', da tabela ' + Self.FTableName;

  Self.ActionGroup.GetActionAtIndex(0).Disable;
  Self.ActionGroup.GetActionAtIndex(1).Disable;
end;

procedure TFCustomDatabaseForm.Internal_ListForeignTables;
var
  vrObjectList : TJupiterObjectList;
  vrVez : Integer;
  vrReference : TJupiterDatabaseForeignKeyReference;
  vrCount : Integer;
  vrObjReference : TJupiterComponentReference;
begin
  vrObjectList := vrJupiterApp.NewWizard.GetForeignKeysFromTable(Self.TableName);

  if vrObjectList.Count = 0 then
    Exit;

  JupiterComponentsAddPopupMenuSeparator(pmOptions);

  vrCount := 0;

  for vrVez := 0 to vrObjectList.Count - 1 do
  begin
    vrReference := TJupiterDatabaseForeignKeyReference(vrObjectList.GetAtIndex(vrVez));

     if Self.QueryOrigin.FieldByName(vrReference.FieldDestinyName).IsNull then
       vrCount := vrJupiterApp.NewWizard.Count(vrReference.TableName, vrReference.FieldName + ' IS NULL ')
     else
       vrCount := vrJupiterApp.NewWizard.Count(vrReference.TableName, vrReference.FieldName + ' = ' + IntToStr(Self.QueryOrigin.FieldByName(vrReference.FieldDestinyName).AsInteger));

    vrObjReference := JupiterComponentsAddPopupMenuItem(pmOptions, JupiterStringUtilsNormalizeToPresent(vrReference.TableName) + ', ' + JupiterStringUtilsNormalizeToPresent(vrReference.FieldName) + ' (' + IntToStr(vrCount) + ')', EmptyStr, ICON_GRID);

    Self.FObjectList.Add(vrReference);

    TMenuItem(vrObjReference.Component).OnClick := @Internal_ClickMenuClick;
    TMenuItem(vrObjReference.Component).Tag := Self.FObjectList.Count - 1;
  end;
end;

function TFCustomDatabaseForm.Internal_OnRequestData: TJupiterVariableList;
var
  vrVez : Integer;
begin
  Result := inherited Internal_OnRequestData;

  for vrVez := 0 to Self.FQueryOrigin.FieldCount - 1 do
    if not Result.Exists(Self.FQueryOrigin.Fields[vrVez].FieldName) then
      Result.AddVariable(Self.FQueryOrigin.Fields[vrVez].FieldName, Self.FQueryOrigin.Fields[vrVez].AsString, EmptyStr);
end;

procedure TFCustomDatabaseForm.FromReference(prReference: TJupiterDatabaseReference);
var
  vrQry : TSQLQuery;
  vrDatabase : TJupiterDatabaseWizard;
begin
  if prReference.ID = NULL_KEY then
    Self.Caption := String.Format('%0:s %1:s', ['Novo:', JupiterStringUtilsNormalizeToPresent(prReference.TableName)])
  else
    Self.Caption := String.Format('%0:s', [TJupiterDesktopApp(vrJupiterApp).NewWizard.GetTableDescription(prReference.TableName, prReference.ID)]);

  vrDatabase := vrJupiterApp.NewWizard;
  try
    vrQry := vrDatabase.NewQueryFromReference(prReference);

    Self.FTableName := prReference.TableName;
    Self.FID        := prReference.ID;
  finally
    vrQry.Open;

    Self.QueryOrigin := vrQry;

    if prReference.ID = NULL_KEY then
      vrQry.Insert
    else
    begin
      vrJupiterApp.AddGlobalReference(TJupiterDatabaseReference.Create(Self.FTableName, Self.FID));

      Self.Internal_ListForeignTables;
    end;
  end;
end;

end.

