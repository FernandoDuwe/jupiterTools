unit uCustomDatabaseForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBCtrls, StdCtrls,
  DBDateTimePicker, SQLDB, DB, uJupiterForm, jupiterformutils, ExtCtrls,
  ComCtrls, jupiterStringUtils, jupiterDatabaseWizard, JupiterApp,
  JupiterVariable, JupiterObject, JupiterConsts, JupiterModule,
  uJupiterStringUtilsScript, uJupiterRunnableScript, jupiterformcomponenttils,
  jupiterDesktopApp, jupiterformdbcomponenttils;

type

  { TFCustomDatabaseForm }

  TFCustomDatabaseForm = class(TFJupiterForm)
    InternalDataSource: TDataSource;
    pcRecord: TPageControl;
    sbBody: TScrollBox;
    tsMain: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InternalDataSourceDataChange(Sender: TObject; Field: TField);
    procedure InternalDataSourceStateChange(Sender: TObject);
    procedure Internal_ClickMenuClick(Sender: TObject);
    procedure Internal_OnCopyClick(Sender: TObject);
    procedure Internal_OnExecuteClick(Sender: TObject);
    procedure Internal_OnViewClick(Sender: TObject);
    procedure Internal_OnMemoDBClick(Sender : TObject);

    procedure Internal_OnMarkPinClick(Sender: TObject);
    procedure Internal_OnUnMarkPinClick(Sender: TObject);
    procedure sbBodyClick(Sender: TObject);
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

    procedure Internal_OnCloseIfModal; override;

    procedure Internal_ListForeignTables;

    function Internal_OnRequestData : TJupiterVariableList; override;

    function Internal_EnableWorkMenu : Boolean; override;

    function Internal_GetRouteName : String;
    function Internal_GetMacroName : String;

    procedure Internal_AddToWorkMenu; override;

    procedure Internal_OnAfterExecuteAction; override;

    procedure Internal_RenderButtons(prReference : TJupiterComponentReference; prTableName, prFieldName : String; prIndex : Integer);
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

uses uJupiterAction, Menus, Clipbrd, Buttons, uJupiterDesktopAppScript, uJupiterDatabaseScript;

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

procedure TFCustomDatabaseForm.Internal_OnExecuteClick(Sender: TObject);
var
  vrField : Integer;
begin
  if (Sender is TSpeedButton) then
  begin
    vrField := TSpeedButton(Sender).Tag;

    if not Self.QueryOrigin.Fields[vrField].IsNull then
      JupiterRunnableScript_RunCommandOnJupiter(Self.QueryOrigin.Fields[vrField].AsString);
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

procedure TFCustomDatabaseForm.Internal_OnMemoDBClick(Sender: TObject);
var
  vrParams : TJupiterVariableList;
begin
  vrParams := Self.Internal_OnRequestData;

  if Sender is TDBMemo then
    vrParams.AddVariable('CURRENTFIELD', TDBMemo(Sender).Field.FieldName);

  vrParams.AddVariable('TABLENAME', Self.FTableName);

  vrJupiterApp.RunMacro(TRIGGER_ONMEMODBCLICK, vrParams);
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

procedure TFCustomDatabaseForm.sbBodyClick(Sender: TObject);
begin

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
  vrAction : TJupiterComponentReference;
  vrReference : TJupiterComponentReference;
  vrWizard : TJupiterDatabaseWizard;
  vrField : TField;
  vrHeight : Integer;
  vrIsFirst : Boolean;
  vrNewTab : TTabSheet;
begin
  vrCurrentLine := FORM_MARGIN_TOP;

  vrIsFirst := False;

  if not Assigned(Self.QueryOrigin) then
    Exit;

  InternalDataSource.DataSet := Self.QueryOrigin;

  vrWizard := vrJupiterApp.NewWizard;
  try
    for vrVez := 0 to Self.QueryOrigin.Fields.Count - 1 do
    begin
      if Self.QueryOrigin.Fields[vrVez].FieldName = 'ID' then
        Continue;

      if ((vrIsFirst) and (vrJupiterApp.Params.VariableById('Interface.Form.Separator').AsBool)) then
      begin
        vrCurrentLine := vrCurrentLine - FORM_MARGIN_BOTTOM;

        vrReference := JupiterComponentsAddLine(TJupiterPosition.Create(vrCurrentLine, 0), 1, sbBody.Width, sbBody);

        TShape(vrReference.Component).Pen.Color := clSilver;

        vrCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;
      end;

      vrField := Self.QueryOrigin.Fields[vrVez];

      if ((vrField is TDateField) or (vrField is TDateTimeField)) then
      begin
        vrIsFirst := True;

        vrReference := JupiterFormDBComponent_NewDateTimeTextEdit(Self.TableName, vrField, InternalDataSource, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody);
        vrCurrentLine := vrReference.Bottom + FORM_MARGIN_TOP + FORM_MARGIN_BOTTOM;

        Continue;
      end;

      if ((vrField is TBlobField) and (vrJupiterApp.Params.VariableById('Interface.Form.Memo.RenderInNewTab').AsBool)) then
      begin
        vrNewTab := pcRecord.AddTabSheet;
        vrNewTab.Caption := JupiterDatabaseScript_GetDescription(Self.TableName, vrField.FieldName);

        vrReference := JupiterComponentsNewDBMemo(vrField, InternalDataSource, TJupiterPosition.Create(0, 0), vrNewTab);

        TDBMemo(vrReference.Component).OnDblClick := @Internal_OnMemoDBClick;
        TDBMemo(vrReference.Component).Align := alClient;

        Continue;
      end;

      if vrField is TBlobField then
      begin
        vrIsFirst := True;

        vrReference := JupiterFormDBComponent_NewDateTextMemoEdit(Self.TableName, vrField, InternalDataSource, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody);
        vrCurrentLine := vrReference.Bottom + FORM_MARGIN_TOP + FORM_MARGIN_BOTTOM;

        if vrJupiterApp.Params.VariableById('Interface.Form.Memo.LasExpand').AsBool then
          if vrVez = (Self.QueryOrigin.Fields.Count - 1) then
          begin
            vrHeight := sbBody.Height;
            vrHeight := vrHeight - (vrReference.Bottom);

            if vrHeight > 0 then
              TDBMemo(vrReference.Component).Height := (TDBMemo(vrReference.Component).Height + vrHeight) - (FORM_MARGIN_BOTTOM_TONEXT * 3);
          end;

        TDBMemo(vrReference.Component).OnDblClick := @Internal_OnMemoDBClick;

//        Self.Internal_RenderButtons(vrReference, Self.FTableName, vrField.FieldName, vrVez);

        Continue;
      end;

      if vrField is TBooleanField then
      begin
        vrIsFirst := True;

        vrReference := JupiterFormDBComponent_NewDateBooleanEdit(Self.TableName, vrField, InternalDataSource, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody);
        vrCurrentLine := vrReference.Bottom + FORM_MARGIN_TOP + FORM_MARGIN_BOTTOM;

        Continue;
      end;

      if vrWizard.IsForeignKeyField(Self.TableName, Self.QueryOrigin.Fields[vrVez].FieldName) then
      begin
        vrIsFirst := True;

        vrReference := JupiterFormDBComponent_NewForeignKeyEdit(Self.TableName, vrField, InternalDataSource, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody);
        vrCurrentLine := vrReference.Bottom + FORM_MARGIN_TOP + FORM_MARGIN_BOTTOM;

        vrReference.Top := vrReference.Top - 2;

        vrAction := JupiterComponentsAddAction(vrReference, ICON_VIEW, sbBody);

        TSpeedButton(vrAction.Component).Tag := vrVez;
        TSpeedButton(vrAction.Component).OnClick := @Internal_OnViewClick;
        TSpeedButton(vrAction.Component).Hint := 'Clique aqui para visulizar o cadastro';
        TSpeedButton(vrAction.Component).ShowHint := True;

        Continue;
      end;

      vrIsFirst := True;

      vrReference := JupiterFormDBComponent_NewTextEdit(Self.TableName, vrField, InternalDataSource, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody);
      vrCurrentLine := vrReference.Bottom + FORM_MARGIN_TOP + FORM_MARGIN_BOTTOM;

      Self.Internal_RenderButtons(vrReference, Self.FTableName, vrField.FieldName, vrVez);
    end;

    if vrCurrentLine > sbBody.Height then
    begin
      vrReference := JupiterComponentsNewLabel(EmptyStr, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody);

      vrCurrentLine := vrReference.Bottom + FORM_MARGIN_TOP + FORM_MARGIN_BOTTOM;
    end;

    if ((not vrJupiterApp.Params.VariableById('Interface.Form.AsList').AsBool) and (vrJupiterApp.Params.VariableById('Interface.Form.Separator').AsBool)) then
    begin
      if vrCurrentLine > sbBody.Height then
        vrReference := JupiterComponentsAddLine(TJupiterPosition.Create(0, vrJupiterApp.Params.VariableById('Interface.Form.Label.Size').AsInteger + 5), vrCurrentLine, 1, sbBody)
      else
        vrReference := JupiterComponentsAddLine(TJupiterPosition.Create(0, vrJupiterApp.Params.VariableById('Interface.Form.Label.Size').AsInteger + 5), sbBody.Height, 1, sbBody);

      TShape(vrReference.Component).Pen.Color := clSilver;
    end;

    pcRecord.ShowTabs := pcRecord.PageCount > 1;
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

      if vrJupiterApp.ExistsMacro(jupiterStringUtilsGetDatabaseTriggerName(TRIGGER_DATABASE_AFTERPOST, Self.FTableName)) then
        vrJupiterApp.RunMacro(jupiterStringUtilsGetDatabaseTriggerName(TRIGGER_DATABASE_AFTERPOST, Self.FTableName), Self.Internal_OnRequestData);

      vrDatabase.Commit;
    except
      vrDatabase.Rollback;

      raise;
    end;
  finally
    FreeAndNil(vrDatabase);

    TJupiterDesktopApp(vrJupiterApp).UpdateChildrenForms;
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

procedure TFCustomDatabaseForm.Internal_OnCloseIfModal;
begin
  if InternalDataSource.State = dsInsert then
  begin
    Self.Internal_OnCancel(Self);

    inherited Internal_OnCloseIfModal;
  end;

  Self.Internal_OnCancel(Self);
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

function TFCustomDatabaseForm.Internal_EnableWorkMenu: Boolean;
var
  vrWizard : TJupiterDatabaseWizard;
begin
  Result := Self.FID > NULL_KEY;

  if not Result then
    Exit;

  vrWizard := vrJupiterApp.NewWizard;
  try
    if vrWizard.Exists('ROUTES', ' ROUTE = "' + Self.Internal_GetRouteName + '" ') then
    begin
      Result := False;
      Exit;
    end;

    Result := True;
  finally
    FreeAndNil(vrWizard);
  end;
end;

function TFCustomDatabaseForm.Internal_GetRouteName: String;
begin
  Result := vrJupiterApp.Params.VariableById('Menus.Work.Route.Records').Value + AnsiLowerCase(Self.FTableName) + '/' + IntToStr(Self.FID);
end;

function TFCustomDatabaseForm.Internal_GetMacroName: String;
begin
  Result := JupiterStringUtilsScript_Replace(Copy(Self.Internal_GetRouteName, 2), '/', '.');
end;

procedure TFCustomDatabaseForm.Internal_AddToWorkMenu;
var
  vrModule : TJupiterModule;
  vrWizard : TJupiterDatabaseWizard;
begin
  inherited Internal_AddToWorkMenu;

  vrWizard := vrJupiterApp.NewWizard;
  vrModule := TJupiterModule.Create;
  try
    if vrModule.CreateMacroIfDontExists(Self.Internal_GetMacroName, 'Clique do item de menu ' + Self.FTableName + ' ' + IntToStr(Self.FID), CreateStringListToMacro(' OpenFormFromTableId(''' + Self.FTableName + ''', ' + IntToStr(Self.FID) + '); ')) then
      vrModule.CreateRouteIfDontExists(Self.Caption, Self.Internal_GetRouteName, vrWizard.GetLastID('MACROS'), ICON_NEW, 1000);
  finally
    FreeAndNil(vrModule);
    FreeAndNil(vrWizard);
  end;
end;

procedure TFCustomDatabaseForm.Internal_OnAfterExecuteAction;
begin
  inherited Internal_OnAfterExecuteAction;

  if InternalDataSource.State in [dsEdit, dsInsert] then
    Exit;

  Self.QueryOrigin.Close;
  Self.QueryOrigin.Open;
end;

procedure TFCustomDatabaseForm.Internal_RenderButtons(prReference: TJupiterComponentReference; prTableName, prFieldName: String; prIndex : Integer);
var
  vrWizard : TJupiterDatabaseWizard;
  vrAction : TJupiterComponentReference;
begin
  vrWizard := vrJupiterApp.NewWizard;
  try
    if vrWizard.Exists('DATABASE_DICTIONARY', ' TABLENAME = "' + prTableName + '" AND FIELDNAME = "' + prFieldName + '" AND ACTION_COPY = TRUE ') then
    begin
      vrAction := JupiterComponentsAddAction(prReference, ICON_COPY, sbBody);
      TSpeedButton(vrAction.Component).Tag := prIndex;
      TSpeedButton(vrAction.Component).OnClick := @Internal_OnCopyClick;
      TSpeedButton(vrAction.Component).Hint := 'Clique aqui para copiar o conteúdo do campo';
      TSpeedButton(vrAction.Component).ShowHint := True;
    end;

    if vrWizard.Exists('DATABASE_DICTIONARY', ' TABLENAME = "' + prTableName + '" AND FIELDNAME = "' + prFieldName + '" AND ACTION_EXECUTE = TRUE ') then
    begin
      vrAction := JupiterComponentsAddAction(prReference, ICON_PLAY, sbBody);

      TSpeedButton(vrAction.Component).Tag := prIndex;
      TSpeedButton(vrAction.Component).OnClick := @Internal_OnExecuteClick;
      TSpeedButton(vrAction.Component).Hint := 'Clique aqui para executar o conteúdo do campo atual';
      TSpeedButton(vrAction.Component).ShowHint := True;
    end;
  finally
    FreeAndNil(vrWizard);
  end;
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

  if Length(Self.Caption) > vrJupiterApp.Params.VariableById(FORM_DESCRIPTION_MAXSIZE).AsInteger then
    Self.Caption := Copy(Self.Caption, 1, vrJupiterApp.Params.VariableById(FORM_DESCRIPTION_MAXSIZE).AsInteger) + '...';

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

