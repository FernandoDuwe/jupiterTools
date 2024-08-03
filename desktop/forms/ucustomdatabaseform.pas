unit uCustomDatabaseForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBCtrls,
  DBDateTimePicker, SQLDB, DB, uJupiterForm, jupiterformutils,
  jupiterStringUtils, jupiterDatabaseWizard, JupiterApp,
  jupiterformcomponenttils;

type

  { TFCustomDatabaseForm }

  TFCustomDatabaseForm = class(TFJupiterForm)
    InternalDataSource: TDataSource;
    sbBody: TScrollBox;
    procedure FormCreate(Sender: TObject);
  private
    FTableName : String;
    FID        : Integer;
    FQueryOrigin : TSQLQuery;

    procedure Internal_PrepareForm; override;
    procedure Internal_BuildForm;
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

uses JupiterConsts, uJupiterAction;

{$R *.lfm}

{ TFCustomDatabaseForm }

procedure TFCustomDatabaseForm.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FTableName := EmptyStr;
  Self.FID        := NULL_KEY;
end;

procedure TFCustomDatabaseForm.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Salvar', 'Clique aqui para salvar', ICON_SAVE));
  Self.ActionGroup.AddAction(TJupiterAction.Create('Cancelar', 'Clique aqui para cancelar', ICON_CANCEL));

  Self.Internal_BuildForm;
end;

procedure TFCustomDatabaseForm.Internal_BuildForm;
var
  vrCurrentLine : Integer;
  vrVez : Integer;
  vrReference : TJupiterComponentReference;
begin
  vrCurrentLine := FORM_MARGIN_TOP;

  if not Assigned(Self.QueryOrigin) then
    Exit;

  InternalDataSource.DataSet := Self.QueryOrigin;

  for vrVez := 0 to Self.QueryOrigin.Fields.Count - 1 do
  begin
    if Self.QueryOrigin.Fields[vrVez].FieldName = 'ID' then
      Continue;


    vrReference := JupiterComponentsNewLabel(JupiterStringUtilsNormalizeToPresent(Self.QueryOrigin.Fields[vrVez].DisplayName),
                                             TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody);

    // Pulando linha
    vrCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;

    if ((Self.QueryOrigin.Fields[vrVez] is TDateField) or (Self.QueryOrigin.Fields[vrVez] is TDateTimeField)) then
      vrReference := JupiterComponentsNewDBDatePicker(Self.QueryOrigin.Fields[vrVez], InternalDataSource, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody)
    else
      if Self.QueryOrigin.Fields[vrVez] is TBlobField then
        vrReference := JupiterComponentsNewDBMemo(Self.QueryOrigin.Fields[vrVez], InternalDataSource, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody)
      else
        vrReference := JupiterComponentsNewDBEdit(Self.QueryOrigin.Fields[vrVez], InternalDataSource, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody);

    // Pulando linha
        vrCurrentLine := vrReference.Bottom + FORM_MARGIN_TOP + FORM_MARGIN_BOTTOM;
  end;
end;

procedure TFCustomDatabaseForm.FromReference(prReference: TJupiterDatabaseReference);
var
  vrQry : TSQLQuery;
  vrDatabase : TJupiterDatabaseWizard;
begin
  Self.Caption := String.Format('%1:s #%0:d', [prReference.ID, JupiterStringUtilsNormalizeToPresent(prReference.TableName)]);

  vrDatabase := vrJupiterApp.NewWizard;
  try
    vrQry := vrDatabase.NewQueryFromReference(prReference);

    Self.FTableName := prReference.TableName;
    Self.FID        := prReference.ID;
  finally
    vrQry.Open;

    Self.QueryOrigin := vrQry;
  end;
end;

end.

