unit jupiterformdbcomponenttils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, DB, DBCtrls, SysUtils, JupiterConsts, Graphics,
  jupiterformcomponenttils, jupiterformutils, jupiterDatabaseWizard, JupiterApp,
  uJupiterDatabaseScript, StdCtrls;

  function JupiterFormDBComponent_NewTextEdit(prTableName : String; prField : TField; prDataSource : TDataSource; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterFormDBComponent_NewDateTextMemoEdit(prTableName : String; prField : TField; prDataSource : TDataSource; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterFormDBComponent_NewDateTimeTextEdit(prTableName : String; prField : TField; prDataSource : TDataSource; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterFormDBComponent_NewDateBooleanEdit(prTableName : String; prField : TField; prDataSource : TDataSource; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterFormDBComponent_NewForeignKeyEdit(prTableName : String; prField : TField; prDataSource : TDataSource; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

implementation

function JupiterFormDBComponent_NewTextEdit(prTableName : String; prField: TField; prDataSource : TDataSource; prPosition: TJupiterPosition; prOwner : TWinControl): TJupiterComponentReference;
var
  vrReference : TJupiterPosition;
begin
  Result := JupiterComponentsNewLabel(JupiterDatabaseScript_GetDescription(prTableName, prField.FieldName),
                                      prPosition, prOwner);

  if vrJupiterApp.Params.VariableById('Interface.Form.Label.Bold').AsBool then
    TLabel(Result.Component).Font.Style := [fsBold];

  if vrJupiterApp.Params.VariableById('Interface.Form.AsList').AsBool then
    vrReference := TJupiterPosition.Create(Result.Bottom + FORM_MARGIN_BOTTOM_LABEL, FORM_MARGIN_LEFT)
  else
  begin
    TLabel(Result.Component).Top       := TLabel(Result.Component).Top + 5;
    TLabel(Result.Component).AutoSize  := False;
    TLabel(Result.Component).Width     := vrJupiterApp.Params.VariableById('Interface.Form.Label.Size').AsInteger - prPosition.Left - FORM_MARGIN_LEFT;
    TLabel(Result.Component).Alignment := taRightJustify;

    vrReference := TJupiterPosition.Create(Result.Top, vrJupiterApp.Params.VariableById('Interface.Form.Label.Size').AsInteger + FORM_MARGIN_RIGHT);
  end;

  Result := JupiterComponentsNewDBEdit(prField, prDataSource, vrReference, prOwner);
end;

function JupiterFormDBComponent_NewDateTextMemoEdit(prTableName: String; prField: TField; prDataSource: TDataSource; prPosition: TJupiterPosition; prOwner: TWinControl): TJupiterComponentReference;
var
  vrReference : TJupiterPosition;
begin
  Result := JupiterComponentsNewLabel(JupiterDatabaseScript_GetDescription(prTableName, prField.FieldName),
                                      prPosition, prOwner);

  if vrJupiterApp.Params.VariableById('Interface.Form.Label.Bold').AsBool then
    TLabel(Result.Component).Font.Style := [fsBold];

  if vrJupiterApp.Params.VariableById('Interface.Form.AsList').AsBool then
    vrReference := TJupiterPosition.Create(Result.Bottom + FORM_MARGIN_BOTTOM_LABEL, FORM_MARGIN_LEFT)
  else
  begin
    TLabel(Result.Component).Top       := TLabel(Result.Component).Top + 5;
    TLabel(Result.Component).AutoSize  := False;
    TLabel(Result.Component).Width     := vrJupiterApp.Params.VariableById('Interface.Form.Label.Size').AsInteger - prPosition.Left - FORM_MARGIN_LEFT;
    TLabel(Result.Component).Alignment := taRightJustify;

    vrReference := TJupiterPosition.Create(Result.Top, vrJupiterApp.Params.VariableById('Interface.Form.Label.Size').AsInteger + FORM_MARGIN_RIGHT);
  end;

  Result := JupiterComponentsNewDBMemo(prField, prDataSource, vrReference, prOwner);
end;

function JupiterFormDBComponent_NewDateTimeTextEdit(prTableName: String; prField: TField; prDataSource: TDataSource; prPosition: TJupiterPosition; prOwner: TWinControl): TJupiterComponentReference;
var
  vrReference : TJupiterPosition;
begin
  Result := JupiterComponentsNewLabel(JupiterDatabaseScript_GetDescription(prTableName, prField.FieldName),
                                      prPosition, prOwner);

  if vrJupiterApp.Params.VariableById('Interface.Form.Label.Bold').AsBool then
    TLabel(Result.Component).Font.Style := [fsBold];

  if vrJupiterApp.Params.VariableById('Interface.Form.AsList').AsBool then
    vrReference := TJupiterPosition.Create(Result.Bottom + FORM_MARGIN_BOTTOM_LABEL, FORM_MARGIN_LEFT)
  else
  begin
    TLabel(Result.Component).Top       := TLabel(Result.Component).Top + 5;
    TLabel(Result.Component).AutoSize  := False;
    TLabel(Result.Component).Width     := vrJupiterApp.Params.VariableById('Interface.Form.Label.Size').AsInteger - prPosition.Left - FORM_MARGIN_LEFT;
    TLabel(Result.Component).Alignment := taRightJustify;

    vrReference := TJupiterPosition.Create(Result.Top, vrJupiterApp.Params.VariableById('Interface.Form.Label.Size').AsInteger + FORM_MARGIN_RIGHT);
  end;

  Result := JupiterComponentsNewDBDatePicker(prField, prDataSource, vrReference, prOwner);
end;

function JupiterFormDBComponent_NewDateBooleanEdit(prTableName: String; prField: TField; prDataSource: TDataSource; prPosition: TJupiterPosition; prOwner: TWinControl): TJupiterComponentReference;
begin
  if not vrJupiterApp.Params.VariableById('Interface.Form.AsList').AsBool then
    prPosition.Left := vrJupiterApp.Params.VariableById('Interface.Form.Label.Size').AsInteger + FORM_MARGIN_RIGHT;

  Result := JupiterComponentsNewDBCheckBox(prField, prDataSource, prPosition, prOwner);
end;

function JupiterFormDBComponent_NewForeignKeyEdit(prTableName: String; prField: TField; prDataSource: TDataSource; prPosition: TJupiterPosition; prOwner: TWinControl): TJupiterComponentReference;
var
  vrWizard : TJupiterDatabaseWizard;
  vrReference : TJupiterPosition;
begin
  vrWizard := vrJupiterApp.NewWizard;
  try
    Result := JupiterComponentsNewLabel(JupiterDatabaseScript_GetDescription(prTableName, prField.FieldName),
                                        prPosition, prOwner);

    if vrJupiterApp.Params.VariableById('Interface.Form.Label.Bold').AsBool then
      TLabel(Result.Component).Font.Style := [fsBold];

    if vrJupiterApp.Params.VariableById('Interface.Form.AsList').AsBool then
       vrReference := TJupiterPosition.Create(Result.Bottom + FORM_MARGIN_BOTTOM_LABEL, FORM_MARGIN_LEFT)
    else
    begin
      TLabel(Result.Component).Top       := TLabel(Result.Component).Top + 5;
      TLabel(Result.Component).AutoSize  := False;
      TLabel(Result.Component).Width     := vrJupiterApp.Params.VariableById('Interface.Form.Label.Size').AsInteger - prPosition.Left - FORM_MARGIN_LEFT;
      TLabel(Result.Component).Alignment := taRightJustify;

      vrReference := TJupiterPosition.Create(Result.Top, vrJupiterApp.Params.VariableById('Interface.Form.Label.Size').AsInteger + FORM_MARGIN_RIGHT);
    end;

    Result := JupiterComponentsNewDBComboBox(prField,
                                             prDataSource,
                                             vrReference,
                                             prOwner,
                                             vrWizard.GetForeignKeyData(prTableName, prField.FieldName));
  finally
    FreeAndNil(vrWizard);
  end;
end;

end.

