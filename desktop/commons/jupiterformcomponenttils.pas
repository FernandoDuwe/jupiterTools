unit jupiterformcomponenttils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ComCtrls, Controls, SysUtils, StdCtrls, jupiterformutils, JupiterConsts,
  jupiterDatabaseWizard, JupiterDataProvider, JupiterApp, DBCtrls, DB, Menus;

  function JupiterComponentsAddPopupMenuSeparator(prMenu : TPopupMenu) : TJupiterComponentReference;

  function JupiterComponentsAddPopupMenuItem(prMenu : TPopupMenu; prTitle, prShortcut : String; prImageIndex : Integer) : TJupiterComponentReference;

  function JupiterComponentsNewLabel(prText : String; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewLink(prText : String; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewEdit(prInitialValue : String; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewComboBox(prDataProvider, prColumn : String; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewTrackBar(prValue, prMin, prMax : Integer; prPosition : TJupiterPosition; prOwner : TWinControl; prOnChange : TNotifyEvent) : TJupiterComponentReference;

  function JupiterComponentsNewCheckBox(prCaption : String; prValue : Boolean; prPosition : TJupiterPosition; prOwner : TWinControl; prOnChange : TNotifyEvent) : TJupiterComponentReference;

  // Componentes de banco de dados
  function JupiterComponentsNewDBEdit(prField : TField; prDataSource : TDataSource; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewDBDatePicker(prField : TField; prDataSource : TDataSource; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewDBMemo(prField : TField; prDataSource : TDataSource; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewDBComboBox(prField : TField; prDataSource : TDataSource; prPosition : TJupiterPosition; prOwner : TWinControl; prForeignKeyData : TJupiterDatabaseForeignKeyReference) : TJupiterComponentReference;

implementation

uses DBDateTimePicker, SQLDB, DateTimePicker, Graphics, LCLProc;

function JupiterComponentsAddPopupMenuSeparator(prMenu: TPopupMenu): TJupiterComponentReference;
var
  vrMenuItem : TMenuItem;
begin
  vrMenuItem := TMenuItem.Create(prMenu);
  vrMenuItem.Caption := '-';

  prMenu.Items.Add(vrMenuItem);

  Result := TJupiterComponentReference.Create(0, 0, 0, 0, vrMenuItem);
end;

function JupiterComponentsAddPopupMenuItem(prMenu: TPopupMenu; prTitle, prShortcut : String; prImageIndex : Integer): TJupiterComponentReference;
var
  vrMenuItem : TMenuItem;
begin
  vrMenuItem := TMenuItem.Create(prMenu);
  vrMenuItem.Caption := prTitle;
  vrMenuItem.ShortCut := TextToShortCut(prShortcut);

  if prImageIndex <> NULL_KEY then
    vrMenuItem.ImageIndex := prImageIndex;

  prMenu.Items.Add(vrMenuItem);

  Result := TJupiterComponentReference.Create(0, 0, 0, 0, vrMenuItem);
end;

function JupiterComponentsNewLabel(prText: String; prPosition : TJupiterPosition; prOwner : TWinControl): TJupiterComponentReference;
var
  vrLabel : TLabel;
begin
  vrLabel           := TLabel.Create(prOwner);
  vrLabel.Parent    := prOwner;
  vrLabel.AutoSize  := True;
  vrLabel.Caption   := prText;
  vrLabel.Font.Size := GetFontSize;
  vrLabel.Top       := prPosition.Top;
  vrLabel.Left      := prPosition.Left;

  Result := TJupiterComponentReference.Create(prPosition.Top,
                                              prPosition.Left,
                                              prPosition.Left + vrLabel.Width,
                                              prPosition.Top + vrLabel.Height,
                                              vrLabel);
end;

function JupiterComponentsNewLink(prText: String; prPosition: TJupiterPosition; prOwner: TWinControl): TJupiterComponentReference;
var
  vrLabel : TLabel;
begin
  vrLabel            := TLabel.Create(prOwner);
  vrLabel.Parent     := prOwner;
  vrLabel.AutoSize   := True;
  vrLabel.Caption    := prText;
  vrLabel.Font.Size  := GetFontSize;
  vrLabel.Top        := prPosition.Top;
  vrLabel.Left       := prPosition.Left;
  vrLabel.Font.Color := $00FD5F5F;
  vrLabel.Cursor     := crHandPoint;
  vrLabel.Hint       := 'Clique aqui para executar';
  vrLabel.ShowHint   := True;

  Result := TJupiterComponentReference.Create(prPosition.Top,
                                              prPosition.Left,
                                              prPosition.Left + vrLabel.Width,
                                              prPosition.Top + vrLabel.Height,
                                              vrLabel);
end;

function JupiterComponentsNewEdit(prInitialValue: String; prPosition: TJupiterPosition; prOwner: TWinControl): TJupiterComponentReference;
var
  vrEdit : TEdit;
begin
  vrEdit := TEdit.Create(prOwner);
  vrEdit.Parent     := prOwner;
  vrEdit.AutoSize   := True;
  vrEdit.Font.Size  := GetFontSize;
  vrEdit.Top        := prPosition.Top;
  vrEdit.Left       := prPosition.Left;
  vrEdit.AutoSize   := False;
  vrEdit.Width      := prOwner.Width - prPosition.Left - FORM_MARGIN_RIGHT;
  vrEdit.Anchors    := [akTop, akLeft, akRight];
  vrEdit.Text       := prInitialValue;

  Result := TJupiterComponentReference.Create(prPosition.Top,
                                              prPosition.Left,
                                              prPosition.Left + vrEdit.Width,
                                              prPosition.Top + vrEdit.Height,
                                              vrEdit);
end;

function JupiterComponentsNewComboBox(prDataProvider, prColumn: String; prPosition: TJupiterPosition; prOwner: TWinControl): TJupiterComponentReference;
var
  vrEdit : TComboBox;
  vrProvider : TJupiterDataProvider;
  vrVez : Integer;
begin
  vrProvider := vrJupiterApp.GetDataProviderById(prDataProvider);

  vrEdit := TComboBox.Create(prOwner);
  vrEdit.Parent     := prOwner;
  vrEdit.AutoSize   := True;
  vrEdit.Font.Size  := GetFontSize;
  vrEdit.Top        := prPosition.Top;
  vrEdit.Left       := prPosition.Left;
  vrEdit.AutoSize   := False;
  vrEdit.Width      := prOwner.Width - prPosition.Left - FORM_MARGIN_RIGHT;
  vrEdit.Anchors    := [akTop, akLeft, akRight];

  for vrVez := 0 to vrProvider.Count - 1 do
    with TJupiterDataProviderRow(vrProvider.GetRowByIndex(vrVez)) do
      vrEdit.Items.Add(Fields.VariableById(prColumn).Value);

  Result := TJupiterComponentReference.Create(prPosition.Top,
                                              prPosition.Left,
                                              prPosition.Left + vrEdit.Width,
                                              prPosition.Top + vrEdit.Height,
                                              vrEdit);
end;

function JupiterComponentsNewTrackBar(prValue, prMin, prMax : Integer; prPosition: TJupiterPosition; prOwner: TWinControl; prOnChange : TNotifyEvent): TJupiterComponentReference;
var
  vrTrackBar : TTrackBar;
begin
  vrTrackBar           := TTrackBar.Create(prOwner);
  vrTrackBar.Parent    := prOwner;
  vrTrackBar.AutoSize  := True;
  vrTrackBar.Min       := prMin;
  vrTrackBar.Max       := prMax;
  vrTrackBar.Position  := prValue;
  vrTrackBar.Font.Size := GetFontSize;
  vrTrackBar.Top       := prPosition.Top;
  vrTrackBar.Left      := prPosition.Left;
  vrTrackBar.Width     := prOwner.Width - prPosition.Left - FORM_MARGIN_RIGHT;
  vrTrackBar.Anchors   := [akTop, akLeft, akRight];
  vrTrackBar.OnChange  := prOnChange;

  Result := TJupiterComponentReference.Create(prPosition.Top,
                                              prPosition.Left,
                                              prPosition.Left + vrTrackBar.Width,
                                              prPosition.Top + vrTrackBar.Height,
                                              vrTrackBar);
end;

function JupiterComponentsNewCheckBox(prCaption : String; prValue: Boolean; prPosition: TJupiterPosition; prOwner: TWinControl; prOnChange: TNotifyEvent): TJupiterComponentReference;
var
  vrCheckBox : TCheckBox;
begin
  vrCheckBox           := TCheckBox.Create(prOwner);
  vrCheckBox.Parent    := prOwner;
  vrCheckBox.Checked   := prValue;
  vrCheckBox.Caption   := prCaption;
  vrCheckBox.Font.Size := GetFontSize;
  vrCheckBox.Top       := prPosition.Top;
  vrCheckBox.Left      := prPosition.Left;
  vrCheckBox.OnChange  := prOnChange;

  Result := TJupiterComponentReference.Create(prPosition.Top,
                                              prPosition.Left,
                                              prPosition.Left + vrCheckBox.Width,
                                              prPosition.Top + vrCheckBox.Height,
                                              vrCheckBox);
end;

function JupiterComponentsNewDBEdit(prField : TField; prDataSource : TDataSource; prPosition : TJupiterPosition; prOwner : TWinControl): TJupiterComponentReference;
var
  vrEdit : TDBEdit;
begin
  vrEdit := TDBEdit.Create(prOwner);
  vrEdit.Parent     := prOwner;
  vrEdit.AutoSize   := True;
  vrEdit.Font.Size  := GetFontSize;
  vrEdit.Top        := prPosition.Top;
  vrEdit.Left       := prPosition.Left;
  vrEdit.DataSource := prDataSource;
  vrEdit.DataField  := prField.FieldName;
  vrEdit.AutoSize   := False;
  vrEdit.Width      := prOwner.Width - prPosition.Left - FORM_MARGIN_RIGHT;
  vrEdit.Anchors    := [akTop, akLeft, akRight];

  Result := TJupiterComponentReference.Create(prPosition.Top,
                                              prPosition.Left,
                                              prPosition.Left + vrEdit.Width,
                                              prPosition.Top + vrEdit.Height,
                                              vrEdit);

end;

function JupiterComponentsNewDBDatePicker(prField: TField; prDataSource: TDataSource; prPosition: TJupiterPosition; prOwner: TWinControl): TJupiterComponentReference;
var
  vrEdit : TDBDateTimePicker;
begin
  vrEdit := TDBDateTimePicker.Create(prOwner);
  vrEdit.Parent     := prOwner;
  vrEdit.AutoSize   := True;
  vrEdit.Font.Size  := GetFontSize;
  vrEdit.Top        := prPosition.Top;
  vrEdit.Left       := prPosition.Left;
  vrEdit.DataSource := prDataSource;
  vrEdit.DataField  := prField.FieldName;
  vrEdit.AutoSize   := False;
  vrEdit.Width      := prOwner.Width - prPosition.Left - FORM_MARGIN_RIGHT;
  vrEdit.Anchors    := [akTop, akLeft, akRight];

  if prField is TDateField then
    vrEdit.Kind := dtkDate
  else
    if prField is TTimeField then
      vrEdit.Kind := dtkTime
    else
      vrEdit.Kind := dtkDateTime;

  Result := TJupiterComponentReference.Create(prPosition.Top,
                                              prPosition.Left,
                                              prPosition.Left + vrEdit.Width,
                                              prPosition.Top + vrEdit.Height,
                                              vrEdit);
end;

function JupiterComponentsNewDBMemo(prField: TField; prDataSource: TDataSource; prPosition: TJupiterPosition; prOwner: TWinControl): TJupiterComponentReference;
var
  vrEdit : TDBMemo;
begin
  vrEdit := TDBMemo.Create(prOwner);
  vrEdit.Parent     := prOwner;
  vrEdit.AutoSize   := True;
  vrEdit.Font.Size  := GetFontSize;
  vrEdit.Top        := prPosition.Top;
  vrEdit.Left       := prPosition.Left;
  vrEdit.DataSource := prDataSource;
  vrEdit.DataField  := prField.FieldName;
  vrEdit.AutoSize   := False;
  vrEdit.Height     := GetTextHeight('OI', vrEdit.Font) * 20;
  vrEdit.Width      := prOwner.Width - prPosition.Left - FORM_MARGIN_RIGHT;
  vrEdit.Anchors    := [akTop, akLeft, akRight];
  vrEdit.ScrollBars := ssBoth;

  Result := TJupiterComponentReference.Create(prPosition.Top,
                                              prPosition.Left,
                                              prPosition.Left + vrEdit.Width,
                                              prPosition.Top + vrEdit.Height,
                                              vrEdit);
end;

function JupiterComponentsNewDBComboBox(prField: TField; prDataSource: TDataSource; prPosition: TJupiterPosition; prOwner: TWinControl; prForeignKeyData : TJupiterDatabaseForeignKeyReference): TJupiterComponentReference;
var
  vrEdit : TDBLookupComboBox;
  vrQry  : TSQLQuery;
begin
  vrQry := TJupiterDatabaseWizard(prForeignKeyData.Wizard).NewQueryFromReference(TJupiterDatabaseReference.Create(prForeignKeyData.TableDestinyName, NULL_KEY));

  vrEdit := TDBLookupComboBox.Create(prOwner);
  vrEdit.Parent     := prOwner;
  vrEdit.AutoSize   := True;
  vrEdit.Font.Size  := GetFontSize;
  vrEdit.Top        := prPosition.Top;
  vrEdit.Left       := prPosition.Left;
  vrEdit.DataSource := prDataSource;
  vrEdit.DataField  := prField.FieldName;
  vrEdit.AutoSize   := False;
  vrEdit.Width      := prOwner.Width - prPosition.Left - FORM_MARGIN_RIGHT;
  vrEdit.Anchors    := [akTop, akLeft, akRight];

  vrQry.Open;

  vrEdit.ListSource     := TJupiterDatabaseWizard(prForeignKeyData.Wizard).NewDataSourceFromQuery(vrQry);
  vrEdit.KeyField       := prForeignKeyData.FieldDestinyName;
  vrEdit.ListFieldIndex := 1;
  vrEdit.ListField      := vrQry.Fields[1].FieldName;

  vrQry := nil;

  Result := TJupiterComponentReference.Create(prPosition.Top,
                                              prPosition.Left,
                                              prPosition.Left + vrEdit.Width,
                                              prPosition.Top + vrEdit.Height,
                                              vrEdit);
end;

end.

