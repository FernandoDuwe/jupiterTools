unit jupiterformcomponenttils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ComCtrls, Controls, SysUtils, StdCtrls, jupiterformutils,
  JupiterConsts, jupiterDatabaseWizard, JupiterDataProvider, JupiterApp,
  jupiterDesktopApp, DBCtrls, DB, Menus, ExtCtrls;

  function JupiterComponentsAddPopupMenuSeparator(prMenu : TPopupMenu) : TJupiterComponentReference;

  function JupiterComponentsAddPopupMenuItem(prMenu : TPopupMenu; prTitle, prShortcut : String; prImageIndex : Integer) : TJupiterComponentReference;

  function JupiterComponentsNewLabel(prText : String; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewLink(prText : String; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewEdit(prInitialValue : String; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewComboBox(prDataProvider, prColumn : String; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewTrackBar(prValue, prMin, prMax : Integer; prPosition : TJupiterPosition; prOwner : TWinControl; prOnChange : TNotifyEvent) : TJupiterComponentReference;

  function JupiterComponentsNewProgressBar(prValue, prMin, prMax : Integer; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewCheckBox(prCaption : String; prValue : Boolean; prPosition : TJupiterPosition; prOwner : TWinControl; prOnChange : TNotifyEvent) : TJupiterComponentReference;

  // Componentes de banco de dados
  function JupiterComponentsNewDBEdit(prField : TField; prDataSource : TDataSource; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewDBDatePicker(prField : TField; prDataSource : TDataSource; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewDBMemo(prField : TField; prDataSource : TDataSource; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewDBComboBox(prField : TField; prDataSource : TDataSource; prPosition : TJupiterPosition; prOwner : TWinControl; prForeignKeyData : TJupiterDatabaseForeignKeyReference) : TJupiterComponentReference;

  function JupiterComponentsNewDBCheckBox(prField : TField; prDataSource : TDataSource; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsAddAction(prField : TJupiterComponentReference; prIcon : Integer; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsAddLine(prPosition : TJupiterPosition; prHeight, prWidth: Integer; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewTile(prTitle : String; prCounter : Integer; prHeight, prWidth: Integer; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewListItem(prTitle, prSubtitle : String; prOwner : TWinControl) : TJupiterComponentReference;

implementation

uses Buttons, DBDateTimePicker, SQLDB, DateTimePicker, Graphics, LCLProc, JupiterEdit, jupiterStringUtils;

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

  if Assigned(prMenu) then
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
  vrLabel.Transparent := True;

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
  vrLabel.Transparent := True;

  Result := TJupiterComponentReference.Create(prPosition.Top,
                                              prPosition.Left,
                                              prPosition.Left + vrLabel.Width,
                                              prPosition.Top + vrLabel.Height,
                                              vrLabel);
end;

function JupiterComponentsNewEdit(prInitialValue: String; prPosition: TJupiterPosition; prOwner: TWinControl): TJupiterComponentReference;
var
  vrEdit : TJupiterEdit;
begin
  vrEdit := TJupiterEdit.Create(prOwner);
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
                                              vrEdit,
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
                                              vrEdit,
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

function JupiterComponentsNewProgressBar(prValue, prMin, prMax: Integer; prPosition: TJupiterPosition; prOwner: TWinControl): TJupiterComponentReference;
var
  vrProgressBar : TProgressBar;
begin
  vrProgressBar           := TProgressBar.Create(prOwner);
  vrProgressBar.Parent    := prOwner;
  vrProgressBar.AutoSize  := True;
  vrProgressBar.Min       := prMin;
  vrProgressBar.Max       := prMax;
  vrProgressBar.Position  := prValue;
  vrProgressBar.Font.Size := GetFontSize;
  vrProgressBar.Top       := prPosition.Top;
  vrProgressBar.Left      := prPosition.Left;
  vrProgressBar.Width     := prOwner.Width - prPosition.Left - FORM_MARGIN_RIGHT;
  vrProgressBar.Anchors   := [akTop, akLeft, akRight];

  Result := TJupiterComponentReference.Create(prPosition.Top,
                                              prPosition.Left,
                                              prPosition.Left + vrProgressBar.Width,
                                              prPosition.Top + vrProgressBar.Height,
                                              vrProgressBar);
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
                                              vrEdit,
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
                                              vrEdit,
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
                                              vrEdit,
                                              vrEdit);
end;

function JupiterComponentsNewDBComboBox(prField: TField; prDataSource: TDataSource; prPosition: TJupiterPosition; prOwner: TWinControl; prForeignKeyData : TJupiterDatabaseForeignKeyReference): TJupiterComponentReference;
var
  vrEdit : TDBLookupComboBox;
  vrQry  : TSQLQuery;
begin
  vrQry := TJupiterDatabaseWizard(prForeignKeyData.Wizard).NewQueryFromReference(TJupiterDatabaseReference.Create(prForeignKeyData.TableDestinyName, NULL_KEY), EmptyStr, ' 2 ',
                                                                                 'ID, ' + TJupiterDatabaseWizard(prForeignKeyData.Wizard).GetDescriptionFieldFromTable(prForeignKeyData.TableDestinyName));

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
  vrEdit.Style      := csDropDownList;

  vrQry.Open;

  vrEdit.ListSource     := TJupiterDatabaseWizard(prForeignKeyData.Wizard).NewDataSourceFromQuery(vrQry);
  vrEdit.KeyField       := prForeignKeyData.FieldDestinyName;
  vrEdit.ListFieldIndex := 1;
  vrEdit.ListField      := vrQry.Fields[1].FieldName;

  if vrJupiterApp.GlobalReferenceExists(prForeignKeyData.TableDestinyName) then
    if vrQry.Locate('ID', vrJupiterApp.GetGlobalReference(prForeignKeyData.TableDestinyName).ID, []) then
    begin
      if prDataSource.State in [dsInsert, dsEdit] then
      begin
        prField.AsInteger := vrQry.FieldByName('ID').AsInteger;
        vrEdit.KeyValue   := vrQry.FieldByName('ID').AsInteger;
      end;
    end;

  vrQry := nil;

  Result := TJupiterComponentReference.Create(prPosition.Top,
                                              prPosition.Left,
                                              prPosition.Left + vrEdit.Width,
                                              prPosition.Top + vrEdit.Height,
                                              vrEdit,
                                              vrEdit);
end;

function JupiterComponentsNewDBCheckBox(prField: TField; prDataSource: TDataSource; prPosition: TJupiterPosition; prOwner: TWinControl): TJupiterComponentReference;
var
  vrEdit : TDBCheckBox;
begin
  vrEdit := TDBCheckBox.Create(prOwner);
  vrEdit.Caption      := JupiterStringUtilsNormalizeToPresent(prField.DisplayName);
  vrEdit.Parent       := prOwner;
  vrEdit.AutoSize     := True;
  vrEdit.Font.Size    := GetFontSize;
  vrEdit.Top          := prPosition.Top;
  vrEdit.Left         := prPosition.Left;
  vrEdit.DataSource   := prDataSource;
  vrEdit.DataField    := prField.FieldName;
  vrEdit.AutoSize     := False;
  vrEdit.Anchors      := [akTop, akLeft];
  vrEdit.ValueChecked := '1';
  vrEdit.ValueChecked := '0';

  Result := TJupiterComponentReference.Create(prPosition.Top,
                                              prPosition.Left,
                                              prPosition.Left + vrEdit.Width,
                                              prPosition.Top + vrEdit.Height,
                                              vrEdit,
                                              vrEdit);

end;

function JupiterComponentsAddAction(prField: TJupiterComponentReference;
  prIcon: Integer; prOwner: TWinControl): TJupiterComponentReference;
var
  vrSpeedButton : TSpeedButton;
begin
  vrSpeedButton := TSpeedButton.Create(prField.Component);
  vrSpeedButton.Parent := prOwner;
  vrSpeedButton.Caption := EmptyStr;
  vrSpeedButton.Top := prField.Top;
  vrSpeedButton.Height := prField.Bottom - prField.Top;
  vrSpeedButton.Width := vrSpeedButton.Height;
  vrSpeedButton.Left := prField.RightCalc - vrSpeedButton.Width;
  vrSpeedButton.Flat := True;
  vrSpeedButton.Images := TJupiterDesktopApp(vrJupiterApp).ImageList;
  vrSpeedButton.ImageIndex := prIcon;
  vrSpeedButton.Anchors    := [akTop, akRight];

  if Assigned(prField.WinControl) then
    prField.WinControl.Width := prField.WinControl.Width - (vrSpeedButton.Width);

  Result := TJupiterComponentReference.Create(vrSpeedButton.Top,
                                              vrSpeedButton.Left,
                                              vrSpeedButton.Left + vrSpeedButton.Width,
                                              vrSpeedButton.Top + vrSpeedButton.Height,
                                              vrSpeedButton);
end;

function JupiterComponentsAddLine(prPosition : TJupiterPosition; prHeight, prWidth: Integer; prOwner : TWinControl): TJupiterComponentReference;
var
  vrShape : TShape;
begin
  vrShape             := TShape.Create(prOwner);
  vrShape.Parent      := prOwner;
  vrShape.AutoSize    := True;
  vrShape.Top         := prPosition.Top;
  vrShape.Left        := prPosition.Left;
  vrShape.Height      := prHeight;
  vrShape.Width       := prWidth;

  if prHeight <= 1 then
    vrShape.Anchors := [akTop, akLeft, akRight];

  Result := TJupiterComponentReference.Create(vrShape.Top,
                                              vrShape.Left,
                                              vrShape.Left + vrShape.Width,
                                              vrShape.Top + vrShape.Height,
                                              vrShape);
end;

function JupiterComponentsNewTile(prTitle: String; prCounter: Integer; prHeight, prWidth: Integer; prPosition: TJupiterPosition; prOwner: TWinControl): TJupiterComponentReference;
var
  vrPanel : TPanel;
begin
  vrPanel             := TPanel.Create(prOwner);
  vrPanel.Parent      := prOwner;
  vrPanel.AutoSize    := False;
  vrPanel.Top         := prPosition.Top;
  vrPanel.Left        := prPosition.Left;
  vrPanel.Height      := prHeight;
  vrPanel.Width       := prWidth;
  vrPanel.BevelOuter  := bvNone;
  vrPanel.Font.Size   := GetFontSize;
  vrPanel.ParentBackground := False;
  vrPanel.ParentColor := False;

  vrPanel.Anchors := [akTop, akLeft, akRight];

  JupiterComponentsNewLabel(prTitle, TJupiterPosition.Create(FORM_MARGIN_TOP, FORM_MARGIN_LEFT), vrPanel);

  if prCounter >= 0 then
    JupiterComponentsNewLabel(IntToStr(prCounter),
                              TJupiterPosition.Create((prHeight - FORM_MARGIN_TOP) - GetTextHeight(IntToStr(prCounter), vrPanel.Font),
                                                      (prWidth - FORM_MARGIN_RIGHT) - GetTextWidth(IntToStr(prCounter), vrPanel.Font)),
                              vrPanel);

  Result := TJupiterComponentReference.Create(vrPanel.Top,
                                              vrPanel.Left,
                                              vrPanel.Left + vrPanel.Width,
                                              vrPanel.Top + vrPanel.Height,
                                              vrPanel);
end;

function JupiterComponentsNewListItem(prTitle, prSubtitle: String; prOwner: TWinControl): TJupiterComponentReference;
var
  vrPanel : TPanel;
  vrLabel : TJupiterComponentReference;
begin
  vrPanel             := TPanel.Create(prOwner);
  vrPanel.Parent      := prOwner;
  vrPanel.Align       := alTop;
  vrPanel.BevelOuter  := bvNone;
  vrPanel.Font.Size   := GetFontSize;
  vrPanel.ParentBackground := False;
  vrPanel.ParentColor := False;

  vrPanel.Anchors := [akTop, akLeft, akRight];

  vrLabel := JupiterComponentsNewLabel(prTitle, TJupiterPosition.Create(FORM_MARGIN_TOP, FORM_MARGIN_LEFT), vrPanel);

  TLabel(vrLabel.Component).Font.Style := [fsBold];

  vrPanel.Height := vrLabel.Bottom + FORM_MARGIN_BOTTOM;

  vrLabel := JupiterComponentsNewLabel(prSubtitle, TJupiterPosition.Create(vrPanel.Height, FORM_MARGIN_LEFT), vrPanel);

  vrPanel.Height := vrLabel.Bottom + FORM_MARGIN_BOTTOM;

  Result := TJupiterComponentReference.Create(vrPanel.Top,
                                              vrPanel.Left,
                                              vrPanel.Left + vrPanel.Width,
                                              vrPanel.Top + vrPanel.Height,
                                              vrPanel);
end;

end.

