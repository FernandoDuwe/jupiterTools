unit jupiterformcomponenttils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, SysUtils, StdCtrls, jupiterformutils, JupiterConsts,
  DBCtrls, DB;

  function JupiterComponentsNewLabel(prText : String; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewDBEdit(prField : TField; prDataSource : TDataSource; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewDBDatePicker(prField : TField; prDataSource : TDataSource; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

  function JupiterComponentsNewDBMemo(prField : TField; prDataSource : TDataSource; prPosition : TJupiterPosition; prOwner : TWinControl) : TJupiterComponentReference;

implementation

uses DBDateTimePicker;

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
  vrEdit.Height     := GetTextHeight('OI', vrEdit.Font) * 10;
  vrEdit.Width      := prOwner.Width - prPosition.Left - FORM_MARGIN_RIGHT;
  vrEdit.Anchors    := [akTop, akLeft, akRight];

  Result := TJupiterComponentReference.Create(prPosition.Top,
                                              prPosition.Left,
                                              prPosition.Left + vrEdit.Width,
                                              prPosition.Top + vrEdit.Height,
                                              vrEdit);
end;

end.

