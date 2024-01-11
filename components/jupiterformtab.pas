unit JupiterFormTab;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, LCLType, Types, LCLIntf;

type
  TJupiterFormTabOnPrepareForm = procedure(var vrForm : TForm; prTab : TObject) of object;
  TJupiterFormTabOnCloseTab = procedure(Sender : TObject) of object;
  TJupiterFormTabOnChangeFromComboBox = procedure(Sender : TObject) of object;

  { TJupiterFormTab }

  TJupiterFormTab = class(TPageControl)
  const btnSize = 13;
  private
    procedure Internal_UpdateComboBox;
    procedure Internal_ComboBoxChange(Sender : TObject);
  protected
    FAlwaysHaveForm : Boolean;
    FOnPrepareForm : TJupiterFormTabOnPrepareForm;
    FOnCloseTab : TJupiterFormTabOnCloseTab;
    FOnChangeFromComboBox : TJupiterFormTabOnChangeFromComboBox;
    FComboBox : TComboBox;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintWindow(DC: HDC); override;
    procedure DoChange; override;
  published
    property AlwaysHaveForm : Boolean                      read FAlwaysHaveForm write FAlwaysHaveForm;
    property ComboBox       : TComboBox                    read FComboBox       write FComboBox;
    property OnPrepareForm  : TJupiterFormTabOnPrepareForm read FOnPrepareForm  write FOnPrepareForm;
    property OnCloseTab     : TJupiterFormTabOnCloseTab    read FOnCloseTab     write FOnCloseTab;

    property OnChangeFromComboBox : TJupiterFormTabOnChangeFromComboBox read FOnChangeFromComboBox write FOnChangeFromComboBox;
  public
    procedure AddForm(prForm : TForm);

    procedure UpdateComboBox;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  end;

procedure Register;

implementation

uses JupiterFormTabSheet;

procedure Register;
begin
  RegisterComponents('Jupiter', [TJupiterFormTab]);
end;

{ TJupiterFormTab }

procedure TJupiterFormTab.Internal_UpdateComboBox;
var
  vrVez : Integer;
begin
  if not Assigned(Self.ComboBox) then
    Exit;

  Self.ComboBox.OnChange := nil;
  Self.ComboBox.Clear;

  for vrVez := 0 to Self.PageCount - 1 do
    Self.ComboBox.Items.Add(Self.Pages[vrVez].Caption);

  Self.ComboBox.ItemIndex := Self.PageIndex;
  Self.ComboBox.OnChange := @Internal_ComboBoxChange;
end;

procedure TJupiterFormTab.Internal_ComboBoxChange(Sender: TObject);
begin
  if not (Sender is TComboBox) then
    Exit;

  Self.PageIndex := TComboBox(Sender).ItemIndex;

  if Assigned(OnChangeFromComboBox) then
    Self.OnChangeFromComboBox(Self);
end;

procedure TJupiterFormTab.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var R : TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbLeft then
  begin
    R := TabRect(ActivePageIndex);

    if ((PageCount = 1) and (Self.AlwaysHaveForm)) then
      Exit;

    if PtInRect(Classes.Rect(R.Right - btnSize - 4, R.Top + 2, R.Right - 4, R.Top + btnSize + 2), Classes.Point(X, Y)) then
      ActivePage.Free;

    Self.Internal_UpdateComboBox;

    if Assigned(Self.OnCloseTab) then
      Self.OnCloseTab(Self);
  end;
end;

procedure TJupiterFormTab.PaintWindow(DC: HDC);
var
  i : integer;
  R : TRect;
  bm : TBitmap;
begin
  inherited PaintWindow(DC);

  if ((PageCount = 1) and (Self.AlwaysHaveForm)) then
    Exit;

  bm := TBitmap.Create;
  try
    bm.SetSize(24, 24);
    Images.GetBitmap(0, bm);

    for i := 0 to Pred(PageCount) do
    begin
      R := TabRect(i);

      StretchBlt(DC, R.Right - btnSize - 4, R.Top + 2,
                 btnSize, btnSize, bm.Canvas.Handle, 0, 0, 16, 16, cmSrcCopy);
    end;
  finally
    bm.Free;
  end;
end;

procedure TJupiterFormTab.DoChange;
begin
  inherited DoChange;

  Self.Internal_UpdateComboBox;
end;

procedure TJupiterFormTab.AddForm(prForm: TForm);
var
  vrTab : TJupiterFormTabSheet;
begin
  vrTab := TJupiterFormTabSheet.Create(Self);
  try
    vrTab.PageControl := Self;
    vrTab.Form := prForm;

    if Assigned(Self.FOnPrepareForm) then
       Self.FOnPrepareForm(prForm, vrTab);
  finally
    Self.PageIndex := Self.PageCount - 1;

    Internal_UpdateComboBox;
  end;
end;

procedure TJupiterFormTab.UpdateComboBox;
begin
  Self.Internal_UpdateComboBox;
end;

constructor TJupiterFormTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TJupiterFormTab.Destroy;
begin
  inherited Destroy;
end;

end.
