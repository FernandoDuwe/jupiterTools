unit JupiterFormTabSheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls;

type

  { TJupiterFormTabSheet }

  TJupiterFormTabSheet = class(TTabSheet)
  private
    FForm : TForm;
    procedure Internal_SetForm(prForm : TForm);
  protected
    FCloseButtonRect : TRect;
    FOnClose         : TNotifyEvent;

    procedure DoClose; virtual;
  published
    property Form : TForm read FForm write Internal_SetForm;
  public
    property OnClose : TNotifyEvent read FOnClose write FOnClose;

    procedure Update;

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  end;

procedure Register;

implementation

uses JupiterFormTab;

procedure Register;
begin
  RegisterComponents('Jupiter', [TJupiterFormTabSheet]);
end;

{ TJupiterFormTabSheet }

procedure TJupiterFormTabSheet.Internal_SetForm(prForm: TForm);
begin
  Self.FForm := prForm;

  Self.FForm.Parent      := Self;
  Self.FForm.Left        := 0;
  Self.FForm.Top         := 0;
  Self.FForm.WindowState := wsMaximized;
  Self.FForm.BorderStyle := bsNone;

  Self.Caption := Self.FForm.Caption + '        ';

  Self.FForm.Show;
end;

procedure TJupiterFormTabSheet.DoClose;
begin
  if Assigned(FOnClose) then FOnClose(Self);
    Free;
end;

procedure TJupiterFormTabSheet.Update;
begin
  Self.Caption := Self.FForm.Caption + '        ';

  if Self.PageControl is TJupiterFormTab then
    TJupiterFormTab(Self.PageControl).UpdateComboBox;
end;

constructor TJupiterFormTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCloseButtonRect := Rect(0, 0, 0, 0);
end;

destructor TJupiterFormTabSheet.Destroy;
begin
  if Assigned(Self.FForm) then
  begin
    Self.Caption := 'Fechando...';

    Self.FForm.Close;
    Self.FForm.Release;

    FreeAndNil(Self.FForm);
  end;

  inherited Destroy;
end;

end.
