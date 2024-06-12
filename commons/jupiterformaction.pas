unit jupiterformaction;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, JupiterConsts, JupiterAction,
  Forms, ExtCtrls, Controls, StdCtrls, Graphics;

type

  { TJupiterFormAction }

  TJupiterFormAction = class(TJupiterObject)
  private
    FPanel       : TPanel;
    FAction      : TJupiterAction;
    FTabOrder    : Integer;
    FRaised      : Boolean;
    FActionColor : TColor;

    function Internal_CreatetTitle(prContainer: TPanel): TLabel;
    function Internal_CreatetSubTitle(prContainer: TPanel; prTop : Integer): TLabel;
    function Internal_CreateContainer(prOwner : TScrollBox) : TPanel;

    procedure Internal_OnClick(Sender: TObject);
  published
    property Action      : TJupiterAction read FAction      write FAction;
    property ActionColor : TColor         read FActionColor write FActionColor;
    property Panel       : TPanel         read FPanel       write FPanel;
    property Raised      : Boolean        read FRaised      write FRaised;
    property TabOrder    : Integer        read FTabOrder    write FTabOrder default 1;
  public
    procedure Draw(prOwner : TScrollBox);
  end;

implementation

uses JupiterApp;

{ TJupiterFormAction }

function TJupiterFormAction.Internal_CreatetTitle(prContainer: TPanel): TLabel;
begin
  Result            := TLabel.Create(prContainer);
  Result.Parent     := prContainer;
  Result.Font.Color := clBlue;
  Result.Cursor     := crHandPoint;
  Result.Top        := FORM_MARGIN_TOP;
  Result.Left       := FORM_MARGIN_LEFT;
  Result.Caption    := Self.Action.Title;
  Result.Font.Size  := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
  Result.OnClick    := @Self.Internal_OnClick;
end;

function TJupiterFormAction.Internal_CreatetSubTitle(prContainer: TPanel; prTop : Integer): TLabel;
begin
  Result           := TLabel.Create(prContainer);
  Result.Parent    := prContainer;
  Result.Top       := prTop;
  Result.Left      := FORM_MARGIN_LEFT;
  Result.Caption   := Self.Action.Hint;
  Result.Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
end;

function TJupiterFormAction.Internal_CreateContainer(prOwner: TScrollBox): TPanel;
begin
  Result            := TPanel.Create(prOwner);
  Result.Parent     := prOwner;
  Result.Align      := alTop;
  Result.Height     := 50;
  Result.Caption    := EmptyStr;

  if not Self.Raised then
    Result.BevelOuter := bvNone
  else
    Result.BevelOuter := bvRaised;

  Result.TabOrder   := GENERATOR_SYSLAYER + Self.TabOrder;
end;

procedure TJupiterFormAction.Internal_OnClick(Sender: TObject);
begin
  Self.Action.Execute;
end;

procedure TJupiterFormAction.Draw(prOwner: TScrollBox);
var
  vrContainer : TPanel;
  vrLabel     : TLabel;
  vrSubLabel  : TLabel;
begin
  try
    vrContainer := Self.Internal_CreateContainer(prOwner);
    vrLabel     := Self.Internal_CreatetTitle(vrContainer);
    vrSubLabel  := Self.Internal_CreatetSubTitle(vrContainer, vrLabel.Top + vrLabel.Height + FORM_MARGIN_TOP);

    vrContainer.Height := (vrSubLabel.Top + vrSubLabel.Height) + FORM_MARGIN_BOTTOM_TONEXT
  finally
    Self.Panel := vrContainer;
  end;
end;

end.

