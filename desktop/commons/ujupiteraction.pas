unit uJupiterAction;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, jupiterformutils, JupiterConsts, JupiterApp,
  ExtCtrls, Controls, Buttons;

type

  { TJupiterAction }

  TJupiterAction = class(TJupiterObject)
  private
    FIcon    : Integer;
    FCaption : String;
    FHint    : String;
    FOnClick : TNotifyEvent;
    FButton  : TSpeedButton;
  published
    property Caption : String       read FCaption write FCaption;
    property Icon    : Integer      read FIcon    write FIcon;
    property Hint    : String       read FHint    write FHint;
    property OnClick : TNotifyEvent read FOnClick write FOnClick;
  public
    constructor Create(prCaption, prHint : String; prIcon : Integer);
    constructor Create(prCaption, prHint : String; prIcon : Integer; prOnClick : TNotifyEvent);

    procedure Render(prFlow : TFlowPanel; prImageList : TImageList);

    procedure Disable;
    procedure Enable;
  end;

  { TJupiterActionGroup }

  TJupiterActionGroup = class(TJupiterObjectList)
  private
    FFlowPanel : TFlowPanel;
    FImageList : TImageList;
  published
    property FlowPanel : TFlowPanel read FFlowPanel write FFlowPanel;
    property ImageList : TImageList read FImageList write FImageList;
  public
    procedure AddAction(prAction : TJupiterAction);

    function GetActionAtIndex(prIndex : Integer) : TJupiterAction;

    procedure Render;
  end;

implementation

{ TJupiterAction }

constructor TJupiterAction.Create(prCaption, prHint: String; prIcon : Integer);
begin
  Self.Caption := prCaption;
  Self.Hint    := prHint;
  Self.Icon    := prIcon;
end;

constructor TJupiterAction.Create(prCaption, prHint: String; prIcon: Integer; prOnClick: TNotifyEvent);
begin
  Self.Caption := prCaption;
  Self.Hint    := prHint;
  Self.Icon    := prIcon;
  Self.OnClick := prOnClick;
end;

procedure TJupiterAction.Render(prFlow: TFlowPanel; prImageList : TImageList);
var
  vrSpeedButton :  TSpeedButton;
begin
  vrSpeedButton            := TSpeedButton.Create(prFlow);
  vrSpeedButton.Parent     := prFlow;
  vrSpeedButton.Caption    := Self.Caption;
  vrSpeedButton.Hint       := Self.Hint;
  vrSpeedButton.ShowHint   := Self.Hint <> EmptyStr;
  vrSpeedButton.Flat       := True;
  vrSpeedButton.Height     := GetTextHeight(vrSpeedButton.Caption, vrSpeedButton.Font) + FORM_MARGIN_TOP + FORM_MARGIN_BOTTOM;
  vrSpeedButton.Width      := GetTextWidth(vrSpeedButton.Caption, vrSpeedButton.Font) + FORM_MARGIN_LEFT + FORM_MARGIN_RIGHT;
  vrSpeedButton.OnClick    := OnClick;

  if Assigned(prImageList) then
  begin
    vrSpeedButton.ImageIndex := Self.Icon;
    vrSpeedButton.Images     := prImageList;
    vrSpeedButton.Width      := vrSpeedButton.Width + 16;
  end;

  Self.FButton := vrSpeedButton;
end;

procedure TJupiterAction.Disable;
begin
  if Assigned(Self.FButton) then
    Self.FButton.Enabled := False;
end;

procedure TJupiterAction.Enable;
begin
  if Assigned(Self.FButton) then
    Self.FButton.Enabled := True;
end;

{ TJupiterActionGroup }

procedure TJupiterActionGroup.AddAction(prAction: TJupiterAction);
begin
  Self.Add(prAction);
end;

function TJupiterActionGroup.GetActionAtIndex(prIndex: Integer): TJupiterAction;
begin
  Result := Self.GetAtIndex(prIndex) as TJupiterAction;
end;

procedure TJupiterActionGroup.Render;
var
  vrVez : Integer;
begin
  for vrVez := 0 to Self.Count - 1 do
    TJupiterAction(Self.GetAtIndex(vrVez)).Render(Self.FlowPanel, Self.ImageList);
end;

end.

