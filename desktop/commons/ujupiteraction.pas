unit uJupiterAction;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, ExtCtrls;

type

  { TJupiterAction }

  TJupiterAction = class(TJupiterObject)
  private
    FIcon    : Integer;
    FCaption : String;
    FHint    : String;
    FOnClick : TNotifyEvent;
  published
    property Caption : String       read FCaption write FCaption;
    property Icon    : Integer      read FIcon    write FIcon;
    property Hint    : String       read FHint    write FHint;
    property OnClick : TNotifyEvent read FOnClick write FOnClick;
  public
    constructor Create(prCaption, prHint : String; prIcon : Integer);
    constructor Create(prCaption, prHint : String; prIcon : Integer; prOnClick : TNotifyEvent);

    procedure Render(prFlow : TFlowPanel);
  end;

  { TJupiterActionGroup }

  TJupiterActionGroup = class(TJupiterObjectList)
  private
    FFlowPanel : TFlowPanel;
  published
    property FlowPanel : TFlowPanel read FFlowPanel write FFlowPanel;
  public
    procedure AddAction(prAction : TJupiterAction);

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

procedure TJupiterAction.Render(prFlow: TFlowPanel);
begin

end;

{ TJupiterActionGroup }

procedure TJupiterActionGroup.AddAction(prAction: TJupiterAction);
begin
  Self.Add(prAction);
end;

procedure TJupiterActionGroup.Render;
begin

end;

end.

