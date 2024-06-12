unit jupiterPanelNotifier;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ExtCtrls, SysUtils;

type

  { TJupiterPanelNotifier }

  TJupiterPanelNotifierType = (pntInfo, pntError, pntSucess);

  TJupiterPanelNotifier = class (TObject)
  private
    FPanel : TPanel;

    procedure Internal_OnCloseClick(Sender: TObject);
  public
    constructor Create(prTitle : String; prType : TJupiterPanelNotifierType);
  end;

implementation

uses Controls, Forms, Graphics;

{ TJupiterPanelNotifier }

procedure TJupiterPanelNotifier.Internal_OnCloseClick(Sender: TObject);
begin
  if Sender is TPanel then
    TPanel(Sender).Parent.Visible := False;
end;

constructor TJupiterPanelNotifier.Create(prTitle : String; prType : TJupiterPanelNotifierType);
var
  vrPanelButton : TPanel;
begin
  Self.FPanel := TPanel.Create(Application.MainForm);
  try
    Self.FPanel.Parent := Application.MainForm;
    Self.FPanel.Align := alTop;
    Self.FPanel.Top := 0;
    Self.FPanel.Height := 40;
    Self.FPanel.Caption := '   ' + prTitle;
    Self.FPanel.BevelOuter := bvNone;
    Self.FPanel.Alignment := taLeftJustify;
    Self.FPanel.BorderStyle := bsNone;
    Self.FPanel.Font.Color := clWhite;

    case prType of
      pntInfo   : Self.FPanel.Color := clSkyBlue;
      pntError  : Self.FPanel.Color := $00B3B3FF;
      pntSucess : Self.FPanel.Color := clMoneyGreen;
    end;
  finally
    vrPanelButton := TPanel.Create(Self.FPanel);
    vrPanelButton.Parent := Self.FPanel;
    vrPanelButton.BevelOuter := bvNone;
    vrPanelButton.Width := 40;
    vrPanelButton.Align := alRight;
    vrPanelButton.Caption := 'X';
    vrPanelButton.OnClick := @Internal_OnCloseClick;
    vrPanelButton.Font.Color := clWhite;
  end;
end;

end.

