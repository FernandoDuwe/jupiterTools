unit uPluginConfig;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, NppForms;

type

  { TFPluginConfig }

  TFPluginConfig = class(TNppForm)
    Button2: TButton;
    edPath: TEdit;
    Image1: TImage;
    imgInfo: TImage;
    Label1: TLabel;
    lbHelp: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    pnHint: TPanel;
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    OkClick : Boolean;
  end;

var
  FPluginConfig: TFPluginConfig;

implementation

uses jupiterformutils;

{$R *.lfm}

{ TFPluginConfig }

procedure TFPluginConfig.Button2Click(Sender: TObject);
begin
  Self.OkClick  := True;
  Self.Close;
end;

procedure TFPluginConfig.FormShow(Sender: TObject);
begin
  Self.Height := PercentOfScreen(Screen.Height, 50);
  Self.Width  := PercentOfScreen(Screen.Width, 50);

  Self.Position := poScreenCenter;
  Self.OkClick  := False;
end;

end.

