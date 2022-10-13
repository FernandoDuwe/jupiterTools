unit uHome;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, JupiterForm, JupiterApp, JupiterConsts, JupiterRoute;

type

  { TFHome }

  TFHome = class(TFJupiterForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbConfigLink: TLabel;
    lbNewTaskLink: TLabel;
    lbTitle: TLabel;
    pnTitle: TPanel;
    sbBody: TScrollBox;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbConfigLinkClick(Sender: TObject);
    procedure lbNewTaskLinkClick(Sender: TObject);
  private

  protected
    procedure Internal_UpdateComponents; override;

  public

  end;

var
  FHome: TFHome;

implementation

{$R *.lfm}

{ TFHome }

procedure TFHome.FormResize(Sender: TObject);
begin
  pnTitle.Top  := Round((sbBody.Height / 2) - (pnTitle.Height / 2));
  pnTitle.Left := Round((sbBody.Width / 2) - (pnTitle.Width / 2));
end;

procedure TFHome.FormShow(Sender: TObject);
begin
  inherited;

  FormResize(Sender);
end;

procedure TFHome.lbConfigLinkClick(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(CONFIG_PATH), True);
end;

procedure TFHome.lbNewTaskLinkClick(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(NEWTASK_FORM_PATH), True);
end;

procedure TFHome.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  lbTitle.Caption := vrJupiterApp.AppName;
end;

end.

