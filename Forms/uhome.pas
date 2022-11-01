unit uHome;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, JupiterForm, JupiterApp, JupiterConsts, JupiterRoute, uMain;

type

  { TFHome }

  TFHome = class(TFJupiterForm)
    Image1: TImage;
    lbVersion: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lbConfigLink: TLabel;
    lbConfigLink1: TLabel;
    lbNewTaskLink: TLabel;
    lbNewTaskLink1: TLabel;
    lbTitle: TLabel;
    pnTitle: TPanel;
    sbBody: TScrollBox;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure Label8DblClick(Sender: TObject);
    procedure lbConfigLink1Click(Sender: TObject);
    procedure lbConfigLinkClick(Sender: TObject);
    procedure lbNewTaskLink1Click(Sender: TObject);
    procedure lbNewTaskLinkClick(Sender: TObject);
    procedure pnTitleClick(Sender: TObject);
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

procedure TFHome.Label5Click(Sender: TObject);
begin
  FMain.miPastasJupiter.Click;
end;

procedure TFHome.Label6Click(Sender: TObject);
begin
  FMain.miPastasDatasets.Click;
end;

procedure TFHome.Label7Click(Sender: TObject);
begin
  FMain.miPastasModules.Click;
end;

procedure TFHome.Label8DblClick(Sender: TObject);
begin
  FMain.miPastasTemp.Click;
end;

procedure TFHome.lbConfigLink1Click(Sender: TObject);
begin
  FMain.tbMessage.Click;
end;

procedure TFHome.lbConfigLinkClick(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(CONFIG_PATH), True);
end;

procedure TFHome.lbNewTaskLink1Click(Sender: TObject);
begin
  if FMain.miOpenCurrentTask.Enabled then
    vrJupiterApp.NavigateTo(TJupiterRoute.Create(TASK_FORM_PATH), True)
  else
    ShowMessage('A tarefa atual não foi definida');
end;

procedure TFHome.lbNewTaskLinkClick(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(NEWTASK_FORM_PATH), True);
end;

procedure TFHome.pnTitleClick(Sender: TObject);
begin

end;

procedure TFHome.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  lbTitle.Caption   := vrJupiterApp.AppName;
  lbVersion.Caption := 'Versão: ' + vrJupiterApp.GetVersion;
end;

end.

