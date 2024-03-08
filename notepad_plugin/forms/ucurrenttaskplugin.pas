unit ucurrentTaskPlugin;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, NppDockingForms,
  NppPlugin, Messages, LMessages, LCLIntf, LCLType;

type

  { TFCurrentTaskPlugin }

  TFCurrentTaskPlugin = class(TNppDockingForm)
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormDock(Sender: TObject);
    procedure FormFloat(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FCurrentTaskPlugin: TFCurrentTaskPlugin;

implementation

{$R *.lfm}

{ TFCurrentTaskPlugin }

procedure TFCurrentTaskPlugin.FormCreate(Sender: TObject);
begin
  Application.MessageBox('37', '', MB_ICONINFORMATION);

  Self.NppDefaultDockingMask := DWS_DF_FLOATING; // whats the default docking position
  Self.KeyPreview            := True; // special hack for input forms
  Self.OnFloat               := Self.FormFloat;
  Self.OnDock                := Self.FormDock;

  Application.MessageBox('45', '', MB_ICONINFORMATION);
end;

procedure TFCurrentTaskPlugin.FormHide(Sender: TObject);
begin
  Application.MessageBox('50', '', MB_ICONINFORMATION);

  SendMessage(Self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, Self.CmdID, 0);

  Application.MessageBox('54', '', MB_ICONINFORMATION);
end;

procedure TFCurrentTaskPlugin.FormDock(Sender: TObject);
begin
  Application.MessageBox('59', '', MB_ICONINFORMATION);

  SendMessage(Self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, Self.CmdID, 1);

  Application.MessageBox('63', '', MB_ICONINFORMATION);
end;

procedure TFCurrentTaskPlugin.FormFloat(Sender: TObject);
begin
  Application.MessageBox('68', '', MB_ICONINFORMATION);

  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 1);

  Application.MessageBox('72', '', MB_ICONINFORMATION);
end;

procedure TFCurrentTaskPlugin.FormShow(Sender: TObject);
begin
  Application.MessageBox('77', '', MB_ICONINFORMATION);

  inherited;

  Application.MessageBox('81', '', MB_ICONINFORMATION);

  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 1);

  Application.MessageBox('85', '', MB_ICONINFORMATION);
end;

end.

