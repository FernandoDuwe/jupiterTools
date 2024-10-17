unit uMain;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ActnList, ExtCtrls, Buttons, uJupiterForm, JupiterFormTab,
  jupiterMainMenuGenerator, JupiterApp, JupiterConsts, jupiterDesktopApp;

type

  { TFMain }

  TFMain = class(TFJupiterForm)
    ilIconFamily: TImageList;
    ilTabs: TImageList;
    jtMainTab: TJupiterFormTab;
    mmMainMenu: TMainMenu;
    sbStatus: TStatusBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure jtMainTabCloseTabClicked(Sender: TObject);
    procedure jtMainTabResize(Sender: TObject);
  private
    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateComponents; override;
  public
    procedure NewTab(Form : TForm);
  end;

var
  FMain: TFMain;

implementation

uses JupiterFormTabSheet;

{$R *.lfm}

{ TFMain }

procedure TFMain.jtMainTabCloseTabClicked(Sender: TObject);
begin
  jtMainTab.CloseTab(jtMainTab.ActivePageIndex);
end;

procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  while jtMainTab.PageCount > 0 do
    jtMainTab.CloseTab(0);
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  inherited;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  while jtMainTab.PageCount > 0 do
    jtMainTab.CloseTab(0);

  inherited;
end;

procedure TFMain.FormShow(Sender: TObject);
begin
  inherited;

  vrJupiterApp.RunMacro(TRIGGER_ONSTART);
end;

procedure TFMain.jtMainTabResize(Sender: TObject);
var
  vrVez : Integer;
  vrForm : TForm;
begin
  for vrVez := 0 to jtMainTab.PageCount - 1 do
  begin
    if not (jtMainTab.Pages[vrVez] is TJupiterFormTabSheet) then
      Continue;

    if not Assigned(TJupiterFormTabSheet(jtMainTab.Pages[vrVez]).Form) then
      Continue;

    vrForm := TJupiterFormTabSheet(jtMainTab.Pages[vrVez]).Form;

    if not (vrForm is TFJupiterForm) then
      Continue;

    TFJupiterForm(vrForm).UpdateForm(False, True, False);
  end;
end;

procedure TFMain.Internal_PrepareForm;
var
  vrMainMenu : TJupiterMainMenuGenerator;
begin
  inherited Internal_PrepareForm;

  TJupiterDesktopApp(vrJupiterApp).ImageList := ilIconFamily;

  vrMainMenu := TJupiterMainMenuGenerator.Create(vrJupiterApp.InternalDatabase);
  try
    vrMainMenu.MainMenu := mmMainMenu;
    vrMainMenu.Render;
  finally
    FreeAndNil(vrMainMenu);
  end;
end;

procedure TFMain.Internal_UpdateComponents;
var
  vrVez : Integer;
begin
  inherited Internal_UpdateComponents;

  Self.Caption := vrJupiterApp.AppName;

  for vrVez := 0 to jtMainTab.PageCount - 1 do
  begin
    if not (jtMainTab.Page[vrVez] is TJupiterFormTabSheet) then
      Continue;

    jtMainTab.Page[vrVez].Caption := (jtMainTab.Page[vrVez] as TJupiterFormTabSheet).Form.Caption + '        ';
  end;

  Self.Repaint;
  Self.Refresh;

  Application.ProcessMessages;
end;

procedure TFMain.NewTab(Form: TForm);
var
  vrSS : TShiftState;
begin
  vrSS := GetKeyShiftState;

  if ssCtrl in vrSS then
  begin
    Form.ShowModal;

    Exit;
  end;

  jtMainTab.Visible := True;
  jtMainTab.AddForm(Form);

  if Form is TFJupiterForm then
    TFJupiterForm(Form).OwnerTab := jtMainTab.Pages[jtMainTab.PageCount - 1] as TJupiterFormTabSheet;
end;

end.

