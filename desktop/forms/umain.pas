unit uMain;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ActnList, ExtCtrls, Buttons, uJupiterForm, JupiterFormTab,
  jupiterMainMenuGenerator, JupiterApp, jupiterDesktopApp;

type

  { TFMain }

  TFMain = class(TFJupiterForm)
    ilIconFamily: TImageList;
    ilTabs: TImageList;
    jtMainTab: TJupiterFormTab;
    mmMainMenu: TMainMenu;
    sbStatus: TStatusBar;
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
begin
  inherited Internal_UpdateComponents;

  Self.Caption := vrJupiterApp.AppName;
end;

procedure TFMain.NewTab(Form: TForm);
begin
  jtMainTab.Visible := True;
  jtMainTab.AddForm(Form);
end;

end.

