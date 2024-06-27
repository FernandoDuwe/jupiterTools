unit uMain;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ActnList, ExtCtrls, Buttons, uJupiterForm, JupiterFormTab,
  jupiterMainMenuGenerator, JupiterApp;

type

  { TFMain }

  TFMain = class(TFJupiterForm)
    acNewTab: TAction;
    ilIconFamily: TImageList;
    ilTabs: TImageList;
    jtMainTab: TJupiterFormTab;
    miNewTab: TMenuItem;
    mmMainMenu: TMainMenu;
    sbStatus: TStatusBar;
    procedure miNewTabClick(Sender: TObject);
  private
    procedure Internal_PrepareForm; override;
  public

  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}

{ TFMain }

procedure TFMain.miNewTabClick(Sender: TObject);
begin
  jtMainTab.AddForm(TFJupiterForm.Create(Self));
end;

procedure TFMain.Internal_PrepareForm;
var
  vrMainMenu : TJupiterMainMenuGenerator;
begin
  inherited Internal_PrepareForm;

  vrMainMenu := TJupiterMainMenuGenerator.Create(vrJupiterApp.InternalDatabase);
  try
    vrMainMenu.MainMenu := mmMainMenu;
    vrMainMenu.Render;
  finally
    FreeAndNil(vrMainMenu);
  end;
end;

end.

