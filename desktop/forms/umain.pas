unit uMain;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  uJupiterForm, JupiterFormTab;

type

  { TFMain }

  TFMain = class(TFJupiterForm)
    jtMainTab: TJupiterFormTab;
    mmMainMenu: TMainMenu;
    sbStatus: TStatusBar;
  private

  public

  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}

end.

