unit uNewTask;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  uJupiterForm, jupiterformutils, jupitertreeviewmenugenerator, JupiterApp,
  jupiterDesktopApp;

type

  { TFNewTask }

  TFNewTask = class(TFJupiterForm)
    tvTreeMenu: TTreeView;
  private
    procedure Internal_UpdateComponents; override;

    procedure Internal_PrepareForm; override;
  public

  end;

var
  FNewTask: TFNewTask;

implementation

{$R *.lfm}

{ TFNewTask }

procedure TFNewTask.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;
end;

procedure TFNewTask.Internal_PrepareForm;
var
  vrTreeView : TJupiterTreeViewMenuGenerator;
begin
  tvTreeMenu.Width := PercentOfScreen(Self.Width, 30);

  inherited Internal_PrepareForm;

  tvTreeMenu.Images := TJupiterDesktopApp(vrJupiterApp).ImageList;

  vrTreeView := TJupiterTreeViewMenuGenerator.Create(vrJupiterApp.InternalDatabase);
  try
    vrTreeView.TreeView := tvTreeMenu;
    vrTreeView.Render;
  finally
    FreeAndNil(vrTreeView);
  end;
end;

end.

