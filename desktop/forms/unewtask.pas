unit uNewTask;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  uJupiterForm, jupiterformutils, jupitertreeviewmenugenerator, JupiterApp,
  JupiterConsts, jupiterDesktopApp, jupiterformcomponenttils;

type

  { TFNewTask }

  TFNewTask = class(TFJupiterForm)
    imLogo: TImage;
    sbShortcut: TScrollBox;
    Splitter1: TSplitter;
    tvTreeMenu: TTreeView;
  private
    procedure Internal_UpdateComponents; override;

    procedure Internal_PrepareForm; override;

    procedure Internal_DrawForm;
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

  tvTreeMenu.Width := PercentOfScreen(Self.Width, 50);
end;

procedure TFNewTask.Internal_PrepareForm;
var
  vrTreeView : TJupiterTreeViewMenuGenerator;
begin
  inherited Internal_PrepareForm;

  tvTreeMenu.Images := TJupiterDesktopApp(vrJupiterApp).ImageList;

  Self.Internal_DrawForm;

  vrTreeView := TJupiterTreeViewMenuGenerator.Create(vrJupiterApp.InternalDatabase);
  try
    vrTreeView.TreeView := tvTreeMenu;
    vrTreeView.Render;
  finally
    FreeAndNil(vrTreeView);
  end;
end;

procedure TFNewTask.Internal_DrawForm;
var
  vrCurrentLine : Integer;
  vrReference : TJupiterComponentReference;
begin
  vrCurrentLine := imLogo.Top;

  vrReference := jupiterformcomponenttils.JupiterComponentsNewLabel('Jupiter', TJupiterPosition.Create(vrCurrentLine, imLogo.Width + imLogo.Left + FORM_MARGIN_LEFT), sbShortcut);

  vrCurrentLine := vrReference.Bottom + FORM_MARGIN_TOP;

  vrReference := jupiterformcomponenttils.JupiterComponentsNewLabel('Versão: ' + vrJupiterApp.GetVersion, TJupiterPosition.Create(vrCurrentLine, imLogo.Width + imLogo.Left + FORM_MARGIN_LEFT), sbShortcut);
end;

end.

