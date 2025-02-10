unit uNewTask;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  uJupiterForm, jupiterformutils, jupitertreeviewmenugenerator, JupiterApp,
  JupiterConsts, JupiterObject, jupiterDesktopApp, jupiterformcomponenttils,
  uJupiterAction, StdCtrls;

type

  { TFNewTask }

  TFNewTask = class(TFJupiterForm)
    imLogo: TImage;
    sbShortcut: TScrollBox;
    Splitter1: TSplitter;
    tvTreeMenu: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FReferences : TJupiterObjectList;
    FCurrentLine : Integer;
    FActionList : TJupiterActionGroup;

    procedure Internal_LinkClick(Sender: TObject);

    procedure Internal_UpdateComponents; override;

    procedure Internal_PrepareForm; override;

    procedure Internal_ListContextMenu;

    procedure Internal_DrawForm;
  public

  end;

var
  FNewTask: TFNewTask;

implementation

{$R *.lfm}

{ TFNewTask }

procedure TFNewTask.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FReferences := TJupiterObjectList.Create;
end;

procedure TFNewTask.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Self.FReferences);

  inherited;
end;

procedure TFNewTask.Internal_LinkClick(Sender: TObject);
begin
  if not (Sender is TLabel) then
    Exit;

  Self.FActionList.GetActionAtIndex(TLabel(Sender).Tag).Execute;
end;

procedure TFNewTask.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if miLookColumn.Checked then
    tvTreeMenu.Width := PercentOfScreen(Self.Width, Self.PercentDivisor);
end;

procedure TFNewTask.Internal_PrepareForm;
var
  vrTreeView : TJupiterTreeViewMenuGenerator;
begin
  inherited Internal_PrepareForm;

  tvTreeMenu.Images := TJupiterDesktopApp(vrJupiterApp).ImageList;

  Self.Internal_ListContextMenu;

  Self.Internal_DrawForm;

  vrTreeView := TJupiterTreeViewMenuGenerator.Create(vrJupiterApp.InternalDatabase);
  try
    vrTreeView.TreeView := tvTreeMenu;
    vrTreeView.Render;
  finally
    FreeAndNil(vrTreeView);
  end;
end;

procedure TFNewTask.Internal_ListContextMenu;
var
  vrVez : Integer;
  vrAction : TJupiterAction;
  vrReference : TJupiterComponentReference;
begin
  Self.FCurrentLine := (imLogo.Height + imLogo.Top) + FORM_MARGIN_BOTTOM_TONEXT;

  Self.FActionList := TJupiterDesktopApp(vrJupiterApp).GenerateContextMenu;

  for vrVez := 0 to Self.FActionList.Count - 1 do
  begin
    vrAction := Self.FActionList.GetAtIndex(vrVez) as TJupiterAction;

    vrReference := JupiterComponentsNewLink(vrAction.Caption, TJupiterPosition.Create(Self.FCurrentLine, FORM_MARGIN_LEFT + FORM_MARGIN_LEFT), sbShortcut);

    TLabel(vrReference.Component).Tag := vrVez;
    TLabel(vrReference.Component).OnClick := @Internal_LinkClick;

    Self.FCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;

    vrReference := JupiterComponentsNewLabel(vrAction.Hint, TJupiterPosition.Create(Self.FCurrentLine, FORM_MARGIN_LEFT + FORM_MARGIN_LEFT + FORM_MARGIN_LEFT), sbShortcut);

    Self.FCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM_TONEXT;
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

