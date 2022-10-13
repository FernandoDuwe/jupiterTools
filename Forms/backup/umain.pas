unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Menus, PopupNotifier, Buttons, JupiterApp, JupiterRoute,
  JupiterConsts, JupiterObject, JupiterForm, JupiterAction,
  uPSComponent_Default, LMessages, PairSplitter;

type

  { TFMain }

  TFMain = class(TFJupiterForm)
    cbNavigationMenu: TCoolBar;
    edSearch: TEdit;
    ilIconFamily: TImageList;
    miFormParams: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    mmMainMenu: TMainMenu;
    pnBody: TPanel;
    pnMenu: TPanel;
    spSplitter: TSplitter;
    tbHome: TToolButton;
    tbMenu: TToolButton;
    tbOptions: TToolBar;
    TbSystemBar: TToolButton;
    ToolBar1: TToolBar;
    tbSearch: TToolBar;
    tbSystemButtons: TToolBar;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    tbMessage: TToolButton;
    tvMenu: TTreeView;
    procedure cbNavigationMenuChange(Sender: TObject);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure pnBodyClick(Sender: TObject);
    procedure tbHomeClick(Sender: TObject);
    procedure tbMenuClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure tvMenuClick(Sender: TObject);
  private
    procedure Internal_ListMenuItens;

    procedure Internal_ShowRoute(prRoute : TJupiterRoute; prList : TJupiterObjectList; prNode : TTreeNode);
  protected
    procedure Internal_UpdateComponents; override;
    procedure Internal_PrepareForm; override;
  public

  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}

{ TFMain }

procedure TFMain.tbHomeClick(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(ROOT_FORM_PATH), False);
end;

procedure TFMain.tbMenuClick(Sender: TObject);
begin
  try
    pnMenu.Visible := not pnMenu.Visible;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFMain.ToolButton1Click(Sender: TObject);
begin
  Self.UpdateForm;
end;

procedure TFMain.ToolButton2Click(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(CONFIG_PATH), True);
end;

procedure TFMain.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
begin

end;

procedure TFMain.cbNavigationMenuChange(Sender: TObject);
begin

end;

procedure TFMain.FormShow(Sender: TObject);
begin
  inherited;

  vrJupiterApp.MainIcons := ilIconFamily;
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(ROOT_FORM_PATH), False);
end;

procedure TFMain.MenuItem5Click(Sender: TObject);
begin

end;

procedure TFMain.pnBodyClick(Sender: TObject);
begin

end;

procedure TFMain.tvMenuClick(Sender: TObject);
var
  vrAction : TJupiterAction;
begin
  if not Assigned(tvMenu.Selected) then
    Exit;

  if not Assigned(tvMenu.Selected.Data) then
    Exit;

  vrAction := TJupiterAction(tvMenu.Selected.Data);

  if Assigned(vrAction.Route) then
    vrJupiterApp.NavigateTo(vrAction.Route, False);
end;

procedure TFMain.Internal_ListMenuItens;
var
  vrMenuList : TJupiterObjectList;
begin
  vrMenuList := vrJupiterApp.GetActions(TJupiterRoute.Create(ROOT_PATH));

  Self.Internal_ShowRoute(TJupiterRoute.Create(ROOT_PATH), vrMenuList, nil);

  tvMenu.FullExpand;
end;

procedure TFMain.Internal_ShowRoute(prRoute: TJupiterRoute; prList: TJupiterObjectList; prNode : TTreeNode);
var
  vrVez    : Integer;
  vrNode   : TTreeNode;
  vrAction : TJupiterAction;
begin
  for vrVez := 0 to prList.Size - 1 do
  begin
    vrAction := TJupiterAction(prList.GetAtIndex(vrVez));

    if not Assigned(vrAction.Location) then
      Continue;

    if vrAction.Location.Path <> prRoute.Path then
      Continue;

    if Assigned(prNode) then
      vrNode := tvMenu.Items.AddChild(prNode, vrAction.Title)
    else
      vrNode := tvMenu.Items.Add(nil, vrAction.Title);

    vrNode.ImageIndex := vrAction.Icon;
    vrNode.SelectedIndex := vrAction.Icon;
    vrNode.Data := vrAction;

    if Assigned(vrAction.Route) then
      Self.Internal_ShowRoute(vrAction.Route, prList, vrNode);
  end;
end;

procedure TFMain.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  Self.Caption := vrJupiterApp.AppName;

  if pnMenu.Visible then
  begin
    tbMenu.ImageIndex := ICON_LEFT;

    tbMenu.Hint := 'Esconder menu';
  end
  else
  begin
    tbMenu.ImageIndex := ICON_RIGHT;

    tbMenu.Hint := 'Exibir menu';
  end;
end;

procedure TFMain.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  vrJupiterApp.BodyPanel := pnBody;

  Self.Internal_ListMenuItens;
end;

end.

