unit uContextMenu;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ActnList,
  StdCtrls, ExtCtrls, EditBtn, Calendar, uJupiterForm, jupiterformutils,
  JupiterApp, JupiterConsts, jupitertreeviewmenugenerator, uJupiterAction,
  jupiterDesktopApp, uJupiterDesktopAppScript;

type

  { TFContextMenu }

  TFContextMenu = class(TFJupiterForm)
    acExit: TAction;
    acSearch: TAction;
    gbMain: TGroupBox;
    gbContext: TGroupBox;
    lvRoute: TListView;
    Splitter1: TSplitter;
    tvTreeMenu: TTreeView;
    procedure acExitExecute(Sender: TObject);
    procedure acSearchExecute(Sender: TObject);
    procedure edSearchKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure lvRouteKeyPress(Sender: TObject; var Key: char);
    procedure Splitter1Moved(Sender: TObject);
    procedure tvTreeMenuEnter(Sender: TObject);
  private
    FSearchOnMenu : Boolean;

    procedure Internal_OnSearchOnMenuClick(Sender: TObject);
    procedure Internal_OnSearchOnDatabaseClick(Sender: TObject);

    procedure Internal_PrepareForm; override;

    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
  end;

var
  FContextMenu: TFContextMenu;

implementation

{$R *.lfm}

{ TFContextMenu }

procedure TFContextMenu.acExitExecute(Sender: TObject);
begin
  Self.DoSecureClose;
end;

procedure TFContextMenu.acSearchExecute(Sender: TObject);
var
  vrTreeView : TJupiterTreeViewMenuGenerator;
begin
  if not Self.FSearchOnMenu then
  begin
    try
      JupiterAppDesktopOpenDatbaseFinder(edSearch.Text);
    finally
      Self.DoSecureClose;
    end;
  end;

  if edSearch.Focused then
  begin
    Self.UpdateForm();

    Exit;
  end;

  if not tvTreeMenu.Focused then
  begin
    if not Assigned(lvRoute.Selected) then
      Exit;

    if not Assigned(lvRoute.Selected.Data) then
      Exit;

    try
      TJupiterAction(lvRoute.Selected.Data).Execute;
    finally
      Self.DoSecureClose;
    end;
  end;

  if tvTreeMenu.Focused then
  begin
    vrTreeView := TJupiterTreeViewMenuGenerator.Create(vrJupiterApp.InternalDatabase);
    try
      vrTreeView.TreeView := tvTreeMenu;
      vrTreeView.DoClick(tvTreeMenu);
    finally
      FreeAndNil(vrTreeView);
    end;

    Self.DoSecureClose;
  end;
end;

procedure TFContextMenu.edSearchKeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TFContextMenu.FormResize(Sender: TObject);
begin
  inherited;

  if Self.Showing then
    Self.UpdateForm(False);
end;

procedure TFContextMenu.lvRouteKeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TFContextMenu.Splitter1Moved(Sender: TObject);
begin
  miLookColumn.Checked := False;
end;

procedure TFContextMenu.tvTreeMenuEnter(Sender: TObject);
begin
  Self.UpdateForm(False);
end;

procedure TFContextMenu.Internal_OnSearchOnMenuClick(Sender: TObject);
begin
  Self.FSearchOnMenu := True;

  Self.UpdateForm();
end;

procedure TFContextMenu.Internal_OnSearchOnDatabaseClick(Sender: TObject);
begin
  Self.FSearchOnMenu := False;

  Self.UpdateForm();
end;

procedure TFContextMenu.Internal_PrepareForm;
var
  vrTreeView : TJupiterTreeViewMenuGenerator;
begin
  inherited Internal_PrepareForm;

  Self.FSearchOnMenu := True;

  tvTreeMenu.OnEnter := @tvTreeMenuEnter;

  edSearch.OnEnter := @tvTreeMenuEnter;
  edSearch.OnExit := @tvTreeMenuEnter;

  lvRoute.OnEnter := @tvTreeMenuEnter;
  lvRoute.OnExit := @tvTreeMenuEnter;

  tvTreeMenu.Images := TJupiterDesktopApp(vrJupiterApp).ImageList;

  Self.ShowSearchBar := True;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Menu', 'Pesquisar no menu de contexto', ICON_SEARCH, @Internal_OnSearchOnMenuClick));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Registros', 'Pesquisar nos registros do sistema', ICON_SEARCH, @Internal_OnSearchOnDatabaseClick));

  edSearch.SetFocus;

  lvRoute.LargeImages := TJupiterDesktopApp(vrJupiterApp).ImageList;
  lvRoute.SmallImages := TJupiterDesktopApp(vrJupiterApp).ImageList;
  lvRoute.StateImages := TJupiterDesktopApp(vrJupiterApp).ImageList;

  vrTreeView := TJupiterTreeViewMenuGenerator.Create(vrJupiterApp.InternalDatabase);
  try
    vrTreeView.TreeView := tvTreeMenu;
    vrTreeView.Render;
  finally
    FreeAndNil(vrTreeView);
  end;
end;

procedure TFContextMenu.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if Self.ActionGroup.Count > 0 then
  begin
    if Self.FSearchOnMenu then
    begin
      Self.ActionGroup.GetActionAtIndex(0).Disable;
      Self.ActionGroup.GetActionAtIndex(1).Enable;

      edSearch.TextHint := 'Pesquisar no menu de contexto';
    end;

    if not Self.FSearchOnMenu then
    begin
      Self.ActionGroup.GetActionAtIndex(1).Disable;
      Self.ActionGroup.GetActionAtIndex(0).Enable;

      edSearch.TextHint := 'Pesquisar em todo o sistema';
    end;
  end;

  gbMain.Width := PercentOfScreen(Self.Width, 50);

  gbMain.Font.Color := clDefault;
  gbContext.Font.Color := clDefault;

  lvRoute.Column[0].Width := lvRoute.Width;

  if tvTreeMenu.Focused then
    gbMain.Font.Color := clRed;

  if lvRoute.Focused then
    gbContext.Font.Color := clRed;
end;

procedure TFContextMenu.Internal_UpdateDatasets;
var
  vrActionList : TJupiterActionGroup;
  vrVez : Integer;
  vrListItem : TListItem;
begin
  inherited Internal_UpdateDatasets;

  vrActionList := TJupiterDesktopApp(vrJupiterApp).GenerateContextMenu(edSearch.Text);

  lvRoute.Items.Clear;

  for vrVez := 0 to vrActionList.Count - 1 do
  begin
    vrListItem := lvRoute.Items.Add;
    vrListItem.Caption := vrActionList.GetActionAtIndex(vrVez).Caption + '   ';
    vrListItem.ImageIndex := vrActionList.GetActionAtIndex(vrVez).Icon;
    vrListItem.StateIndex := NULL_KEY;
    vrListItem.Data := vrActionList.GetActionAtIndex(vrVez);
  end;

  if lvRoute.Items.Count > 0 then
    lvRoute.Selected := lvRoute.Items[0];
end;

end.

