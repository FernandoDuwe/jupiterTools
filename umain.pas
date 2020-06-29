unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ExtCtrls, StdCtrls, Buttons, JupiterForm, uNewAction, JupiterParams,
  jupiterconsts;

type

  { TFMain }

  TFMain = class(TJupiterForm)
    ilIcons: TImageList;
    lvItens: TListView;
    mmOutput: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    mmOptions: TMainMenu;
    pcBody: TPageControl;
    pcLeft: TPageControl;
    pnTop: TPanel;
    sbStatus: TStatusBar;
    spDivisor: TSplitter;
    SpeedButton1: TSpeedButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    tvActions: TTreeView;
    procedure lvItensDblClick(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure tvActionsClick(Sender: TObject);
  private
    procedure Internal_AddParam(prSender : TObject; prParam : TJupiterAction);
  protected
    procedure Internal_Prepare; override;
    procedure Internal_UpdateComponents; override;
    procedure Internal_ItemChangeStatus(prSender: TObject; prStatus: TJupiterRunnableItemStatus); override;
    procedure Internal_ItemAddItem(prSender : TObject; prItem : TJupiterListItem); override;
  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}

{ TFMain }

procedure TFMain.tvActionsClick(Sender: TObject);
begin
  if not Assigned(tvActions.Selected) then
    Exit;

  if not Assigned(tvActions.Selected.Data) then
    Exit;

  lvItens.Items.Clear;

  Self.ActionFactory(TJupiterAction(tvActions.Selected.Data), nil);
end;

procedure TFMain.MenuItem5Click(Sender: TObject);
begin
  Self.Internal_Prepare;

  Self.UpdateForm;
end;

procedure TFMain.lvItensDblClick(Sender: TObject);
begin
  if not Assigned(lvItens.Selected) then
    Exit;

  if not Assigned(lvItens.Selected.Data) then
    Exit;

  Self.ActionFactory(nil, TJupiterListItem(lvItens.Selected.Data));
end;

procedure TFMain.SpeedButton1Click(Sender: TObject);
begin
  Application.CreateForm(TFNewAction, FNewAction);
  try
     FNewAction.ShowModal;
  finally
    FNewAction.Release;
  end;
end;

procedure TFMain.Internal_AddParam(prSender: TObject; prParam: TJupiterAction);
var
  vrTree  : TTreeNode;
  vrOwner : TTreeNode;
begin
  tvActions.SortType := stText;

  if prParam.Category <> EmptyStr then
  begin
    vrOwner := tvActions.Items.FindNodeWithText(prParam.Category);

    if not Assigned(vrOwner) then
    begin
      vrOwner               := tvActions.Items.Add(nil, prParam.Category);
      vrOwner.ImageIndex    := prParam.Icon;
      vrOwner.SelectedIndex := prParam.Icon;
    end;
  end;

  if prParam.Category <> EmptyStr then
    vrTree := tvActions.Items.AddChild(vrOwner, prParam.Title)
  else
    vrTree := tvActions.Items.Add(nil, prParam.Title);

  vrTree.ImageIndex    := prParam.Icon;
  vrTree.SelectedIndex := prParam.Icon;
  vrTree.StateIndex    := JUPITER_ICON_NONE;
  vrTree.Data          := prParam;

  if prParam.Category <> EmptyStr then
     vrOwner.Expand(True);

  tvActions.SortType := stBoth;
end;

procedure TFMain.Internal_Prepare;
var
  vrParams : TJupiterParams;
  vrItem   : TTreeNode;
  vrNode   : TJupiterAction;
begin
  inherited Internal_Prepare;

  if not DirectoryExists('./datasets/') then
     CreateDir('./datasets/');

  tvActions.Items.Clear;
  lvItens.Items.Clear;

  vrParams := TJupiterParams.Create;
  try
    vrParams.OnAddParam := @Self.Internal_AddParam;

    vrParams.CheckFile;

    vrParams.List;
  finally
    FreeAndNil(vrParams);
  end;
end;

procedure TFMain.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  lvItens.Enabled := tvActions.Items.Count <> 0;
end;

procedure TFMain.Internal_ItemChangeStatus(prSender: TObject; prStatus: TJupiterRunnableItemStatus);
begin
  inherited Internal_ItemChangeStatus(prSender, prStatus);

  lvItens.Enabled := prStatus = jrsDone;

  sbStatus.Panels[0].Text := 'Ready';

  if prStatus = jrsRunning then
    sbStatus.Panels[0].Text := 'Working...';
end;

procedure TFMain.Internal_ItemAddItem(prSender: TObject; prItem: TJupiterListItem);
var
  vrItem : TListItem;
begin
  inherited Internal_ItemAddItem(prSender, prItem);

  lvItens.SortType := stText;

  vrItem         := lvItens.Items.Add;
  vrItem.Caption := prItem.Title;
  vrItem.SubItems.Add(prItem.Description);

  vrItem.ImageIndex := prItem.Icon;
  vrItem.StateIndex := JUPITER_ICON_NONE;

  vrItem.Data := prItem;

  lvItens.SortType := stBoth;
end;

end.

