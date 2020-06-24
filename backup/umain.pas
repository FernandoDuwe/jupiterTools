unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ExtCtrls, StdCtrls, JupiterForm, JupiterParams, jupiterconsts;

type

  { TFMain }

  TFMain = class(TJupiterForm)
    ilIcons: TImageList;
    lvItens: TListView;
    mmMsg: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    mmOptions: TMainMenu;
    pcBody: TPageControl;
    pnBody: TPanel;
    pnActions: TPanel;
    sbStatus: TStatusBar;
    spMessage: TSplitter;
    spSeparator: TSplitter;
    TabSheet1: TTabSheet;
    tvActions: TTreeView;
    procedure tvActionsClick(Sender: TObject);
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

  Self.ActionFactory(TJupiterParamsNode(tvActions.Selected.Data));
end;

procedure TFMain.Internal_Prepare;
var
  vrParams : TJupiterParams;
  vrItem   : TTreeNode;
  vrNode   : TJupiterParamsNode;
begin
  inherited Internal_Prepare;

  if not DirectoryExists('./datasets/') then
     CreateDir('./datasets/');

  vrParams := TJupiterParams.Create;
  try
    vrParams.CheckFile;
  finally
    FreeAndNil(vrParams);
  end;

  tvActions.Items.Clear;
  lvItens.Items.Clear;

  vrNode            := TJupiterParamsNode.Create;
  vrNode.ListAction := 'ListDirectory';
  vrNode.OptionPath := '/home/duwe/Projetos/';
  vrNode.Param      := '*';
  vrNode.Tags       := '-HIDDENFILES -DIRECTORIES';

  vrItem      := tvActions.Items.Add(nil, 'Listar projetos');
  vrItem.Data := vrNode;
  vrItem.ImageIndex := 0;
  vrItem.SelectedIndex := 0;

  vrNode            := TJupiterParamsNode.Create;
    vrNode.ListAction := 'ListDirectory';
    vrNode.OptionPath := '/home/duwe/';
    vrNode.Param      := '*';
    vrNode.Tags       := '-HIDDENFILES -DIRECTORIES';

    vrItem      := tvActions.Items.Add(nil, 'Listar pasta pessoa');
    vrItem.Data := vrNode;
    vrItem.ImageIndex := 0;
    vrItem.SelectedIndex := 0;

  vrNode            := TJupiterParamsNode.Create;
  vrNode.ListAction := 'ListFromFile';
  vrNode.OptionPath := 'Report.json';

  vrItem      := tvActions.Items.Add(nil, 'Listar relatório');
  vrItem.Data := vrNode;
  vrItem.ImageIndex := 1;
  vrItem.SelectedIndex := 1;
end;

procedure TFMain.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;
end;

procedure TFMain.Internal_ItemChangeStatus(prSender: TObject; prStatus: TJupiterRunnableItemStatus);
begin
  inherited Internal_ItemChangeStatus(prSender, prStatus);

  sbStatus.Panels[0].Text := 'Ready';

  if prStatus = jrsRunning then
    sbStatus.Panels[0].Text := 'Working...';
end;

procedure TFMain.Internal_ItemAddItem(prSender: TObject; prItem: TJupiterListItem);
var
  vrItem : TListItem;
begin
  inherited Internal_ItemAddItem(prSender, prItem);

  vrItem         := lvItens.Items.Add;
  vrItem.Caption := prItem.Title;
  vrItem.SubItems.Add(prItem.Description);

  vrItem.ImageIndex := prItem.Icon;

  vrItem.Data := prItem;
end;

end.

