unit uContextMenu;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ActnList,
  uJupiterForm, jupiterformutils, JupiterApp, JupiterConsts, uJupiterAction,
  jupiterDesktopApp;

type

  { TFContextMenu }

  TFContextMenu = class(TFJupiterForm)
    acExit: TAction;
    acSearch: TAction;
    lvRoute: TListView;
    procedure acExitExecute(Sender: TObject);
    procedure acSearchExecute(Sender: TObject);
    procedure edSearchKeyPress(Sender: TObject; var Key: char);
  private
    procedure Internal_PrepareForm; override;

    procedure Internal_UpdateDatasets; override;
  public

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
begin
  if edSearch.Focused then
  begin
    Self.UpdateForm();

    Exit;
  end;

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

procedure TFContextMenu.edSearchKeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TFContextMenu.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.ShowSearchBar := True;

  Width := PercentOfScreen(Screen.Width, 50);
  Height := PercentOfScreen(Screen.Height, 50);

  BorderStyle := bsDialog;

  edSearch.SetFocus;

  lvRoute.LargeImages := TJupiterDesktopApp(vrJupiterApp).ImageList;
  lvRoute.SmallImages := TJupiterDesktopApp(vrJupiterApp).ImageList;
  lvRoute.StateImages := TJupiterDesktopApp(vrJupiterApp).ImageList;
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
    vrListItem.Caption := vrActionList.GetActionAtIndex(vrVez).Caption;
    vrListItem.ImageIndex := vrActionList.GetActionAtIndex(vrVez).Icon;
    vrListItem.StateIndex := NULL_KEY;
    vrListItem.Data := vrActionList.GetActionAtIndex(vrVez);
  end;

  if lvRoute.Items.Count > 0 then
    lvRoute.Selected := lvRoute.Items[0];
end;

end.

