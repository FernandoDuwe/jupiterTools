unit uMenuNavigator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  uJupiterForm, jupiterformutils, jupitertreeviewmenugenerator, JupiterApp,
  JupiterConsts, jupiterDesktopApp;

type

  { TFMenuNavigator }

  TFMenuNavigator = class(TFJupiterForm)
    pnBody: TPanel;
    Splitter1: TSplitter;
    tvTreeMenu: TTreeView;
  private
    FForm : TForm;
    FClickItem : Boolean;
    FClicKComponent : TNotifyEvent;

    procedure Internal_UpdateComponents; override;
    procedure Internal_PrepareForm; override;
    procedure Internal_TreeClicked(Sender: TObject);
  published
    property ClickItem : Boolean read FClickItem;
  public
    procedure AddForm(prForm : TForm);
  end;

var
  FMenuNavigator: TFMenuNavigator;

implementation

{$R *.lfm}

{ TFMenuNavigator }

procedure TFMenuNavigator.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if miLookColumn.Checked then
    tvTreeMenu.Width := PercentOfScreen(Self.Width, Self.PercentDivisor);
end;

procedure TFMenuNavigator.Internal_PrepareForm;
var
  vrTreeView : TJupiterTreeViewMenuGenerator;
begin
  inherited Internal_PrepareForm;

  Self.FClickItem := False;

  tvTreeMenu.Images := TJupiterDesktopApp(vrJupiterApp).ImageList;

  vrTreeView := TJupiterTreeViewMenuGenerator.Create(vrJupiterApp.InternalDatabase);
  try
    if Self.Params.Exists(PARAM_PARAMS) then
      vrTreeView.MenuRoute := Self.Params.VariableById(PARAM_PARAMS).Value;

    vrTreeView.OnClickExecuting := @Internal_TreeClicked;
    vrTreeView.SingleClick := True;
    vrTreeView.FormSender := Self.FormID;
    vrTreeView.TreeView := tvTreeMenu;
    vrTreeView.Render;

    Self.FClicKComponent := tvTreeMenu.OnClick;

    tvTreeMenu.OnClick := @Internal_TreeClicked;
  finally
    FreeAndNil(vrTreeView);
  end;
end;

procedure TFMenuNavigator.Internal_TreeClicked(Sender: TObject);
begin
  if not Assigned(TTreeView(Sender).Selected) then
    Exit;

  if not Assigned(TTreeView(Sender).Selected.Data) then
    Exit;

  Self.FClickItem := True;

  if Assigned(Self.FClicKComponent) then
    Self.FClicKComponent(Sender);
end;

procedure TFMenuNavigator.AddForm(prForm: TForm);
begin
  if Assigned(FForm) then
    FForm.Destroy;

  Self.FClickItem := False;

  FForm := prForm;

  Self.FForm.Parent      := pnBody;
  Self.FForm.Left        := 0;
  Self.FForm.Top         := 0;
  Self.FForm.WindowState := wsMaximized;
  Self.FForm.BorderStyle := bsNone;

  Self.FForm.Show;
end;

end.

