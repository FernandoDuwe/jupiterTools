unit uMenuNavigator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  uJupiterForm, jupiterformutils, jupitertreeviewmenugenerator, JupiterApp,
  JupiterConsts, jupiterDesktopApp, jupiterDatabaseWizard;

type

  { TFMenuNavigator }

  TFMenuNavigator = class(TFJupiterForm)
    pnBody: TPanel;
    Splitter1: TSplitter;
    tvTreeMenu: TTreeView;
    procedure FormDestroy(Sender: TObject);
  private
    FCurrentData : TJupiterDatabaseReference;
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

procedure TFMenuNavigator.FormDestroy(Sender: TObject);
begin
  if Assigned(FForm) then
    FForm.Destroy;

  inherited;
end;

procedure TFMenuNavigator.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  Self.Repaint;
  Self.Refresh;

  if miLookColumn.Checked then
    tvTreeMenu.Width := PercentOfScreen(Self.Width, Self.PercentDivisor);

  if Assigned(Self.FForm) then
    if Self.FForm is TFJupiterForm then
      TFJupiterForm(Self.FForm).UpdateForm();

  pnBody.Caption := EmptyStr;

  if not Self.Showing then
    Exit;

  if not Assigned(tvTreeMenu.Selected) then
    Exit;

  // Caso o formulário tenhas sido fechado
  if not Assigned(Self.FForm) then
  begin
    pnBody.Caption := 'Selecione um item para exibir a rota correspondente';

    Exit;
  end;

  if not Self.FForm.Showing then
  begin
    pnBody.Caption := 'Selecione um item para exibir a rota correspondente';

    Exit;
  end;
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

    Self.Caption := 'Menu: ' + vrTreeView.MenuRoute;

    vrTreeView.OnClickExecuting := @Internal_TreeClicked;
    vrTreeView.SingleClick := True;
    vrTreeView.FormSender := Self.FormID;
    vrTreeView.TreeView := tvTreeMenu;
    vrTreeView.Render;

    Self.FClicKComponent := tvTreeMenu.OnClick;

    tvTreeMenu.OnClick := @Internal_TreeClicked;

    if tvTreeMenu.Items.Count > 0 then
    begin
      tvTreeMenu.Selected := tvTreeMenu.Items[0];
      Internal_TreeClicked(tvTreeMenu);
    end;
  finally
    FreeAndNil(vrTreeView);
  end;
end;

procedure TFMenuNavigator.Internal_TreeClicked(Sender: TObject);
var
  vrWizard : TJupiterDatabaseWizard;
begin
  if not Assigned(TTreeView(Sender).Selected) then
    Exit;

  if not Assigned(TTreeView(Sender).Selected.Data) then
    Exit;

  // Se é a mesma rota que a atual
  if Assigned(Self.FCurrentData) then
    if Self.FCurrentData.ID = TJupiterDatabaseReference(TTreeView(Sender).Selected.Data).ID then
      Exit;

  vrWizard := vrJupiterApp.NewWizard;
  try
    if vrWizard.Exists('ROUTES', ' ID = ' + IntToStr(TJupiterDatabaseReference(TTreeView(Sender).Selected.Data).ID) + ' AND DESTINY IS NULL ') then
      Exit;

    Self.FCurrentData := TJupiterDatabaseReference(TTreeView(Sender).Selected.Data);

    Self.FClickItem := True;

    if Assigned(Self.FClicKComponent) then
      Self.FClicKComponent(Sender);
  finally
    FreeAndNil(vrWizard);
  end;
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

