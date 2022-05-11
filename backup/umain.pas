unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  Menus, StdCtrls, Buttons, JupiterApp, JupiterConfig, JupiterModule,
  uJupiterForm, uConfig, uExplorer, uCurrentTask, uNewTask, JupiterConsts;

type

  { TFMain }

  TFMain = class(TJupiterForm)
    edSearch: TEdit;
    ilMainIcons: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    pnTaskBar: TPanel;
    sbRefresh: TSpeedButton;
    Separator1: TMenuItem;
    miConfig: TMenuItem;
    mMenu: TMainMenu;
    pnBody: TPanel;
    pnLeft: TPanel;
    sbStatus: TStatusBar;
    Splitter1: TSplitter;
    tvItens: TTreeView;
    procedure edSearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure miModulesClick(Sender: TObject);
    procedure miConfigClick(Sender: TObject);
    procedure sbRefreshClick(Sender: TObject);
    procedure tvItensClick(Sender: TObject);
  private
    FCurrentForm : TJupiterForm;

    procedure Internal_UpdateComponents; override;
    procedure Internal_ShowForm(prItem : TJupiterListem);
  public

  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}

{ TFMain }

procedure TFMain.miConfigClick(Sender: TObject);
begin
  Application.CreateForm(TFConfig, FConfig);
  try
    FConfig.ShowModal;
  finally
    FConfig.Release;
    FreeAndNil(FConfig);
  end;
end;

procedure TFMain.sbRefreshClick(Sender: TObject);
begin
  Self.UpdateForm;
end;

procedure TFMain.tvItensClick(Sender: TObject);
begin
  if not Assigned(tvItens.Selected) then
    Exit;

  if not Assigned(tvItens.Selected.Data) then
    Exit;

  Self.Internal_ShowForm(TJupiterListem(tvItens.Selected.Data));
end;

procedure TFMain.Internal_UpdateComponents;
var
  vrCount : Integer;
begin
  inherited Internal_UpdateComponents;

  if tvItens.Items.Count = 0 then
  begin
    tvItens.SortType := stNone;
    tvItens.Items.Clear;

    for vrCount := 0 to vrJupiterApp.ModuleCount - 1 do
        TJupiterModule(vrJupiterApp.GetModuleByIndex(vrCount)).GetTasks(tvItens);

    tvItens.SortType := stText;
  end;

  if Assigned(Self.FCurrentForm) then
     Self.FCurrentForm.UpdateForm;

  sbRefresh.Height := edSearch.Height;
  sbRefresh.Width  := edSearch.Height;

  pnTaskBar.Height := (edSearch.Top * 2) + edSearch.Height;
end;

procedure TFMain.Internal_ShowForm(prItem: TJupiterListem);
begin
  if Assigned(Self.FCurrentForm) then
  begin
    Self.FCurrentForm.Release;
    FreeAndNil(Self.FCurrentForm);
  end;

  case prItem.Tag of
    0 : begin
          Self.FCurrentForm := TFExplorer.Create(pnBody);
          TFExplorer(FCurrentForm).Params := prItem;
        end;
    90 : begin
           Self.FCurrentForm := TFCurrentTask.Create(pnBody);
         end;
   end;

  Self.FCurrentForm.Parent      := pnBody;
  Self.FCurrentForm.WindowState := wsMaximized;
  Self.FCurrentForm.BorderStyle := bsNone;
  Self.FCurrentForm.Align       := alClient;

  try
    Self.FCurrentForm.Show;
  finally
    Self.FCurrentForm.UpdateForm;
  end;
end;

procedure TFMain.MenuItem7Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFMain.FormResize(Sender: TObject);
begin

end;

procedure TFMain.MenuItem6Click(Sender: TObject);
begin
  Application.CreateForm(TFNewTask, FNewTask);
  try
    FNewTask.ShowModal;
  finally
    FNewTask.Release;

    FreeAndNil(FNewTask);

    Self.UpdateForm;
  end;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin

end;

procedure TFMain.edSearchChange(Sender: TObject);
begin
  Self.Search(edSearch.Text);
end;

procedure TFMain.miModulesClick(Sender: TObject);
begin

end;

end.

