unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  Menus, JupiterApp, JupiterConfig, JupiterModule, uJupiterForm, uConfig,
  uExplorer, JupiterConsts;

type

  { TFMain }

  TFMain = class(TJupiterForm)
    ilMainIcons: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    miModules: TMenuItem;
    miMessages: TMenuItem;
    Separator1: TMenuItem;
    miConfig: TMenuItem;
    mMenu: TMainMenu;
    pnBody: TPanel;
    pnLeft: TPanel;
    sbStatus: TStatusBar;
    Splitter1: TSplitter;
    tvItens: TTreeView;
    procedure FormResize(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure miModulesClick(Sender: TObject);
    procedure miConfigClick(Sender: TObject);
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
    tvItens.Items.Clear;

    for vrCount := 0 to vrJupiterApp.ModuleCount - 1 do
        TJupiterModule(vrJupiterApp.GetModuleByIndex(vrCount)).GetTasks(tvItens);
  end;

  if Assigned(Self.FCurrentForm) then
     Self.FCurrentForm.UpdateForm;
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
  end;

  Self.FCurrentForm.Parent      := pnBody;
  Self.FCurrentForm.WindowState := wsMaximized;
  Self.FCurrentForm.BorderStyle := bsNone;
  Self.FCurrentForm.Align       := alClient;
  Self.FCurrentForm.Show;
end;

procedure TFMain.MenuItem7Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFMain.FormResize(Sender: TObject);
begin

end;

procedure TFMain.miModulesClick(Sender: TObject);
begin

end;

end.

