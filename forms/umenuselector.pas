unit umenuselector;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, ActnList, JupiterForm, jupiterformutils, JupiterApp, JupiterObject,
  JupiterRoute, JupiterConsts, JupiterAction, uMain;

type

  { TFMenuSelector }

  TFMenuSelector = class(TFJupiterForm)
    acESC: TAction;
    acAcima: TAction;
    acAbaixo: TAction;
    acEnter: TAction;
    edSearch: TEdit;
    pnBody: TPanel;
    tmSearch: TTimer;
    tvMenu : TTreeView;
    procedure acAbaixoExecute(Sender: TObject);
    procedure acAcimaExecute(Sender: TObject);
    procedure acEnterExecute(Sender: TObject);
    procedure acESCExecute(Sender: TObject);
    procedure edSearchChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmSearchTimer(Sender: TObject);
  private
  protected
    procedure Internal_PrepareForm; override;

    procedure Internal_UpdateDatasets; override;
  public

  end;

var
  FMenuSelector: TFMenuSelector;

implementation

{$R *.lfm}

{ TFMenuSelector }

procedure TFMenuSelector.acESCExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TFMenuSelector.edSearchChange(Sender: TObject);
begin
  tmSearch.Enabled := False;
  tmSearch.Enabled := True;
end;

procedure TFMenuSelector.acAcimaExecute(Sender: TObject);
var
  vrIndex : Integer;
begin
  vrIndex := 0;

  if Assigned(tvMenu.Selected) then
    vrIndex := tvMenu.Selected.AbsoluteIndex - 1;

  if vrIndex >= 0 then
    tvMenu.Selected := tvMenu.Items[vrIndex];
end;

procedure TFMenuSelector.acAbaixoExecute(Sender: TObject);
var
  vrIndex : Integer;
begin
  vrIndex := 0;

  if Assigned(tvMenu.Selected) then
    vrIndex := tvMenu.Selected.AbsoluteIndex + 1;

  if vrIndex < tvMenu.Items.Count then
    tvMenu.Selected := tvMenu.Items[vrIndex];
end;

procedure TFMenuSelector.acEnterExecute(Sender: TObject);
var
  vrAction : TJupiterAction;
  vrVez : Integer;
begin
  if not Assigned(tvMenu.Selected) then
    Exit;

  if not Assigned(tvMenu.Selected.Data) then
    Exit;

  if tvMenu.MultiSelect then
  begin
    for vrVez := 0 to tvMenu.Items.Count - 1 do
    begin
      if not tvMenu.Items[vrVez].Selected then
        Continue;

      if not Assigned(tvMenu.Items[vrVez].Data) then
        Continue;

      vrAction := TJupiterAction(tvMenu.Items[vrVez].Data);

      if Assigned(vrAction.Route) then
        vrJupiterApp.NavigateTo(vrAction.Route, False);
    end;
  end
  else
  begin
    vrAction := TJupiterAction(tvMenu.Selected.Data);

    if Assigned(vrAction.Route) then
      vrJupiterApp.NavigateTo(vrAction.Route, False);
  end;

  Self.Close;
end;

procedure TFMenuSelector.FormShow(Sender: TObject);
begin
  inherited;

  try
    Self.Position := poDefault;

    Self.Width  := PercentOfScreen(Screen.Width, 50);
    Self.Height := PercentOfScreen(Screen.Height, 70);
  finally
    Self.Position := poScreenCenter;
  end;
end;

procedure TFMenuSelector.tmSearchTimer(Sender: TObject);
begin
  tmSearch.Enabled := False;

  Self.Search(edSearch.Text);
end;

procedure TFMenuSelector.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  tvMenu.MultiSelect := FMain.JupiterFormTab1.Visible;

  Self.SpecialSize := True;
end;

procedure TFMenuSelector.Internal_UpdateDatasets;
var
  vrMenuList : TJupiterObjectList;
begin
  inherited Internal_UpdateDatasets;

  vrMenuList := vrJupiterApp.GetActions(TJupiterRoute.Create(ROOT_PATH));

  tvMenu.Items.Clear;

  ShowRouteOnTreeView(tvMenu, TJupiterRoute.Create(ROOT_PATH), vrMenuList, nil);

  if Trim(edSearch.Text) <> EmptyStr then
    SearchOnTreeView(tvMenu, edSearch.Text);
end;

end.

