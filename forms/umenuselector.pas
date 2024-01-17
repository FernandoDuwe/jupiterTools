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
  vrIndex := tvMenu.Selected.AbsoluteIndex - 1;

  if vrIndex >= 0 then
    tvMenu.Selected := tvMenu.Items[vrIndex];
end;

procedure TFMenuSelector.acAbaixoExecute(Sender: TObject);
var
  vrIndex : Integer;
begin
  vrIndex := tvMenu.Selected.AbsoluteIndex + 1;

  if vrIndex < tvMenu.Items.Count then
    tvMenu.Selected := tvMenu.Items[vrIndex];
end;

procedure TFMenuSelector.acEnterExecute(Sender: TObject);
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

  Self.Close;
end;

procedure TFMenuSelector.FormShow(Sender: TObject);
begin
  inherited;

  try
    Self.Position := poDefault;

    Self.Width  := PercentOfScreen(Screen.Width, 50);
    Self.Height := PercentOfScreen(Screen.Height, 50);
  finally
    Self.Position := poScreenCenter;
  end;
end;

procedure TFMenuSelector.tmSearchTimer(Sender: TObject);
begin
  Self.Search(edSearch.Text);

  tmSearch.Enabled := False;
end;

procedure TFMenuSelector.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

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

  if tvMenu.Items.Count > 0 then
    tvMenu.Selected := tvMenu.Items[0];
end;

end.

