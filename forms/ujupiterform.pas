unit uJupiterForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, ExtCtrls,
  ButtonPanel, StdCtrls, Menus, ComCtrls, Buttons, JupiterConsts, JupiterFormTabSheet,
  jupiterformutils, JupiterApp, uJupiterAction, jupiterDesktopApp, JupiterVariable;

type

  { TFJupiterForm }

  TFJupiterForm = class(TForm)
    acOptions: TActionList;
    edSearch: TEdit;
    fpOptions: TFlowPanel;
    pnSearchBar: TPanel;
    tmrAutoUpdater: TTimer;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmrAutoUpdaterTimer(Sender: TObject);
  private
    FShowSearchBar : Boolean;
    FActionGroup   : TJupiterActionGroup;
    FOwnerTab      : TJupiterFormTabSheet;
    FParams        : TJupiterVariableList;
    FUpdateCount   : Integer;

    procedure Internal_SetSearchBar(prNewValue : Boolean);
  published
    property ActionGroup   : TJupiterActionGroup  read FActionGroup   write FActionGroup;
    property ShowSearchBar : Boolean              read FShowSearchBar write Internal_SetSearchBar default False;
    property OwnerTab      : TJupiterFormTabSheet read FOwnerTab      write FOwnerTab;
    property Params        : TJupiterVariableList read FParams        write FParams;

    procedure Internal_UpdateComponents; virtual;
    procedure Internal_UpdateDatasets; virtual;
    procedure Internal_UpdateCalcs; virtual;
    procedure Internal_PrepareForm; virtual;
    procedure Internal_Resize; virtual;
    function Internal_OnRequestData : TJupiterVariableList; virtual;
  public
    procedure PrepareForm; virtual;
    procedure UpdateForm(prUpdateDatasets : Boolean = True; prUpdateComponentes : Boolean = True; prUpdateCalcs : Boolean = True); virtual;

    procedure DoSecureClose;
  end;

var
  FJupiterForm: TFJupiterForm;

implementation

{$R *.lfm}

{ TFJupiterForm }

procedure TFJupiterForm.FormShow(Sender: TObject);
begin
  try
    Self.PrepareForm;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFJupiterForm.tmrAutoUpdaterTimer(Sender: TObject);
begin
  Self.FUpdateCount := Self.FUpdateCount + 1;

  if (Self.FUpdateCount >= 3) then
    Self.FUpdateCount := 0;

  tmrAutoUpdater.Enabled := False;

  Self.UpdateForm(Self.FUpdateCount = 0);

  tmrAutoUpdater.Enabled := True;
end;

procedure TFJupiterForm.Internal_SetSearchBar(prNewValue: Boolean);
begin
  try
    Self.FShowSearchBar := prNewValue;
  finally
    Self.UpdateForm(False, True, False);
  end;
end;

procedure TFJupiterForm.FormActivate(Sender: TObject);
begin
  Self.UpdateForm();
end;

procedure TFJupiterForm.FormCreate(Sender: TObject);
begin
  Self.FUpdateCount := 1;

  Self.FActionGroup := TJupiterActionGroup.Create;
  Self.FActionGroup.FlowPanel := fpOptions;

  Self.FActionGroup.ImageList := TJupiterDesktopApp(vrJupiterApp).ImageList;

  Self.FActionGroup.OnRequestData := @Internal_OnRequestData;

  Self.FParams := TJupiterVariableList.Create;

  Self.Height := PercentOfScreen(Screen.Height, 80);
  Self.Width  := PercentOfScreen(Screen.Width, 80);
end;

procedure TFJupiterForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Self.FParams);
  FreeAndNil(Self.FActionGroup);
end;

procedure TFJupiterForm.FormResize(Sender: TObject);
begin
  Self.Internal_Resize;
end;

procedure TFJupiterForm.Internal_UpdateComponents;
begin
  DrawForm(Self);

  Self.ActionGroup.UpdateActions;

  pnSearchBar.Visible := Self.ShowSearchBar;

  if pnSearchBar.Visible then
    pnSearchBar.Top := fpOptions.Top + fpOptions.Height + 1;
end;

procedure TFJupiterForm.Internal_UpdateDatasets;
begin

end;

procedure TFJupiterForm.Internal_UpdateCalcs;
begin

end;

procedure TFJupiterForm.Internal_PrepareForm;
begin
  fpOptions.Align := alTop;
end;

procedure TFJupiterForm.Internal_Resize;
begin
  edSearch.Top   := FORM_MARGIN_TOP;
  edSearch.Left  := FORM_MARGIN_LEFT;
  edSearch.Width := pnSearchBar.Width - FORM_MARGIN_LEFT - FORM_MARGIN_RIGHT;

  pnSearchBar.Width := FORM_MARGIN_TOP + edSearch.Height + FORM_MARGIN_BOTTOM;
end;

function TFJupiterForm.Internal_OnRequestData: TJupiterVariableList;
begin
  Result := TJupiterVariableList.Create;
end;

procedure TFJupiterForm.PrepareForm;
begin
  try
    Self.Internal_PrepareForm;
  finally
    Self.FActionGroup.Render;
  end;
end;

procedure TFJupiterForm.UpdateForm(prUpdateDatasets: Boolean; prUpdateComponentes: Boolean; prUpdateCalcs: Boolean);
begin
  if Self.Owner is TJupiterFormTabSheet then
    TJupiterFormTabSheet(Self.Owner).Caption := Self.Caption;

  if prUpdateDatasets then
    Self.Internal_UpdateDatasets;

  if prUpdateComponentes then
    Self.Internal_UpdateComponents;

  if prUpdateCalcs then
    Self.Internal_UpdateCalcs;

  Self.Internal_Resize;
end;

procedure TFJupiterForm.DoSecureClose;
begin
  if Assigned(OwnerTab) then
  begin
    Self.OwnerTab.DoClose;

    Exit;
  end;

  Self.Close;
end;

end.

