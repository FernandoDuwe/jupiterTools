unit uNewTask;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  uJupiterForm, jupiterformutils, jupitertreeviewmenugenerator, JupiterApp,
  JupiterConsts, JupiterObject, jupiterDesktopApp, jupiterformcomponenttils,
  uJupiterAction, uJupiterDesktopAppScript, StdCtrls, Types;

type

  { TFNewTask }

  TFNewTask = class(TFJupiterForm)
    edSearchTables: TEdit;
    fpTabs: TFlowPanel;
    imLogo: TImage;
    pnSearchResult: TPanel;
    pnPesquisar: TPanel;
    pnHomeBody: TPanel;
    pnTop: TPanel;
    sbShortcut: TScrollBox;
    sbSearchResult: TScrollBox;
    Splitter1: TSplitter;
    tvTreeMenu: TTreeView;
    procedure edSearchTablesKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Splitter1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Splitter1Moved(Sender: TObject);
  private
    FCustomColor : TColor;
    FReferences : TJupiterObjectList;
    FCurrentLine : Integer;
    FActionList : TJupiterActionGroup;

    function Internal_GetNextColor : TColor;

    procedure Internal_LinkClick(Sender: TObject);

    procedure Internal_UpdateComponents; override;

    procedure Internal_PrepareForm; override;

    procedure Internal_ListContextMenu;

    procedure Internal_DrawForm;
  public

  end;

var
  FNewTask: TFNewTask;

implementation

{$R *.lfm}

{ TFNewTask }

procedure TFNewTask.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FReferences := TJupiterObjectList.Create;
end;

procedure TFNewTask.edSearchTablesKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    JupiterAppDesktopOpenDatbaseFinder(edSearchTables.Text);

    Key := #0;
  end;
end;

procedure TFNewTask.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Self.FReferences);

  inherited;
end;

procedure TFNewTask.Splitter1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TFNewTask.Splitter1Moved(Sender: TObject);
begin
  miLookColumn.Checked := False;
end;

function TFNewTask.Internal_GetNextColor: TColor;
begin
  try
    if Self.FCustomColor = $00FFC175 then
    begin
      Self.FCustomColor := clSkyBlue;
      Exit;
    end;

    if Self.FCustomColor = clSkyBlue then
    begin
      Self.FCustomColor := clCream;
      Exit;
    end;

    if Self.FCustomColor = clCream then
    begin
      Self.FCustomColor := clMoneyGreen;
      Exit;
    end;

    if Self.FCustomColor = clMoneyGreen then
    begin
      Self.FCustomColor := clSilver;
      Exit;
    end;

    Self.FCustomColor := $00FFC175;
  finally
    Result := Self.FCustomColor;
  end;
end;

procedure TFNewTask.Internal_LinkClick(Sender: TObject);
begin
  if (Sender is TLabel) then
    Self.FActionList.GetActionAtIndex(TLabel(Sender).Tag).Execute;

  if (Sender is TPanel) then
    Self.FActionList.GetActionAtIndex(TPanel(Sender).Tag).Execute;
end;

procedure TFNewTask.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if miLookColumn.Checked then
    tvTreeMenu.Width := PercentOfScreen(Self.Width, Self.PercentDivisor);
end;

procedure TFNewTask.Internal_PrepareForm;
var
  vrTreeView : TJupiterTreeViewMenuGenerator;
begin
  inherited Internal_PrepareForm;

  tvTreeMenu.Images := TJupiterDesktopApp(vrJupiterApp).ImageList;

  Self.Internal_ListContextMenu;

  Self.Internal_DrawForm;

  vrTreeView := TJupiterTreeViewMenuGenerator.Create(vrJupiterApp.InternalDatabase);
  try
    vrTreeView.TreeView := tvTreeMenu;
    vrTreeView.Render;
  finally
    FreeAndNil(vrTreeView);
  end;
end;

procedure TFNewTask.Internal_ListContextMenu;
var
  vrVez : Integer;
  vrAction : TJupiterAction;
  vrReference : TJupiterComponentReference;
  vrAlternative : Boolean;
begin
  Self.FCurrentLine := FORM_MARGIN_TOP;

  Self.FActionList := TJupiterDesktopApp(vrJupiterApp).GenerateContextMenu;

  for vrVez := 0 to Self.FActionList.Count - 1 do
  begin
    vrAction := Self.FActionList.GetAtIndex(vrVez) as TJupiterAction;

    vrReference := JupiterComponentsNewCard(vrAction.Caption, vrAction.Hint, 80, PercentOfScreen(fpTabs.Width, vrJupiterApp.Params.VariableById('Interface.Cards.Size').AsInteger), TJupiterPosition.Create(0, 0), fpTabs);

    vrAlternative := False;

    if vrJupiterApp.Params.VariableById('Interface.Cards.Zebring').AsInteger <> 0 then
      vrAlternative := (vrVez mod vrJupiterApp.Params.VariableById('Interface.Cards.Zebring').AsInteger) = 0;

    TPanel(vrReference.Component).BorderSpacing.Around := 2;

    if vrAlternative then
      if vrJupiterApp.Params.Exists('Interface.Form.Color.Alternative') then
        TPanel(vrReference.Component).Color := vrJupiterApp.Params.VariableById('Interface.Form.Color.Alternative').AsColor
	  else
	    TPanel(vrReference.Component).Color := ALTERNATIVE_COLOR; 

    TPanel(vrReference.Component).ParentBackground := not vrAlternative;
    TPanel(vrReference.Component).ParentColor := not vrAlternative;

//      TPanel(vrReference.Component).Color := Self.Internal_GetNextColor;

    TPanel(vrReference.Component).Tag     := vrVez;
    TPanel(vrReference.Component).Cursor  := crHandPoint;
    TPanel(vrReference.Component).OnClick := @Internal_LinkClick;
  end;

  edSearchTables.Left := FORM_MARGIN_LEFT * 2;
  edSearchTables.Width := pnPesquisar.Width - ((FORM_MARGIN_LEFT * 2) + (FORM_MARGIN_RIGHT * 2));

  fpTabs.BorderSpacing.Top    := FORM_MARGIN_TOP * 2;
  fpTabs.BorderSpacing.Left   := FORM_MARGIN_LEFT * 2;
  fpTabs.BorderSpacing.Right  := FORM_MARGIN_RIGHT * 2;
  fpTabs.BorderSpacing.Bottom := FORM_MARGIN_BOTTOM * 2;
end;

procedure TFNewTask.Internal_DrawForm;
var
  vrCurrentLine : Integer;
  vrReference : TJupiterComponentReference;
begin
  vrCurrentLine := imLogo.Top;

  vrReference := jupiterformcomponenttils.JupiterComponentsNewLabel('Jupiter', TJupiterPosition.Create(vrCurrentLine, imLogo.Width + imLogo.Left + FORM_MARGIN_LEFT + FORM_MARGIN_LEFT), pnTop);

  vrCurrentLine := vrReference.Bottom + FORM_MARGIN_TOP;

  vrReference := jupiterformcomponenttils.JupiterComponentsNewLabel('Versão: ' + vrJupiterApp.GetVersion, TJupiterPosition.Create(vrCurrentLine, imLogo.Width + imLogo.Left + FORM_MARGIN_LEFT + FORM_MARGIN_LEFT), pnTop);
end;

end.

