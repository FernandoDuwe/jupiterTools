unit uExplorer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Menus, Buttons, uJupiterForm, JupiterConsts, JupiterApp;

type

  { TFExplorer }

  TFExplorer = class(TJupiterForm)
    imgInfo: TImage;
    lbInfo: TLabel;
    lvReport: TListView;
    MenuItem1: TMenuItem;
    pnTaskBar: TPanel;
    spOtherActions: TMenuItem;
    miRelatorio: TMenuItem;
    miIcon: TMenuItem;
    miList: TMenuItem;
    miSmallIcon: TMenuItem;
    Separator1: TMenuItem;
    pnHint: TPanel;
    ppOpcoes: TPopupMenu;
    procedure FormShow(Sender: TObject);
    procedure lvReportDblClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure miRelatorioClick(Sender: TObject);
    procedure miIconClick(Sender: TObject);
    procedure miListClick(Sender: TObject);
    procedure miSmallIconClick(Sender: TObject);
  private
    FParams : TJupiterListem;

    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_BtnClick(Sender: TObject);
    procedure Internal_CreateActions;
  published
    property Params : TJupiterListem read FParams write FParams;
  end;

var
  FExplorer: TFExplorer;

implementation

uses uMain;

{$R *.lfm}


{ TFExplorer }

procedure TFExplorer.lvReportDblClick(Sender: TObject);
var
  vrItem : TJupiterListableItem;
begin
  if not Assigned(lvReport.Selected) then
    Exit;

  if not Assigned(lvReport.Selected.Data) then
    Exit;

  vrItem := TJupiterListableItem(lvReport.Selected.Data);

  try
    vrJupiterApp.RunListable(Self.Params, vrItem);
  finally
    lvReport.Selected.Data := vrItem;

    Self.UpdateForm;
  end;
end;

procedure TFExplorer.FormShow(Sender: TObject);
begin
  Self.Internal_CreateActions;
end;

procedure TFExplorer.MenuItem1Click(Sender: TObject);
var
  vrVez : Integer;
begin
  for vrVez := 0 to lvReport.Columns.Count - 1 do
      lvReport.Column[vrVez].AutoSize := True;
end;

procedure TFExplorer.MenuItem2Click(Sender: TObject);
begin
  Self.UpdateForm;
end;

procedure TFExplorer.miRelatorioClick(Sender: TObject);
begin
  lvReport.ViewStyle := vsReport;

  Self.UpdateForm;
end;

procedure TFExplorer.miIconClick(Sender: TObject);
begin
  lvReport.ViewStyle := vsIcon;

  Self.UpdateForm;
end;

procedure TFExplorer.miListClick(Sender: TObject);
begin
  lvReport.ViewStyle := vsList;

  Self.UpdateForm;
end;

procedure TFExplorer.miSmallIconClick(Sender: TObject);
begin
  lvReport.ViewStyle := vsSmallIcon;

  Self.UpdateForm;
end;

procedure TFExplorer.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  lvReport.Font.Size := StrToInt(vrJupiterApp.Config.GetByID('JupiterTools.UI.Display.FontSize').Value);

  pnHint.Visible := Self.FParams.Hint <> EmptyStr;
  lbInfo.Caption := Self.FParams.Hint;

  miRelatorio.Checked := lvReport.ViewStyle = vsReport;
  miIcon.Checked      := lvReport.ViewStyle = vsIcon;
  miList.Checked      := lvReport.ViewStyle = vsList;
  miSmallIcon.Checked := lvReport.ViewStyle = vsSmallIcon;
end;

procedure TFExplorer.Internal_UpdateDatasets;
var
  vrList : TList;
  vrVez  : Integer;
  vrNode : TListItem;
  vrItem : TJupiterListableItem;
  vrShowDetails : Boolean;
begin
  vrShowDetails := False;

  lvReport.SmallImages := FMain.ilMainIcons;
//  lvReport.StateImages := FMain.ilMainIcons;
  lvReport.LargeImages := FMain.ilMainIcons;

  inherited Internal_UpdateDatasets;

  lvReport.Items.Clear;
  lvReport.DisableAutoSizing;
  lvReport.SortType := stNone;

  vrList := TList.Create;
  try
    vrList.Clear;

    vrJupiterApp.ListItems(Self.Params, vrList);

    vrShowDetails := vrList.Count = 0;

    for vrVez := 0 to vrList.Count - 1 do
    begin
      vrItem := TJupiterListableItem(vrList[vrVez]);

      if Self.FSearchParam <> EmptyStr then
         if ((Pos(AnsiUpperCase(Self.FSearchParam), AnsiUpperCase(vrItem.Item)) = 0) and (Pos(AnsiUpperCase(Self.FSearchParam), AnsiUpperCase(vrItem.Descricao)) = 0)) then
           Continue;

      vrNode := lvReport.Items.Add;
      vrNode.Caption := vrItem.Item;
      vrNode.SubItems.Add(vrItem.Descricao);

      if Trim(vrItem.Descricao) <> EmptyStr then
        vrShowDetails := True;

      vrNode.ImageIndex := vrItem.ImageIndex;

      if vrItem.Selecionado then
        vrNode.ImageIndex := ICON_CHECKED;

      vrNode.Data := vrItem;
    end;
  finally
    FreeAndNil(vrList);

    lvReport.SortType := stText;
    lvReport.SortColumn := 0;
    lvReport.SortDirection:= sdAscending;
    lvReport.Sort;

    lvReport.Column[1].Visible := vrShowDetails;

    if lvReport.Column[0].AutoSize then
      lvReport.EnableAutoSizing;
  end;
end;

procedure TFExplorer.Internal_BtnClick(Sender: TObject);
var
  vrList : TList;
begin
  vrList := TList.Create;
  try
    vrJupiterApp.ListActions(Self.FParams, vrList);

    if Sender is TSpeedButton then
      TJupiterAction(vrList[TSpeedButton(Sender).Tag]).Run(Self.FParams);

    if Sender is TMenuItem then
      TJupiterAction(vrList[TMenuItem(Sender).Tag]).Run(Self.FParams);
  finally
    vrList.Clear;
    FreeAndNil(vrList);

    FMain.UpdateForm;
  end;
end;

procedure TFExplorer.Internal_CreateActions;
var
  vrBtn      : TSpeedButton;
  vrList     : TList;
  vrVez      : Integer;
  vrLeft     : Integer;
  vrMenuItem : TMenuItem;
begin
  vrList := TList.Create;
  try
    vrList.Clear;

    vrJupiterApp.ListActions(Self.FParams, vrList);

    vrLeft := 8;

    for vrVez := 0 to vrList.Count -1 do
    begin
      vrBtn            := TSpeedButton.Create(pnTaskBar);
      vrBtn.Parent     := pnTaskBar;
      vrBtn.Top        := 8;
      vrBtn.Left       := vrLeft;
      vrBtn.Height     := 33;
      vrBtn.Width      := 144;
      vrBtn.Caption    := TJupiterAction(vrList[vrVez]).Title;
      vrBtn.ImageIndex := TJupiterAction(vrList[vrVez]).ImageIndex;
      vrBtn.Images     := FMain.ilMainIcons;
      vrBtn.Hint       := TJupiterAction(vrList[vrVez]).Hint;
      vrBtn.Tag        := vrVez;
      vrBtn.AutoSize   := True;
      vrBtn.OnClick    := @Internal_BtnClick;

      vrMenuItem            := TMenuItem.Create(ppOpcoes);
      vrMenuItem.Caption    := TJupiterAction(vrList[vrVez]).Title;
      vrMenuItem.ImageIndex := TJupiterAction(vrList[vrVez]).ImageIndex;
      vrMenuItem.Hint       := TJupiterAction(vrList[vrVez]).Hint;
      vrMenuItem.Tag        := vrVez;
      vrMenuItem.OnClick    := @Internal_BtnClick;

      ppOpcoes.Items.Add(vrMenuItem);

      spOtherActions.Visible := True;

      vrLeft := vrLeft + (vrBtn.Width) + 8;
    end;

    Application.ProcessMessages;
  finally
    vrList.Clear;
    FreeAndNil(vrList);
  end;
end;

end.

