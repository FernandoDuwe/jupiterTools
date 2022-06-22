unit uExplorer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, uJupiterForm, JupiterConsts, JupiterApp;

type

  { TFExplorer }

  TFExplorer = class(TJupiterForm)
    imgInfo: TImage;
    lbInfo: TLabel;
    lvReport: TListView;
    pnHint: TPanel;
    procedure lvReportDblClick(Sender: TObject);
  private
    FParams : TJupiterListem;

    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
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

procedure TFExplorer.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  lvReport.Font.Size := StrToInt(vrJupiterApp.Config.GetByID('JupiterTools.UI.Display.FontSize').Value);

  pnHint.Visible := Self.FParams.Hint <> EmptyStr;
  lbInfo.Caption := Self.FParams.Hint;
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

    lvReport.;
  end;
end;

end.

