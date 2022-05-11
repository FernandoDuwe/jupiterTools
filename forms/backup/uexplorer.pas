unit uExplorer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, uJupiterForm,
  JupiterConsts, JupiterApp;

type

  { TFExplorer }

  TFExplorer = class(TJupiterForm)
    lvReport: TListView;
    procedure lvReportDblClick(Sender: TObject);
  private
    FParams : TJupiterListem;

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
begin
  if not Assigned(lvReport.Selected) then
    Exit;

  if not Assigned(lvReport.Selected.Data) then
    Exit;

  try
    vrJupiterApp.RunListable(Self.Params, TJupiterListableItem(lvReport.Selected.Data));
  finally
    Self.UpdateForm;
  end;
end;

procedure TFExplorer.Internal_UpdateDatasets;
var
  vrList : TList;
  vrVez  : Integer;
  vrNode : TListItem;
  vrItem : TJupiterListableItem;
begin
  lvReport.SmallImages := FMain.ilMainIcons;
  lvReport.StateImages := FMain.ilMainIcons;
  lvReport.LargeImages := FMain.ilMainIcons;

  inherited Internal_UpdateDatasets;

  lvReport.Items.Clear;
  lvReport.SortType := stNone;

  vrList := TList.Create;
  try
    vrList.Clear;

    vrJupiterApp.ListItems(Self.Params, vrList);

    for vrVez := 0 to vrList.Count - 1 do
    begin
      vrItem := TJupiterListableItem(vrList[vrVez]);

      vrNode := lvReport.Items.Add;
      vrNode.Caption := vrItem.Item;
      vrNode.SubItems.Add(vrItem.Descricao);

      vrNode.ImageIndex := -1;
      vrNode.Data := vrItem;
    end;
  finally
    FreeAndNil(vrList);

    lvReport.SortType := stText;
  end;
end;

end.

