unit uConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  uJupiterForm, JupiterApp, JupiterConfig;

type

  { TFConfig }

  TFConfig = class(TJupiterForm)
    lvParams: TListView;
    sbStatus: TStatusBar;
  private
    procedure Internal_UpdateDatasets; override;
  public

  end;

var
  FConfig: TFConfig;

implementation

{$R *.lfm}

{ TFConfig }

procedure TFConfig.Internal_UpdateDatasets;
var
  vrVez : Integer;
  vrNode : TListItem;
  vrItem : TJupiterConfigItem;
begin
  inherited Internal_UpdateDatasets;

  lvParams.Items.Clear;

  for vrVez := 0 to vrJupiterApp.Config.Count - 1 do
  begin
    vrNode := lvParams.Items.Add;

    vrItem := vrJupiterApp.Config.GetByIndex(vrVez);

    vrNode.Caption := vrItem.ID;
    vrNode.SubItems.Add(vrItem.Description);
    vrNode.SubItems.Add(vrItem.Value);
    vrNode.Data := vrItem;
  end;
end;

end.

