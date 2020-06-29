unit uNewAction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  JupiterForm, jupiter, jupiterconsts, JupiterRunnableItem;

type

  { TFNewAction }

  TFNewAction = class(TJupiterForm)
    btnSave: TButton;
    cbListAction: TComboBox;
    edFilter: TLabeledEdit;
    edFlags: TLabeledEdit;
    edCategory: TLabeledEdit;
    edTitle: TLabeledEdit;
    edParams: TLabeledEdit;
    lbListAction: TLabel;
    pnBody: TPanel;
    pnTop: TPanel;
  private
    procedure Internal_Prepare; override;
  public

  end;

var
  FNewAction: TFNewAction;

implementation

{$R *.lfm}

{ TFNewAction }

procedure TFNewAction.Internal_Prepare;
var
  vrJupiter : TJupiter;
  vrVez     : Integer;
  vrClass   : ClassRunnableItem;
begin
  inherited Internal_Prepare;

  cbListAction.Items.Clear;

  vrJupiter := TJupiter.Create;
  try
    for vrVez := 0 to vrJupiter.List.Count - 1 do
    begin
      vrClass := ClassRunnableItem(vrJupiter.List[vrVez]);

      cbListAction.Items.Add(vrClass.ListAction);
    end;
  finally
    FreeAndNil(vrJupiter);
  end;

  cbListAction.ItemIndex := 0;
end;

end.

