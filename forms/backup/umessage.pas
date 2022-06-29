unit uMessage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, uJupiterForm, JupiterApp, jupiterLog;

type

  { TFMessage }

  TFMessage = class(TJupiterForm)
    GroupBox1: TGroupBox;
    lbDetails: TListBox;
    lvMessage: TListView;
    sbStatus: TStatusBar;
    Splitter1: TSplitter;
    procedure FormShow(Sender: TObject);
    procedure lvMessageClick(Sender: TObject);
  private
    procedure Internal_UpdateDatasets; override;
  public

  end;

var
  FMessage: TFMessage;

implementation

{$R *.lfm}

{ TFMessage }

procedure TFMessage.FormShow(Sender: TObject);
begin
  if vrJupiterApp.Config.GetByID('JupiterTools.UI.Display.WindowsState').Value = 'Maximized' then
    Self.WindowState := wsMaximized;
end;

procedure TFMessage.lvMessageClick(Sender: TObject);
var
  vrObj : TJupiterLogNode;
begin
  if not Assigned(lvMessage.Selected) then
    Continue;

  if not Assigned(lvMessage.Selected.Data) then
    Continue;

  vrObj := TJupiterLogNode(lvMessage.Selected.Data);

  lbDetails.Items.Clear;
  lbDetails.Items.Add('Data: ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', vrObj.Data));
  lbDetails.Items.Add('Origem: ' + vrObj.Message);
  lbDetails.Items.Add('Mensagem: ' + vrObj.Details);
end;

procedure TFMessage.Internal_UpdateDatasets;
var
  vrVez  : Integer;
  vrNode : TListItem;
  vrObj  : TJupiterLogNode;
begin
  inherited Internal_UpdateDatasets;

  lvMessage.Items.Clear;

  for vrVez := 0 to vrJupiterApp.Log.Count - 1 do
  begin
    vrObj := vrJupiterApp.Log.GetByIndex(vrVez);

    vrNode := lvMessage.Items.Add;
    vrNode.Caption := FormatDateTime('dd/mm/yyyy hh:nn:ss', vrObj.Data);
    vrNode.SubItems.Add(vrObj.Message);
    vrNode.SubItems.Add(vrObj.Details);
    vrNode.Data := vrObj;
  end;
end;

end.

