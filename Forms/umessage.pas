unit uMessage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  uCustomJupiterForm, JupiterApp, JupiterSystemMessage;

type

  { TFMessage }

  TFMessage = class(TFCustomJupiterForm)
    lbTimeDesc: TLabel;
    lbTimeTitle: TLabel;
    lbDetailsTitle: TLabel;
    lbTitleDesc: TLabel;
    lbOriginDesc: TLabel;
    lbTitleTile: TLabel;
    lbResume: TListBox;
    lbOriginTitle: TLabel;
    mmDetails: TMemo;
    Splitter1: TSplitter;
    procedure lbResumeClick(Sender: TObject);
  private
    procedure Internal_ShowMessage(prIndex : Integer);
  protected
    procedure Internal_UpdateDatasets; override;
  public

  end;

var
  FMessage: TFMessage;

implementation

{$R *.lfm}

{ TFMessage }

procedure TFMessage.lbResumeClick(Sender: TObject);
begin
  Self.Internal_ShowMessage(lbResume.ItemIndex);
end;

procedure TFMessage.Internal_ShowMessage(prIndex: Integer);
begin
  if prIndex = -1 then
    Exit;

  lbTitleDesc.Caption  := TJupiterSystemMessage(vrJupiterApp.Messages.GetAtIndex(prIndex)).Title;
  lbOriginDesc.Caption := TJupiterSystemMessage(vrJupiterApp.Messages.GetAtIndex(prIndex)).Origin;
  lbTimeDesc.Caption   := FormatDateTime('dd/mm/yyyy hh:nn:ss', TJupiterSystemMessage(vrJupiterApp.Messages.GetAtIndex(prIndex)).Timestamp);

  mmDetails.Lines.Clear;
  mmDetails.Lines.AddStrings(TJupiterSystemMessage(vrJupiterApp.Messages.GetAtIndex(prIndex)).Details);
end;

procedure TFMessage.Internal_UpdateDatasets;
var
  vrVez : Integer;
begin
  inherited Internal_UpdateDatasets;

  lbResume.Items.Clear;

  for vrVez := 0 to vrJupiterApp.Messages.Size - 1 do
    lbResume.Items.Add(Format('%0:s - %1:s', [FormatDateTime('dd/mm/yyyy hh:nn:ss', TJupiterSystemMessage(vrJupiterApp.Messages.GetAtIndex(vrVez)).Timestamp), TJupiterSystemMessage(vrJupiterApp.Messages.GetAtIndex(vrVez)).Title]));

  if vrJupiterApp.Messages.Size > 0 then
    Self.Internal_ShowMessage(0);
end;

end.

