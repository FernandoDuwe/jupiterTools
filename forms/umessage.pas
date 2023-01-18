unit uMessage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  uCustomJupiterForm, JupiterApp, JupiterSystemMessage, JupiterAction,
  JupiterRunnable, JupiterConsts, jupiterformutils;

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
    procedure FormShow(Sender: TObject);
    procedure lbResumeClick(Sender: TObject);
  private
    procedure Internal_ShowMessage(prIndex : Integer);

    procedure Internal_PrepareForm; override;

    procedure Internal_RefreshClick(Sender: TObject);
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

procedure TFMessage.FormShow(Sender: TObject);
begin
  inherited;

  lbResume.Width := PercentOfScreen(Self.Width, 30);
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

procedure TFMessage.Internal_PrepareForm;
var
  vrAction : TJupiterAction;
begin
  inherited Internal_PrepareForm;

  vrAction      := TJupiterAction.Create('Atualizar', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para atualizar a página';
  vrAction.Icon := ICON_REFRESH;
  vrAction.OnClick := @Internal_RefreshClick;

  Self.Actions.Add(vrAction);

  Self.Hint := 'Todas as ações executadas no sistema são gravadas nas mensagens e podem ser consultadas nessa tela';
end;

procedure TFMessage.Internal_RefreshClick(Sender: TObject);
begin
  Self.UpdateForm;
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

