unit uProcessMonitor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, JupiterForm, JupiterAction, JupiterApp, JupiterConsts,
  jupiterScriptFunctions, JupiterRunnable, jupiterThread, LCLType;

type

  { TFProcessMonitor }

  TFProcessMonitor = class(TFJupiterForm)
    cbNotStarted: TCheckBox;
    cbRunning: TCheckBox;
    cbFinished: TCheckBox;
    lvProcessList: TListView;
    pnProcessSelector: TPanel;
    tmrUpdateScreen: TTimer;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure lvProcessListKeyPress(Sender: TObject; var Key: char);
    procedure lvProcessListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure tmrUpdateScreenTimer(Sender: TObject);
  private
    FCurrentSelected : Integer;

    procedure Internal_NewProcess(Sender : TObject);
    procedure Internal_StopProcess(Sender : TObject);
    procedure Internal_ListenerOnUpdate;

    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_SetCurrentSelected(prNew : Integer);
  published
    property CurrentSelected : Integer read FCurrentSelected write Internal_SetCurrentSelected;
  public
    procedure UpdateForm(prUpdateDatasets : Boolean = True; prUpdateComponentes : Boolean = True; prUpdateCalcs : Boolean = True); override;
  end;

var
  FProcessMonitor: TFProcessMonitor;

implementation

{$R *.lfm}

{ TFProcessMonitor }

procedure TFProcessMonitor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  tmrUpdateScreen.Enabled := False;
end;

procedure TFProcessMonitor.lvProcessListKeyPress(Sender: TObject; var Key: char
  );
begin

end;

procedure TFProcessMonitor.FormActivate(Sender: TObject);
begin
  inherited;
end;

procedure TFProcessMonitor.lvProcessListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if not Assigned(Item) then
    Exit;

  if not Assigned(Item.Data) then
    Exit;

  Self.CurrentSelected := TJupiterThread(Item.Data).ID;
end;

procedure TFProcessMonitor.tmrUpdateScreenTimer(Sender: TObject);
begin
  tmrUpdateScreen.Enabled := False;

  Self.UpdateForm;
end;

procedure TFProcessMonitor.Internal_NewProcess(Sender: TObject);
var
  vrFile : String;
begin
  vrFile := JupiterInputText('Arquivo a executar');

  if Trim(vrFile) = EmptyStr then
  begin
    JupiterShowErrorMessage('Nenhum arquivo informado. Operação cancelada.');
    Exit;
  end;

  vrJupiterApp.Threads.NewThread('Processo adicionado pelo usuário', TJupiterRunnable.Create(vrFile, False));
end;

procedure TFProcessMonitor.Internal_StopProcess(Sender: TObject);
begin
  vrJupiterApp.Threads.ThreadByD(Self.CurrentSelected).Terminate;
end;

procedure TFProcessMonitor.Internal_ListenerOnUpdate;
begin
  try
    Self.UpdateForm;
  finally

  end;
end;

procedure TFProcessMonitor.Internal_PrepareForm;
begin
  Self.FCurrentSelected := NULL_KEY;

  inherited Internal_PrepareForm;

  Self.Actions.Add(TJupiterAction.Create('Novo processo', @Internal_NewProcess));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para criar um novo processo';
    Icon := ICON_ADD;
  end;

  Self.Actions.Add(TJupiterAction.Create('Parar', @Internal_StopProcess));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para parar a thread selecionada';
    Icon := ICON_MARK;
  end;

  Self.Hint := 'Aqui são listados todos os processos em execução no sistema.';
end;

procedure TFProcessMonitor.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if Self.Actions.Count >= 1 then
    if Assigned(Self.Actions.GetActionButton(1, sbActions)) then
      Self.Actions.GetActionButton(1, sbActions).Enabled := Self.FCurrentSelected <> NULL_KEY;
end;

procedure TFProcessMonitor.Internal_UpdateDatasets;
var
  vrVez  : Integer;
  vrItem : TListItem;
begin
  inherited Internal_UpdateDatasets;

  lvProcessList.Items.Clear;

  for vrVez := vrJupiterApp.Threads.Count - 1 downto 0 do
  begin
    if not Assigned(vrJupiterApp.Threads.ThreadByIndex(vrVez)) then
      Continue;

    if ((vrJupiterApp.Threads.ThreadByIndex(vrVez).Status = jtsNotStarted) and (not cbNotStarted.Checked)) then
      Continue;

    if ((vrJupiterApp.Threads.ThreadByIndex(vrVez).Status = jtsRunning) and (not cbRunning.Checked)) then
      Continue;

    if ((vrJupiterApp.Threads.ThreadByIndex(vrVez).Status = jtsFinished) and (not cbFinished.Checked)) then
      Continue;

    vrItem := lvProcessList.Items.Add;
    vrItem.Caption := IntToStr(vrJupiterApp.Threads.ThreadByIndex(vrVez).ID);

    vrItem.SubItems.Add(vrJupiterApp.Threads.ThreadByIndex(vrVez).Title);

    if Assigned(vrJupiterApp.Threads.ThreadByIndex(vrVez).Runnable) then
      vrItem.SubItems.Add(vrJupiterApp.Threads.ThreadByIndex(vrVez).Runnable.CommandLine)
    else
      vrItem.SubItems.Add(EmptyStr);

    case vrJupiterApp.Threads.ThreadByIndex(vrVez).Status of
      jtsNotStarted : vrItem.SubItems.Add('Não iniciado');
      jtsRunning    : vrItem.SubItems.Add('Executando');
      jtsFinished   : vrItem.SubItems.Add('Finalizado');
    end;

    vrItem.SubItems.Add(FormatDateTime('dd/mm/yyyy hh:nn:ss', vrJupiterApp.Threads.ThreadByIndex(vrVez).StartedAt));

    if vrJupiterApp.Threads.ThreadByIndex(vrVez).Status <> jtsFinished then
      vrItem.SubItems.Add(EmptyStr)
    else
      vrItem.SubItems.Add(FormatDateTime('dd/mm/yyyy hh:nn:ss', vrJupiterApp.Threads.ThreadByIndex(vrVez).EndedAt));

    vrItem.Data := vrJupiterApp.Threads.ThreadByIndex(vrVez);
  end;
end;

procedure TFProcessMonitor.Internal_SetCurrentSelected(prNew: Integer);
begin
  Self.FCurrentSelected := prNew;

  Self.Actions.GetActionButton(1, sbActions).Enabled := Self.FCurrentSelected <> NULL_KEY;
end;

procedure TFProcessMonitor.UpdateForm(prUpdateDatasets: Boolean;
  prUpdateComponentes: Boolean; prUpdateCalcs: Boolean);
begin
  inherited UpdateForm;

  tmrUpdateScreen.Enabled := True;
end;

end.

