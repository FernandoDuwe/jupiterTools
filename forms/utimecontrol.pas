unit utimecontrol;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Arrow, Calendar, EditBtn, JupiterForm, JupiterConsts, JupiterAction,
  jupiterformutils, jupiterTimeControlDataProvider, JupiterRunnable;

type

  { TFTimeControl }

  TFTimeControl = class(TFJupiterForm)
    dtCurrentDate: TDateEdit;
    lbTimes: TLabel;
    lbDateDesc: TLabel;
    lbDateDesc1: TLabel;
    lvTimes: TListView;
    pnDetails: TPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTimeDataProvider : TJupiterTimeControlDataProvider;

    procedure Internal_MarcarTempoInicial(Sender : TObject);
    procedure Internal_MarcarTempoFinal(Sender : TObject);
    procedure Internal_LimparTempos(Sender : TObject);
    procedure Internal_CriarTemposPadroes(Sender : TObject);
    procedure Internal_AbrirArquivoExternamente(Sender : TObject);

    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_UpdateCalcs; override;
  public

  end;

var
  FTimeControl: TFTimeControl;

implementation

{$R *.lfm}

{ TFTimeControl }

procedure TFTimeControl.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FTimeDataProvider := TJupiterTimeControlDataProvider.Create;
end;

procedure TFTimeControl.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Self.FTimeDataProvider);

  inherited;
end;

procedure TFTimeControl.Internal_MarcarTempoInicial(Sender: TObject);
begin
  try
    Self.FTimeDataProvider.RegisterStartTime;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFTimeControl.Internal_MarcarTempoFinal(Sender: TObject);
begin
  try
    Self.FTimeDataProvider.RegisterEndTime;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFTimeControl.Internal_LimparTempos(Sender: TObject);
begin
  try
    Self.FTimeDataProvider.CleanTimes;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFTimeControl.Internal_CriarTemposPadroes(Sender: TObject);
begin
  try
    Self.FTimeDataProvider.CreateStandardTimes;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFTimeControl.Internal_AbrirArquivoExternamente(Sender: TObject);
begin
  TJupiterRunnable.Create(Self.FTimeDataProvider.Filename, True);
end;

procedure TFTimeControl.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  dtCurrentDate.Date := Now;

  Self.FTimeDataProvider.CurrentDate := dtCurrentDate.Date;

  Self.Hint := 'Controle os seus horários, tarefas executadas e o tempo executado.';

  Self.Params.AddVariable(FIELD_ID_GENERADOR, 'TimeControlForm', 'ID do formulário');

  Self.Actions.Add(TJupiterAction.Create('Tempo inicial', @Internal_MarcarTempoInicial));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para marcar o tempo inicial da sua tarefa';
    Icon := ICON_STARTTIME;
  end;

  Self.Actions.Add(TJupiterAction.Create('Tempo final', @Internal_MarcarTempoFinal));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para marcar o tempo final da sua tarefa';
    Icon := ICON_ENDTIME;
  end;

  Self.Actions.Add(TJupiterAction.Create('Limpar Tempos', @Internal_LimparTempos));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui limpar todos os tempos registrados';
    Icon := ICON_DELETE;

    ConfirmBeforeExecute := True;
  end;

  Self.Actions.Add(TJupiterAction.Create('Abrir Arquivo', @Internal_AbrirArquivoExternamente));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para abrir o arquivo onde os horários são guardados, em um editor de texto';
    Icon := ICON_OPEN;
  end;

  pnDetails.Width := PercentOfScreen(Self.Width, 30);
end;

procedure TFTimeControl.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if Self.Actions.Count > 0 then
  begin
    Self.Actions.GetActionButton(0, sbActions).Enabled := not Self.FTimeDataProvider.Started;
    Self.Actions.GetMenuItem(0).Enabled := not Self.FTimeDataProvider.Started;
  end;

  if Self.Actions.Count > 1 then
  begin
    Self.Actions.GetActionButton(1, sbActions).Enabled := ((Self.FTimeDataProvider.Started) and (not Self.FTimeDataProvider.Ended));
    Self.Actions.GetMenuItem(1).Enabled := ((Self.FTimeDataProvider.Started) and (not Self.FTimeDataProvider.Ended));
  end;

  if Self.Actions.Count > 2 then
  begin
    Self.Actions.GetActionButton(2, sbActions).Enabled := Self.FTimeDataProvider.Count <> 0;
    Self.Actions.GetMenuItem(2).Enabled := Self.FTimeDataProvider.Count <> 0;
  end;
end;

procedure TFTimeControl.Internal_UpdateDatasets;
var
  vrVez  : Integer;
  vrItem : TListItem;
begin
  inherited Internal_UpdateDatasets;

  Self.FTimeDataProvider.CurrentDate := dtCurrentDate.Date;
  Self.FTimeDataProvider.ProvideData;

  lvTimes.Items.Clear;

  for vrVez := 0 to Self.FTimeDataProvider.Count - 1 do
  begin
    vrItem := lvTimes.Items.Add;
    vrItem.Caption := Self.FTimeDataProvider.GetRowByIndex(vrVez).Fields.VariableById('start').Value;
    vrItem.SubItems.Add(Self.FTimeDataProvider.GetRowByIndex(vrVez).Fields.VariableById('end').Value);
    vrItem.SubItems.Add(Self.FTimeDataProvider.GetRowByIndex(vrVez).Fields.VariableById('info').Value);
  end;
end;

procedure TFTimeControl.Internal_UpdateCalcs;
begin
  inherited Internal_UpdateCalcs;

  lbTimes.Caption := FormatDateTime('hh:nn:ss', Self.FTimeDataProvider.ExecutedTime);
end;

end.

