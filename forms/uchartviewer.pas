unit uchartViewer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, TAGraph, TASources, TASeries, JupiterForm, jupiterformutils,
  JupiterAction, JupiterRunnable, JupiterConsts, JupiterDialogForm,
  JupiterEnviroment, JupiterApp, jupiterScriptFunctions, JupiterCSVDataProvider,
  JupiterObject, JupiterVariable, JupiterRoute, TACustomSource, TARadialSeries,
  TAStyles, TALegendPanel, TATools, TAMultiSeries, TAFuncSeries, TAChartListbox,
  TAChartImageList, TADbSource, TAChartCombos, TANavigation, TAIntervalSources,
  TATransformations;

type

  { TFChartViewer }

  TFChartViewer = class(TFJupiterForm)
    clListBox: TChartListbox;
    ChartNavPanel1: TChartNavPanel;
    ChartNavScrollBar1: TChartNavScrollBar;
    chChart: TChart;
    chStyle: TChartStyles;
    crToolset: TChartToolset;
    crToolsetDataPointHintTool1: TDataPointHintTool;
    crToolsetLegendClickTool1: TLegendClickTool;
    crToolsetLegendClickTool2: TLegendClickTool;
    crToolsetZoomMouseWheelTool1: TZoomMouseWheelTool;
    lvInfo: TListView;
    pcControls: TPageControl;
    pnInfo: TPanel;
    pnChart: TPanel;
    pcBody: TPageControl;
    spSplitter: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    tmrUpdateAutoComponents: TTimer;
    tsReport: TTabSheet;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    function ListChartSource1Compare(AItem1, AItem2: Pointer): Integer;
    function lsChartSourceCompare(AItem1, AItem2: Pointer): Integer;
    procedure lvInfoDblClick(Sender: TObject);
    procedure tmrUpdateAutoComponentsTimer(Sender: TObject);
  private
    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateDatasets; override;

    procedure Internal_NewMenuItemClick(Sender: TObject);
    procedure Internal_SaveMenuItemClick(Sender: TObject);
    procedure Internal_CreateCharts(prParams : TJupiterVariableList);
    procedure Internal_UpdateCharts(prParams : TJupiterVariableList; prIndex : Integer);
    procedure Internal_HideShowGrid(Sender: TObject);
  public
    procedure PrepareForm; override;

    procedure UpdateForm(prUpdateDatasets : Boolean = True; prUpdateComponentes : Boolean = True; prUpdateCalcs : Boolean = True); override;
  end;

var
  FChartViewer: TFChartViewer;

implementation

uses TAChartUtils;

{$R *.lfm}

{ TFChartViewer }

function TFChartViewer.lsChartSourceCompare(AItem1, AItem2: Pointer): Integer;
begin

end;

procedure TFChartViewer.lvInfoDblClick(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
  vrRoute      : TJupiterRoute;
  vrFile       : String;
begin
  if not Assigned(lvInfo.Selected) then
    Exit;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrRoute := TJupiterRoute.Create(EXPLORER_FORM_PATH);

    vrFile := vrEnviroment.FullPath(lvInfo.Selected.Caption);

    vrRoute.Params.AddVariable('type', DATAPROVIDER_TYPE_LIST_CSV, 'Tipo');
    vrRoute.Params.AddVariable('path', vrFile, 'Arquivo');
    vrRoute.Params.AddVariable('hint', 'Arquivo: ' + vrFile, 'Dica');
    vrRoute.Params.AddVariable('title', 'Arquivo: ' + vrFile, 'Dica');
    vrRoute.Params.AddVariable('hideColumns', 'Line', 'Campos a esconder');

    vrJupiterApp.NavigateTo(vrRoute, True);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFChartViewer.tmrUpdateAutoComponentsTimer(Sender: TObject);
begin
  tmrUpdateAutoComponents.Enabled := False;

  Self.UpdateForm();

  tmrUpdateAutoComponents.Enabled := True;
end;

procedure TFChartViewer.FormResize(Sender: TObject);
begin
  inherited;

  lvInfo.Column[0].Width := PercentOfScreen(lvInfo.Width, 20);
  lvInfo.Column[1].Width := PercentOfScreen(lvInfo.Width, 20);
  lvInfo.Column[2].Width := PercentOfScreen(lvInfo.Width, 20);
  lvInfo.Column[3].Width := PercentOfScreen(lvInfo.Width, 20);
  lvInfo.Column[4].Width := PercentOfScreen(lvInfo.Width, 20);
  lvInfo.Column[5].Width := PercentOfScreen(lvInfo.Width, 20);

  pcControls.Width := PercentOfScreen(Self.Width, 20);
end;

procedure TFChartViewer.FormActivate(Sender: TObject);
begin
  inherited;

  tmrUpdateAutoComponents.Enabled := Self.Showing and Self.Params.Exists('UpdateTimerIntervalTime');
end;

procedure TFChartViewer.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  tmrUpdateAutoComponents.Enabled := False;
end;

procedure TFChartViewer.FormDeactivate(Sender: TObject);
begin
  tmrUpdateAutoComponents.Enabled := False;
end;

procedure TFChartViewer.FormDestroy(Sender: TObject);
begin
  tmrUpdateAutoComponents.Enabled := False;

  inherited;
end;

function TFChartViewer.ListChartSource1Compare(AItem1, AItem2: Pointer
  ): Integer;
begin

end;

procedure TFChartViewer.Internal_PrepareForm;
var
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Internal_PrepareForm;

  if Self.Params.Exists('UpdateTimerIntervalTime') then
  begin
    tmrUpdateAutoComponents.Interval := StrToInt(Self.Params.VariableById('UpdateTimerIntervalTime').Value);
    tmrUpdateAutoComponents.Enabled := True;
  end;

  if not Self.Params.Exists('chartFile') then
    Self.Params.AddVariable('chartFile', '/datasets/chartDatasets.csv', 'Arquivo de configuração com os datasets');

  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.CreateFile(Self.Params.VariableById('chartFile').Value, 'DATASOURCE;FIELDX;FIELDY;FIELDLABEL;FIELDCOLOR;CHARTTYPE;');

    Self.Hint := 'Adicione suas fontes de dados e configure o gráfico desejado.';

    Self.Actions.Add(TJupiterAction.Create('Gráfico', @Internal_NewMenuItemClick));

    with TJupiterAction(Self.Actions.GetLastObject) do
    begin
      Hint := 'Clique aqui para criar um novo gráfico';
      Icon := ICON_ADD;
    end;

    Self.Actions.Add(TJupiterAction.Create('Salvar', @Internal_SaveMenuItemClick));

    with TJupiterAction(Self.Actions.GetLastObject) do
    begin
      Hint := 'Clique aqui para salvar o gráfico atual';
      Icon := ICON_SAVE;
    end;

    Self.Actions.Add(TJupiterAction.Create('Exibir/Esconder Grid', @Internal_HideShowGrid));

    with TJupiterAction(Self.Actions.GetLastObject) do
    begin
      Hint := 'Clique aqui para exibir ou esconder a grid';
      Icon := NULL_KEY;
    end;
  finally
  end;
end;

procedure TFChartViewer.Internal_UpdateDatasets;
var
  vrEnviroment : TJupiterEnviroment;
  vrStr  : TStringList;
  vrCSV  : TJupiterCSVDataProvider;
  vrVez  : Integer;
  vrItem : TListItem;
  vrGenerate : Boolean;
begin
  vrGenerate := False;

  inherited Internal_UpdateDatasets;

  lvInfo.Items.Clear;

  vrGenerate := chChart.Series.Count = 0;

  vrStr := TStringList.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  vrCSV := TJupiterCSVDataProvider.Create;
  try
    vrCSV.Filename := vrEnviroment.FullPath(Self.Params.VariableById('chartFile').Value);
    vrCSV.ProvideData;

    for vrVez := 0 to vrCSV.Count - 1 do
      with vrCSV.GetRowByIndex(vrVez) do
      begin
        vrItem := lvInfo.Items.Add;

        vrItem.Caption := Fields.VariableById('DATASOURCE').Value;

        vrItem.SubItems.Add(Fields.VariableById('FIELDX').Value);
        vrItem.SubItems.Add(Fields.VariableById('FIELDY').Value);
        vrItem.SubItems.Add(Fields.VariableById('FIELDLABEL').Value);
        vrItem.SubItems.Add(Fields.VariableById('FIELDCOLOR').Value);
        vrItem.SubItems.Add(Fields.VariableById('CHARTTYPE').Value);

        if vrGenerate then
          Self.Internal_CreateCharts(Fields)
        else
          Self.Internal_UpdateCharts(Fields, vrVez);
      end;
  finally
    FreeAndNil(vrCSV);
    FreeAndNil(vrEnviroment);
    FreeAndNil(vrStr);
  end;
end;

procedure TFChartViewer.Internal_NewMenuItemClick(Sender: TObject);
var
  vrDialog : TJupiterDialogForm;
  vrEnviroment : TJupiterEnviroment;
  vrStr : TStringList;
begin
  vrStr := TStringList.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  vrDialog := TJupiterDialogForm.Create;
  try
    vrStr.Clear;
    vrStr.LoadFromFile(vrEnviroment.FullPath(Self.Params.VariableById('chartFile').Value));

    vrDialog.Title := 'Novo gráfico';
    vrDialog.Hint  := 'Escolha o tipo de gráfico, ';

    vrDialog.Fields.AddField('DATASOURCE', 'Fonte de Dados', EmptyStr, True);
    vrDialog.Fields.AddField('FIELDX', 'Campo X', EmptyStr, True);
    vrDialog.Fields.AddField('FIELDY', 'Campo Y', EmptyStr, True);
    vrDialog.Fields.AddField('FIELDLABEL', 'Legenda', EmptyStr, True);
    vrDialog.Fields.AddField('FIELDCOLOR', 'Cor', EmptyStr, True);
    vrDialog.Fields.AddField('CHARTTYPE', 'Tipo de gráfico (Linha, Pizza, Barra, Polar, Area)', EmptyStr, True);

    if vrDialog.Show then
    begin
      JupiterShowInfoMessage('Novo gráfico cadastrado. Ao abrir o gerador de gráficos novamente o novo gráfico será gerado.');

      vrStr.Add(vrDialog.Fields.VariableById('DATASOURCE').Value + ';' +
                vrDialog.Fields.VariableById('FIELDX').Value + ';' +
                vrDialog.Fields.VariableById('FIELDY').Value + ';' +
                vrDialog.Fields.VariableById('FIELDLABEL').Value + ';' +
                vrDialog.Fields.VariableById('FIELDCOLOR').Value + ';' +
                vrDialog.Fields.VariableById('CHARTTYPE').Value + ';');

      vrStr.SaveToFile(vrEnviroment.FullPath(Self.Params.VariableById('chartFile').Value));
    end;

    Self.UpdateForm();
  finally
    FreeAndNil(vrEnviroment);
    FreeAndNil(vrStr);
    FreeAndNil(vrDialog);
  end;
end;

procedure TFChartViewer.Internal_SaveMenuItemClick(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
  vrFileName : String;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrFileName := vrEnviroment.SaveToFile('*.jpg');

    if vrFileName <> EmptyStr then
      chChart.SaveToBitmapFile(vrFileName);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFChartViewer.Internal_CreateCharts(prParams : TJupiterVariableList);
var
  vrData : TListChartSource;
  vrCSV  : TJupiterCSVDataProvider;
  vrEnviroment : TJupiterEnviroment;
  vrVez : Integer;

  vrPieSeries : TPieSeries;
  vrLineSeries : TLineSeries;
  vrBarSeries : TBarSeries;
  vrPolarSeries : TPolarSeries;
  vrAreaSeries : TAreaSeries;
  vrFirstLabel : String;

  vrColor : TChartColor;
begin
  vrColor := clTAColor;
  vrFirstLabel := prParams.VariableById('FIELDLABEL').Value;

  vrCSV        := TJupiterCSVDataProvider.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrCSV.Filename := vrEnviroment.FullPath(prParams.VariableById('DATASOURCE').Value);
    vrCSV.ProvideData;

    vrData := TListChartSource.Create(Self);

    for vrVez := 0 to vrCSV.Count - 1 do
      with vrCSV.GetRowByIndex(vrVez) do
      begin
        if not prParams.VariableById('FIELDCOLOR').Value.IsEmpty then
          vrColor := StringToColor(prParams.VariableById('FIELDCOLOR').Value);

        vrData.Add(Fields.VariableById(prParams.VariableById('FIELDX').Value).AsDouble,
                   Fields.VariableById(prParams.VariableById('FIELDY').Value).AsDouble,
                   prParams.VariableById('FIELDLABEL').Value,
                   vrColor);
      end;

    if AnsiUpperCase(prParams.VariableById('CHARTTYPE').Value) = 'PIZZA' then
    begin
      vrPieSeries := TPieSeries.Create(chChart);
      vrPieSeries.Source := vrData;
      vrPieSeries.AxisIndexX := 1;
      vrPieSeries.AxisIndexY := 0;

      vrPieSeries.Title := prParams.VariableById('DATASOURCE').Value;

      chChart.AddSeries(vrPieSeries);
    end;

    if AnsiUpperCase(prParams.VariableById('CHARTTYPE').Value) = 'LINHA' then
    begin
      vrLineSeries := TLineSeries.Create(chChart);
      vrLineSeries.Source := vrData;
      vrLineSeries.AxisIndexX := 1;
      vrLineSeries.AxisIndexY := 0;
      vrLineSeries.Title := vrFirstLabel;
      vrLineSeries.LinePen.Color := vrColor;

      chChart.AddSeries(vrLineSeries);
    end;

    if AnsiUpperCase(prParams.VariableById('CHARTTYPE').Value) = 'BARRA' then
    begin
      vrBarSeries := TBarSeries.Create(chChart);
      vrBarSeries.Source := vrData;
      vrBarSeries.AxisIndexX := 1;
      vrBarSeries.AxisIndexY := 0;
      vrBarSeries.Title := vrFirstLabel;

      chChart.AddSeries(vrBarSeries);
    end;

    if AnsiUpperCase(prParams.VariableById('CHARTTYPE').Value) = 'POLAR' then
    begin
      vrPolarSeries := TPolarSeries.Create(chChart);
      vrPolarSeries.Source := vrData;
      vrPolarSeries.AxisIndexX := 1;
      vrPolarSeries.AxisIndexY := 0;
      vrPolarSeries.Title := vrFirstLabel;

      chChart.AddSeries(vrPolarSeries);
    end;

    if AnsiUpperCase(prParams.VariableById('CHARTTYPE').Value) = 'AREA' then
    begin
      vrAreaSeries := TAreaSeries.Create(chChart);
      vrAreaSeries.Source := vrData;
      vrAreaSeries.AxisIndexX := 1;
      vrAreaSeries.AxisIndexY := 0;
      vrAreaSeries.Title := vrFirstLabel;

      chChart.AddSeries(vrAreaSeries);
    end;
  finally
    FreeAndNil(vrCSV);
  end;
end;

procedure TFChartViewer.Internal_UpdateCharts(prParams: TJupiterVariableList; prIndex: Integer);
var
  vrData : TListChartSource;
  vrCSV  : TJupiterCSVDataProvider;
  vrEnviroment : TJupiterEnviroment;
  vrVez : Integer;
  vrColor : TChartColor;
begin
  vrColor := clTAColor;

  vrCSV        := TJupiterCSVDataProvider.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrCSV.Filename := vrEnviroment.FullPath(prParams.VariableById('DATASOURCE').Value);
    vrCSV.ProvideData;

    if AnsiUpperCase(prParams.VariableById('CHARTTYPE').Value) = 'PIZZA' then
      vrData := TPieSeries(chChart.Series[prIndex]).Source as TListChartSource;

    if AnsiUpperCase(prParams.VariableById('CHARTTYPE').Value) = 'LINHA' then
      vrData := TLineSeries(chChart.Series[prIndex]).Source as TListChartSource;

    if AnsiUpperCase(prParams.VariableById('CHARTTYPE').Value) = 'BARRA' then
      vrData := TBarSeries(chChart.Series[prIndex]).Source as TListChartSource;

    if AnsiUpperCase(prParams.VariableById('CHARTTYPE').Value) = 'POLAR' then
      vrData := TPolarSeries(chChart.Series[prIndex]).Source as TListChartSource;

    if AnsiUpperCase(prParams.VariableById('CHARTTYPE').Value) = 'AREA' then
      vrData := TAreaSeries(chChart.Series[prIndex]).Source as TListChartSource;

    vrData.Clear;

    for vrVez := 0 to vrCSV.Count - 1 do
      with vrCSV.GetRowByIndex(vrVez) do
      begin
        if not prParams.VariableById('FIELDCOLOR').Value.IsEmpty then
          vrColor := StringToColor(prParams.VariableById('FIELDCOLOR').Value);

        vrData.Add(Fields.VariableById(prParams.VariableById('FIELDX').Value).AsDouble,
                   Fields.VariableById(prParams.VariableById('FIELDY').Value).AsDouble,
                   prParams.VariableById('FIELDLABEL').Value,
                   vrColor);
      end;
  finally
    FreeAndNil(vrCSV);
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFChartViewer.Internal_HideShowGrid(Sender: TObject);
var
  vrVez : Integer;
begin
  for vrVez := 0 to chChart.AxisList.Count - 1 do
    chChart.AxisList[vrVez].Visible := not chChart.AxisList[vrVez].Visible;
end;

procedure TFChartViewer.PrepareForm;
begin
  inherited PrepareForm;

  tmrUpdateAutoComponents.Enabled := Self.Showing and Self.Params.Exists('UpdateTimerIntervalTime');
end;

procedure TFChartViewer.UpdateForm(prUpdateDatasets: Boolean;
  prUpdateComponentes: Boolean; prUpdateCalcs: Boolean);
begin
  tmrUpdateAutoComponents.Enabled := False;

  inherited UpdateForm(prUpdateDatasets, prUpdateComponentes, prUpdateCalcs);

  tmrUpdateAutoComponents.Enabled := Self.Showing and Self.Params.Exists('UpdateTimerIntervalTime');
end;

end.

