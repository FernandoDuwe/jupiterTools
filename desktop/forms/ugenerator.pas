unit uGenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  DBGrids, uJupiterForm, JupiterApp, jupiterformutils, JupiterConsts,
  JupiterModule, JupiterRoute, uJupiterDesktopAppScript, uJupiterAction,
  jupiterDesktopApp, SQLDB, DB;

type

  { TFGenerator }

  TFGenerator = class(TFJupiterForm)
    dbGridActions: TDBGrid;
    dsMacro: TDataSource;
    dsActions: TDataSource;
    dsRoutes: TDataSource;
    dbGridRoutes: TDBGrid;
    dbGridMacros: TDBGrid;
    mmDetails: TMemo;
    pcOptions: TPageControl;
    qryActions: TSQLQuery;
    qryRoutes: TSQLQuery;
    qryMacro: TSQLQuery;
    tsActions: TTabSheet;
    tsMacros: TTabSheet;
    tsRoutes: TTabSheet;
    tsGenerator: TTabSheet;
    procedure dbGridActionsDblClick(Sender: TObject);
    procedure dbGridMacrosDblClick(Sender: TObject);
    procedure dbGridRoutesDblClick(Sender: TObject);
  private
    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_PrepareForm; override;

    procedure Internal_OnNewRoute(Sender: TObject);
    procedure Internal_OnNewMacro(Sender: TObject);
    procedure Internal_OnNewAction(Sender: TObject);
  public

  end;

var
  FGenerator: TFGenerator;

implementation

{$R *.lfm}

{ TFGenerator }

procedure TFGenerator.dbGridRoutesDblClick(Sender: TObject);
begin
  if qryRoutes.EOF then
    Exit;

  JupiterAppDesktopOpenFormFromTableId('ROUTES', qryRoutes.FieldByName('ID').AsInteger);
end;

procedure TFGenerator.dbGridMacrosDblClick(Sender: TObject);
begin
  if qryMacro.EOF then
    Exit;

  JupiterAppDesktopOpenFormFromTableId('MACROS', qryMacro.FieldByName('ID').AsInteger);
end;

procedure TFGenerator.dbGridActionsDblClick(Sender: TObject);
begin
  if qryActions.EOF then
    Exit;

  JupiterAppDesktopOpenFormFromTableId('ACTIONS', qryActions.FieldByName('ID').AsInteger);
end;

procedure TFGenerator.Internal_UpdateComponents;
begin
  Self.Hint := 'Você pode alterar todo o sistema a partir do módulo Generator. Crie novas rotas, altere as funções do sistema e adicione novas tabelas.';

  inherited Internal_UpdateComponents;

  if qryRoutes.FieldCount > 0 then
    qryRoutes.Fields[0].Visible := False;

  if qryRoutes.FieldCount > 1 then
  begin
    qryRoutes.Fields[1].DisplayLabel := 'Título';
    qryRoutes.Fields[1].DisplayWidth := PercentOfScreen(Self.Width, 50);
  end;

  if qryRoutes.FieldCount >= 2 then
  begin
    qryRoutes.Fields[2].DisplayLabel := 'Rota';
    qryRoutes.Fields[2].DisplayWidth := PercentOfScreen(Self.Width, 50);
  end;

  if dbGridRoutes.Columns.Count > 0 then
    dbGridRoutes.Columns[0].Width := PercentOfScreen(Self.Width, 50);

  if dbGridRoutes.Columns.Count >= 1 then
    dbGridRoutes.Columns[1].Width := PercentOfScreen(Self.Width, 50);

  if qryMacro.FieldCount > 0 then
    qryMacro.Fields[0].Visible := False;

  if qryMacro.FieldCount > 1 then
  begin
    qryMacro.Fields[1].DisplayLabel := 'Título';
    qryMacro.Fields[1].DisplayWidth := PercentOfScreen(Self.Width, 50);
  end;

  if qryMacro.FieldCount >= 2 then
  begin
    qryMacro.Fields[2].DisplayLabel := 'Macro ID';
    qryMacro.Fields[2].DisplayWidth := PercentOfScreen(Self.Width, 50);
  end;

  if dbGridMacros.Columns.Count > 0 then
    dbGridMacros.Columns[0].Width := PercentOfScreen(Self.Width, 50);

  if dbGridMacros.Columns.Count >= 1 then
    dbGridMacros.Columns[1].Width := PercentOfScreen(Self.Width, 50);

  if qryActions.FieldCount > 0 then
    qryActions.Fields[0].Visible := False;

  if qryActions.FieldCount > 1 then
  begin
    qryActions.Fields[1].DisplayLabel := 'Título';
    qryActions.Fields[1].DisplayWidth := PercentOfScreen(Self.Width, 50);
  end;

  if qryActions.FieldCount > 2 then
  begin
    qryActions.Fields[2].DisplayLabel := 'Nome';
    qryActions.Fields[2].DisplayWidth := PercentOfScreen(Self.Width, 50);
  end;

  if dbGridActions.Columns.Count > 0 then
    dbGridActions.Columns[0].Width := PercentOfScreen(Self.Width, 50);


    if dbGridActions.Columns.Count > 1 then
      dbGridActions.Columns[1].Width := PercentOfScreen(Self.Width, 50);
end;

procedure TFGenerator.Internal_UpdateDatasets;
begin
  inherited Internal_UpdateDatasets;

  qryRoutes.Close;
  qryRoutes.SQL.Clear;
  qryRoutes.SQL.Add(' SELECT R1.ID, R1.TITLE, R1.ROUTE FROM ROUTES R1 ORDER BY 2, 3 ');
  qryRoutes.Open;
  qryRoutes.First;

  qryMacro.Close;
  qryMacro.SQL.Clear;
  qryMacro.SQL.Add(' SELECT M1.ID, M1.NAME, M1.MACROID FROM MACROS M1 ORDER BY 2, 3 ');
  qryMacro.Open;
  qryMacro.First;

  qryActions.Close;
  qryActions.SQL.Clear;
  qryActions.SQL.Add(' SELECT M1.ID, M1.TITLE, M1.NAME FROM ACTIONS M1 ORDER BY 2 ');
  qryActions.Open;
  qryActions.First;
end;

procedure TFGenerator.Internal_PrepareForm;
var
  vrVez : Integer;
  vrPrefix : String;
begin
  inherited Internal_PrepareForm;

  Self.ShowSearchBar := False;

  vrPrefix := '      ';

  Self.ActionGroup.AddAction(TJupiterAction.Create('Nova Rota', 'Clique aqui para criar uma nova rota', ICON_NEW, @Internal_OnNewRoute));
  Self.ActionGroup.AddAction(TJupiterAction.Create('Nova Macro', 'Clique aqui para criar uma nova macro', ICON_NEW, @Internal_OnNewMacro));
  Self.ActionGroup.AddAction(TJupiterAction.Create('Nova Ação', 'Clique aqui para criar uma nova ação', ICON_NEW, @Internal_OnNewAction));

  mmDetails.Lines.Clear;
  mmDetails.Lines.Add(EmptyStr);
  mmDetails.Lines.Add('Aplicação');
  mmDetails.Lines.Add(vrPrefix + vrJupiterApp.AppID + ' - ' + vrJupiterApp.AppName);
  mmDetails.Lines.Add(EmptyStr);
  mmDetails.Lines.Add('Módulos:');

  for vrVez := 0 to vrJupiterApp.ModulesList.Count - 1 do
    with vrJupiterApp.ModulesList.GetModuleByIndex(vrVez) do
      mmDetails.Lines.Add(vrPrefix + ModuleID + ' - ' + ModuleTitle);

  mmDetails.Lines.Add(EmptyStr);
  mmDetails.Lines.Add('Rotas:');

  for vrVez := 0 to TJupiterDesktopApp(vrJupiterApp).FormRoutes.Count - 1 do
    with TJupiterFormRoute(TJupiterDesktopApp(vrJupiterApp).FormRoutes.GetAtIndex(vrVez)) do
      mmDetails.Lines.Add(vrPrefix + FormClass.ClassName + ' - ' + DestinyPath);

  with vrJupiterApp.NewWizard do
  begin
    qryRoutes.DataBase    := Connection;
    qryRoutes.Transaction := Transaction;

    qryMacro.DataBase := Connection;
    qryRoutes.Transaction := Transaction;

    qryActions.DataBase    := Connection;
    qryActions.Transaction := Transaction;
  end;
end;

procedure TFGenerator.Internal_OnNewRoute(Sender: TObject);
begin
  JupiterAppDesktopOpenFormFromTableId('ROUTES', NULL_KEY);
end;

procedure TFGenerator.Internal_OnNewMacro(Sender: TObject);
begin
  JupiterAppDesktopOpenFormFromTableId('MACROS', NULL_KEY);
end;

procedure TFGenerator.Internal_OnNewAction(Sender: TObject);
begin
  JupiterAppDesktopOpenFormFromTableId('ACTIONS', NULL_KEY);
end;

end.

