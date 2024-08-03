unit uGenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  DBGrids, uJupiterForm, JupiterApp, jupiterformutils, uJupiterDesktopAppScript,
  SQLDB, DB;

type

  { TFGenerator }

  TFGenerator = class(TFJupiterForm)
    dsMacro: TDataSource;
    dsRoutes: TDataSource;
    dbGridRoutes: TDBGrid;
    dbGridMacros: TDBGrid;
    mmDetails: TMemo;
    pcOptions: TPageControl;
    qryRoutes: TSQLQuery;
    qryMacro: TSQLQuery;
    tsMacros: TTabSheet;
    tsRoutes: TTabSheet;
    tsGenerator: TTabSheet;
    procedure dbGridMacrosDblClick(Sender: TObject);
    procedure dbGridRoutesDblClick(Sender: TObject);
  private
    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_PrepareForm; override;
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

procedure TFGenerator.Internal_UpdateComponents;
begin
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
end;

procedure TFGenerator.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.ShowSearchBar := True;

  mmDetails.Lines.Clear;
  mmDetails.Lines.Add('Aplicação');
  mmDetails.Lines.Add(vrJupiterApp.AppID + ' - ' + vrJupiterApp.AppName);

  with vrJupiterApp.NewWizard do
  begin
    qryRoutes.DataBase    := Connection;
    qryRoutes.Transaction := Transaction;

    qryMacro.DataBase := Connection;
    qryRoutes.Transaction := Transaction;
  end;
end;

end.

