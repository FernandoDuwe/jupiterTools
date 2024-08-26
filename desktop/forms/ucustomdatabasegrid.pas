unit ucustomdatabasegrid;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, uJupiterForm,
  jupiterDatabaseWizard, JupiterApp, jupiterStringUtils, jupiterformutils,
  JupiterConsts, uJupiterAction, DB, SQLDB;

type

  { TFCustomDatabaseGrid }

  TFCustomDatabaseGrid = class(TFJupiterForm)
    InternalDataSource: TDataSource;
    dbMainGrid: TDBGrid;
    InternalQuery: TSQLQuery;
    procedure dbMainGridDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FReference : TJupiterDatabaseReference;

    procedure Internal_UpdateComponents; override;
    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateDatasets; override;

    procedure Internal_OnNew(Sender: TObject);
  public
    procedure FromReference(prReference : TJupiterDatabaseReference);
  end;

var
  FCustomDatabaseGrid: TFCustomDatabaseGrid;

implementation

uses uJupiterDesktopAppScript;

{$R *.lfm}

{ TFCustomDatabaseGrid }

procedure TFCustomDatabaseGrid.FormCreate(Sender: TObject);
var
  vrWizard : TJupiterDatabaseWizard;
begin
  inherited;

  vrWizard := vrJupiterApp.NewWizard;
  try
    InternalQuery.DataBase    := vrWizard.Connection;
    InternalQuery.Transaction := vrWizard.Transaction;
  finally
    FreeAndNil(vrWizard);
  end;
end;

procedure TFCustomDatabaseGrid.dbMainGridDblClick(Sender: TObject);
begin
  if InternalQuery.EOF then
    Exit;

  JupiterAppDesktopOpenFormFromTableId(Self.FReference.TableName, InternalQuery.FieldByName('ID').AsInteger);
end;

procedure TFCustomDatabaseGrid.Internal_UpdateComponents;
var
  vrVez : Integer;
begin
  inherited Internal_UpdateComponents;

  for vrVez := 0 to dbMainGrid.Columns.Count - 1 do
  begin
    dbMainGrid.Columns[vrVez].Visible := dbMainGrid.Columns[vrVez].FieldName <> 'ID';
    dbMainGrid.Columns[vrVez].Title.Caption := JupiterStringUtilsNormalizeToPresent(dbMainGrid.Columns[vrVez].FieldName);
    dbMainGrid.Columns[vrVez].Width := PercentOfScreen(dbMainGrid.Width, 40);
  end;
end;

procedure TFCustomDatabaseGrid.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.ShowSearchBar := True;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Novo', 'Clique aqui para criar um novo registro', ICON_NEW, @Internal_OnNew));
end;

procedure TFCustomDatabaseGrid.Internal_UpdateDatasets;
begin
  inherited Internal_UpdateDatasets;

  InternalQuery.Close;
  InternalQuery.Open;
end;

procedure TFCustomDatabaseGrid.Internal_OnNew(Sender: TObject);
begin
  JupiterAppDesktopOpenFormFromTableId(Self.FReference.TableName, NULL_KEY);
end;

procedure TFCustomDatabaseGrid.FromReference(prReference: TJupiterDatabaseReference);
var
  vrWizard : TJupiterDatabaseWizard;
begin
  inherited;

  Self.FReference := prReference;

  Self.Caption := JupiterStringUtilsNormalizeToPresent(prReference.TableName);

  vrWizard := vrJupiterApp.NewWizard;
  try
    InternalQuery.Close;
    InternalQuery.SQL.Clear;
    InternalQuery.SQL.AddStrings(vrWizard.NewQueryFromReference(prReference).SQL);
    InternalQuery.Open;
    InternalQuery.First;
  finally
    FreeAndNil(vrWizard);
  end;
end;

end.

