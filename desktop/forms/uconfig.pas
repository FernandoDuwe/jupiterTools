unit uConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBCtrls, ExtCtrls,
  DBGrids, Menus, ComCtrls, uJupiterForm, jupiterformutils, JupiterObject,
  JupiterConsts, jupiterDatabaseWizard, JupiterApp, DB, SQLDB;

type

  { TFConfig }

  TFConfig = class(TFJupiterForm)
    dsConfig: TDataSource;
    gbVariables: TDBGrid;
    spSeparator: TSplitter;
    qryConfig: TSQLQuery;
    tvFilter: TTreeView;
    procedure tvFilterSelectionChanged(Sender: TObject);
  private
    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_PrepareForm; override;

    function Internal_GetFilter : String;
  public

  end;

var
  FConfig: TFConfig;

implementation


{$R *.lfm}

{ TFConfig }

procedure TFConfig.tvFilterSelectionChanged(Sender: TObject);
begin
  Self.UpdateForm();
end;

procedure TFConfig.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  tvFilter.Width := PercentOfScreen(Self.Width, 30);

  if qryConfig.FieldCount > 0 then
    qryConfig.Fields[0].Visible := False;

  if qryConfig.FieldCount > 1 then
  begin
    qryConfig.Fields[1].DisplayLabel := 'Nome';
    qryConfig.Fields[1].DisplayWidth := PercentOfScreen(gbVariables.Width, 50);
  end;

  if qryConfig.FieldCount > 2 then
  begin
    qryConfig.Fields[2].DisplayLabel := 'Valor';
    qryConfig.Fields[2].DisplayWidth := PercentOfScreen(gbVariables.Width, 50);
  end;

  if qryConfig.FieldCount > 3 then
    qryConfig.Fields[3].Visible := False;

  if gbVariables.Columns.Count > 1 then
    gbVariables.Columns[1].Width := PercentOfScreen(gbVariables.Width, 50);

  if gbVariables.Columns.Count > 2 then
    gbVariables.Columns[2].Width := PercentOfScreen(gbVariables.Width, 50);
end;

procedure TFConfig.Internal_UpdateDatasets;
begin
  inherited Internal_UpdateDatasets;

  qryConfig.Close;
  qryConfig.SQL.Clear;
  qryConfig.SQL.Add(' SELECT V1.ID, V1.NAME, V1.VALUE, V1.MODULE FROM VARIABLES V1 ');

  if Self.Internal_GetFilter <> EmptyStr then
    qryConfig.SQL.Add(' WHERE ' + Self.Internal_GetFilter);

  qryConfig.SQL.Add(' ORDER BY 2 ');
  qryConfig.Open;
  qryConfig.First;
end;

procedure TFConfig.Internal_PrepareForm;
var
  vrNote  : TTreeNode;
  vrChild : TTreeNode;
  vrQry   : TSQLQuery;
begin
  inherited Internal_PrepareForm;

  tvFilter.Items.Clear;

  vrNote := tvFilter.Items.Add(nil, 'Todos');
  vrNote.Data := TJupiterDatabaseReference.Create('MODULES', NULL_KEY);

  vrNote := tvFilter.Items.Add(nil, 'Módulos');

  with vrJupiterApp.NewWizard do
  begin
    qryConfig.DataBase    := Connection;
    qryConfig.Transaction := Transaction;

    vrQry := NewQuery;
    try
      vrQry.SQL.Add(' SELECT M1.ID, M1.NAME FROM MODULES M1 ');
      vrQry.Open;

      while not vrQry.EOF do
      begin
        vrChild := tvFilter.Items.AddChild(vrNote, vrQry.FieldByName('NAME').AsString);
        vrChild.Data := TJupiterDatabaseReference.Create('MODULES', vrQry.FieldByName('ID').AsInteger);

        vrQry.Next;
      end;

      tvFilter.FullExpand;
    finally
      vrQry.Close;
      FreeAndNil(vrQry);

      FreeAndNil(vrQry);
    end;
  end;
end;

function TFConfig.Internal_GetFilter: String;
begin
  Result := EmptyStr;

  if not Assigned(tvFilter.Selected) then
    Exit;

  if not Assigned(tvFilter.Selected.Data) then
    Exit;

  if TJupiterDatabaseReference(tvFilter.Selected.Data).ID = NULL_KEY then
    Exit;

  Result := Format(' ID = %0:d ', [TJupiterDatabaseReference(tvFilter.Selected.Data).ID]);
end;

end.

