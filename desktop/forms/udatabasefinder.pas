unit udatabasefinder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  uJupiterForm, jupiterutilspas, jupiterformutils, JupiterConsts, JupiterObject,
  jupiterDatabaseWizard, JupiterApp, jupiterStringUtils, uJupiterDatabaseScript,
  jupiterformcomponenttils, jupiterDesktopApp, SQLDB, uJupiterDesktopAppScript;

type

  { TFDatabaseFinder }

  TFDatabaseFinder = class(TFJupiterForm)
    pnForm: TPanel;
    sbBody: TScrollBox;
    Splitter1: TSplitter;
    procedure edSearchChange(Sender: TObject);
    procedure edSearchKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FClickItem : Boolean;
    FForm : TForm;
    FReferenceList : TJupiterObjectList;

    FCurrentLine : Integer;

    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_PrepareForm; override;

    procedure Internal_LinkClick(Sender: TObject);

    procedure Internal_DoSearch(prSearchTerm : String);

    function Internal_SearchInTable(prTablename, prSearchTerm : String) : Integer;

    procedure Internal_RenderRecord(prReference : TJupiterDatabaseReference);
  published
    property ClickItem : Boolean read FClickItem write FClickItem;
  public
    procedure AddForm(prForm : TForm);
  end;

var
  FDatabaseFinder: TFDatabaseFinder;

implementation

{$R *.lfm}

{ TFDatabaseFinder }

procedure TFDatabaseFinder.edSearchChange(Sender: TObject);
begin

end;

procedure TFDatabaseFinder.edSearchKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Self.UpdateForm();

    Key := #0;
  end;
end;

procedure TFDatabaseFinder.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FReferenceList := TJupiterObjectList.Create;
  Self.FReferenceList.ClearListItens;
end;

procedure TFDatabaseFinder.FormDestroy(Sender: TObject);
begin
  Self.FReferenceList.Free;

  if Assigned(FForm) then
    FForm.Destroy;

  inherited;
end;

procedure TFDatabaseFinder.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if miLookColumn.Checked then
    sbBody.Width := PercentOfScreen(Self.Width, Self.PercentDivisor);
end;

procedure TFDatabaseFinder.Internal_UpdateDatasets;
begin
  inherited Internal_UpdateDatasets;

  Self.FReferenceList.ClearListItens;

  Self.Internal_DoSearch(edSearch.Text);
end;

procedure TFDatabaseFinder.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  if Params.Exists('params') then
    edSearch.Text := Params.VariableById('params').Value;

  Self.FClickItem := False;

  Self.ShowSearchBar := True;
end;

procedure TFDatabaseFinder.Internal_LinkClick(Sender: TObject);
begin
  if not (Sender is TLabel) then
    Exit;

  Self.ClickItem := True;

  with TJupiterDatabaseReference(Self.FReferenceList.GetAtIndex(TLabel(Sender).Tag))  do
    JupiterAppDesktopOpenFormFromTableId(TableName, ID);
end;

procedure TFDatabaseFinder.Internal_DoSearch(prSearchTerm: String);
var
  vrCount       : Integer;
  vrQry         : TSQLQuery;
begin
  RemoveChildren(sbBody);

  Self.FCurrentLine := FORM_MARGIN_TOP * 2;

  if Trim(prSearchTerm) = EmptyStr then
  begin
    JupiterComponentsNewLabel('Informe um termo de pesquisa e pressione Enter', TJupiterPosition.Create(FORM_MARGIN_TOP * 2, FORM_MARGIN_LEFT), sbBody);

    Exit;
  end;

  vrQry := vrJupiterApp.NewWizard.NewQuery;
  try
    vrQry.SQL.Add(' SELECT DISTINCT TABLENAME FROM SEARCH_FIELD ORDER BY 1 ');
    vrQry.Open;
    vrQry.First;

    while not vrQry.EOF do
    begin
      vrCount := vrCount + Self.Internal_SearchInTable(vrQry.Fields[0].AsString, prSearchTerm);

      vrQry.Next;
    end;
  finally
    if vrCount = 0 then
      JupiterComponentsNewLabel('Nenhum registro encontrado.', TJupiterPosition.Create(Self.FCurrentLine, FORM_MARGIN_LEFT), sbBody);

    FreeAndNil(vrQry);
  end;
end;

function TFDatabaseFinder.Internal_SearchInTable(prTablename, prSearchTerm: String): Integer;
var
  vrQryField  : TSQLQuery;
  vrQrySearch : TSQLQuery;
  vrFirst     : Boolean;
  vrObj       : TJupiterComponentReference;
begin
  Result := 0;

  vrFirst := True;

  vrQryField  := vrJupiterApp.NewWizard.NewQuery;
  vrQrySearch := vrJupiterApp.NewWizard.NewQuery;
  try
    vrQryField.SQL.Add(' SELECT FIELDNAME FROM SEARCH_FIELD WHERE TABLENAME = :PRTABLE ');
    vrQryField.ParamByName('PRTABLE').AsString := prTablename;
    vrQryField.Open;

    vrQrySearch.SQL.Add(' SELECT ID FROM ' + prTablename + ' WHERE ');

    while not vrQryField.EOF do
    begin
      if not vrFirst then
        vrQrySearch.SQL.Add(' OR ');

      vrFirst := False;

      vrQrySearch.SQL.Add(' (UPPER(' + vrQryField.FieldByName('FIELDNAME').AsString + ') LIKE "%' + AnsiUpperCase(prSearchTerm) + '%") ');

      vrQryField.Next;
    end;

    vrQrySearch.Open;
    vrQrySearch.First;

    while not vrQrySearch.EOF do
    begin
      if Result = 0 then
      begin
        Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

        vrObj := JupiterComponentsNewLabel(JupiterStringUtilsNormalizeToPresent(prTablename), TJupiterPosition.Create(Self.FCurrentLine, FORM_MARGIN_LEFT), sbBody);

        TLabel(vrObj.Component).Font.Style := [fsBold];

        Self.FCurrentLine := vrObj.Bottom + FORM_MARGIN_BOTTOM;
      end;

      Result := Result + 1;

      Self.Internal_RenderRecord(TJupiterDatabaseReference.Create(prTablename, vrQrySearch.FieldByName('ID').AsInteger));

      vrQrySearch.Next;
    end;
  finally
    FreeAndNil(vrQrySearch);
    FreeAndNil(vrQryField);
  end;
end;

procedure TFDatabaseFinder.Internal_RenderRecord(prReference: TJupiterDatabaseReference);
var
  vrObj : TJupiterComponentReference;
begin
  vrObj := JupiterComponentsNewLink(JupiterDatabaseScript_ResolveRecordTable(prReference.TableName, prReference.ID), TJupiterPosition.Create(Self.FCurrentLine, FORM_MARGIN_LEFT * 2), sbBody);

  TLabel(vrObj.Component).Tag := Self.FReferenceList.Count;
  TLabel(vrObj.Component).OnClick := @Internal_LinkClick;

  Self.FReferenceList.Add(prReference);

  Self.FCurrentLine := vrObj.Bottom + FORM_MARGIN_BOTTOM;

  vrObj := JupiterComponentsNewLabel('Tabela: ' + JupiterStringUtilsNormalizeToPresent(prReference.TableName), TJupiterPosition.Create(Self.FCurrentLine, FORM_MARGIN_LEFT * 2), sbBody);

  Self.FCurrentLine := vrObj.Bottom + FORM_MARGIN_BOTTOM;

  vrObj := JupiterComponentsNewLabel('ID: ' + IntToStr(prReference.ID), TJupiterPosition.Create(Self.FCurrentLine, FORM_MARGIN_LEFT * 2), sbBody);

  Self.FCurrentLine := vrObj.Bottom + FORM_MARGIN_BOTTOM_TONEXT;
end;

procedure TFDatabaseFinder.AddForm(prForm: TForm);
begin
  if Assigned(FForm) then
    FForm.Destroy;

  Self.FClickItem := False;

  FForm := prForm;

  Self.FForm.Parent      := pnForm;
  Self.FForm.Left        := 0;
  Self.FForm.Top         := 0;
  Self.FForm.WindowState := wsMaximized;
  Self.FForm.BorderStyle := bsNone;

  Self.FForm.Show;
end;

end.

