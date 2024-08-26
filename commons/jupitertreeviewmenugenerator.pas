unit jupitertreeviewmenugenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, jupiterDatabaseWizard, JupiterConsts,
  JupiterApp, SQLDB, ComCtrls;

type

  { TJupiterTreeViewMenuGenerator }

  TJupiterTreeViewMenuGenerator = class(TJupiterDatabaseWizard)
  private
    FTreeView : TTreeView;
    FOnClick  : TNotifyEvent;

    procedure Internal_RenderRoute(prOwner : TTreeNode; prPrefix : String);
    function Internal_GetLevel(prRoute : String) : Integer;

    procedure Internal_OnClick(Sender: TObject);
  published
    property TreeView : TTreeView    read FTreeView write FTreeView;
    property OnClick  : TNotifyEvent read FOnClick  write FOnClick;
  public
    procedure Render;
  end;

implementation

{ TJupiterTreeViewMenuGenerator }

procedure TJupiterTreeViewMenuGenerator.Internal_RenderRoute(prOwner: TTreeNode; prPrefix: String);
var
  vrQry      : TSQLQuery;
  vrNodeItem : TTreeNode;
begin
  vrQry := Self.NewQuery;
  try
    vrQry.SQL.Add(' SELECT R1.ID, R1.TITLE, R1.ROUTE, R1.ICON FROM ROUTES R1 WHERE ROUTE LIKE :PRROUTE ORDER BY COALESCE(R1.ZINDEX, 0) ');
    vrQry.ParamByName('PRROUTE').AsString := prPrefix + '%';
    vrQry.Open;
    vrQry.First;

    while not vrQry.EOF do
    begin
      if vrQry.FieldByName('ROUTE').AsString = prPrefix then
      begin
        vrQry.Next;

        Continue;
      end;

      if Self.Internal_GetLevel(StringReplace(vrQry.FieldByName('ROUTE').AsString, prPrefix, EmptyStr, [rfIgnoreCase, rfReplaceAll])) > 1 then
      begin
        vrQry.Next;

        Continue;
      end;

      if Assigned(prOwner) then
        vrNodeItem := Self.TreeView.Items.AddChild(prOwner, vrQry.FieldByName('TITLE').AsString)
      else
        vrNodeItem := Self.TreeView.Items.Add(nil, vrQry.FieldByName('TITLE').AsString);

      vrNodeItem.Data := TJupiterDatabaseReference.Create('ROUTES', vrQry.FieldByName('ID').AsInteger);

      if ((not vrQry.FieldByName('ICON').IsNull) and (vrQry.FieldByName('ICON').AsInteger <> NULL_KEY)) then
      begin
        vrNodeItem.ImageIndex := vrQry.FieldByName('ICON').AsInteger;
        vrNodeItem.SelectedIndex := vrQry.FieldByName('ICON').AsInteger;
      end;

      Self.Internal_RenderRoute(vrNodeItem, vrQry.FieldByName('ROUTE').AsString);

      vrQry.Next;
    end;
  finally
    vrQry.Close;
    FreeAndNil(vrQry);
  end;
end;

function TJupiterTreeViewMenuGenerator.Internal_GetLevel(prRoute: String): Integer;
var
  vrVez : Integer;
begin
  Result := 0;

  for vrVez := 1 to Length(prRoute) do
    if prRoute[vrVez] = '/' then
      Result := Result + 1;
end;

procedure TJupiterTreeViewMenuGenerator.Internal_OnClick(Sender: TObject);
var
  vrWizard : TJupiterDatabaseWizard;
  vrReference : TJupiterDatabaseReference;
begin
  if not Assigned(Sender) then
    Exit;

  if not (Sender is TTreeView) then
    Exit;

  if not Assigned(TTreeView(Sender).Selected) then
    Exit;

  if not Assigned(TTreeView(Sender).Selected.Data) then
    Exit;

  vrReference := TJupiterDatabaseReference(TTreeView(Sender).Selected.Data);

  vrWizard := vrJupiterApp.NewWizard;
  try
    if not vrWizard.Exists('ROUTES', Format(' ID = %0:d AND DESTINY IS NOT NULL ', [vrReference.ID])) then
      Exit;

    vrJupiterApp.RunMacro(vrWizard.GetField('ROUTES', 'DESTINY', ' ID = ' + IntToStr(vrReference.ID)));
  finally
    FreeAndNil(vrWizard);
  end;

end;

procedure TJupiterTreeViewMenuGenerator.Render;
begin
  try
    Self.Internal_RenderRoute(nil, '/main/');

    Self.TreeView.OnDblClick := @Internal_OnClick;
  finally
    Self.TreeView.FullExpand;
  end;
end;

end.

