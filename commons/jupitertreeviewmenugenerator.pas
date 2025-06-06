unit jupitertreeviewmenugenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, jupiterDatabaseWizard, JupiterConsts,
  JupiterApp, JupiterVariable, JupiterRoute, jupiterStringUtils, SQLDB,
  ComCtrls, jupiterDesktopApp;

type

  // R1.ID, R1.TITLE, R1.ROUTE, R1.ICON

  { TJupiterTreeViewMenuGenerator }

  TJupiterTreeViewMenuGenerator = class(TJupiterDatabaseWizard)
  private
    FTreeView : TTreeView;
    FOnClick  : TNotifyEvent;

    procedure Internal_RenderRoute(prOwner : TTreeNode; prPrefix : String);
    procedure Internal_CheckRender(prOwner : TTreeNode; prPrefix : String; prRouteData : TJupiterRouteData);
    function Internal_GetLevel(prRoute : String) : Integer;

    procedure Internal_OnClick(Sender: TObject);
    procedure Internal_OnKeyPress(Sender: TObject; var Key: char);
  published
    property TreeView : TTreeView    read FTreeView write FTreeView;
    property OnClick  : TNotifyEvent read FOnClick  write FOnClick;
  public
    procedure Render;

    procedure DoClick(Sender : TObject);
  end;

implementation

{ TJupiterTreeViewMenuGenerator }

procedure TJupiterTreeViewMenuGenerator.Internal_RenderRoute(prOwner: TTreeNode; prPrefix: String);
var
  vrQry      : TSQLQuery;
  vrVez      : Integer;
  vrData     : TJupiterRouteData;
begin
  vrQry := Self.NewQuery;
  try
    vrQry.SQL.Add(' SELECT R1.ID, R1.TITLE, R1.ROUTE, R1.ICON FROM ROUTES R1 WHERE ROUTE LIKE :PRROUTE ORDER BY COALESCE(R1.ZINDEX, 0) ');
    vrQry.ParamByName('PRROUTE').AsString := prPrefix + '%';
    vrQry.Open;
    vrQry.First;

    while not vrQry.EOF do
    begin
      vrData := TJupiterRouteData.Create;
      try
        vrData.Icon  := NULL_KEY;
        vrData.ID    := vrQry.FieldByName('ID').AsInteger;
        vrData.Title := vrQry.FieldByName('TITLE').AsString;
        vrData.Route := vrQry.FieldByName('ROUTE').AsString;

        if not vrQry.FieldByName('ICON').IsNull then
          vrData.Icon  := vrQry.FieldByName('ICON').AsInteger;

        Self.Internal_CheckRender(prOwner, prPrefix, vrData);
      finally
        FreeAndNil(vrData);
      end;

      vrQry.Next;
    end;

    for vrVez := 0 to TJupiterDesktopApp(vrJupiterApp).DynamicRouteList.Count - 1 do
    begin
      if not jupiterStringUtilsIsSameRootRoute(prPrefix, TJupiterVariableList(TJupiterDesktopApp(vrJupiterApp).DynamicRouteList.GetAtIndex(vrVez)).VariableById('Route').Value) then
        Continue;

      with TJupiterVariableList(TJupiterDesktopApp(vrJupiterApp).DynamicRouteList.GetAtIndex(vrVez)) do
      begin
        vrData          := TJupiterRouteData.Create;
        vrData.ID       := NULL_KEY;
        vrData.Title    := VariableById('Title').Value;
        vrData.Route    := VariableById('Route').Value;
        vrData.Icon     := VariableById('Icon').AsInteger;
        vrData.Shortcut := VariableById('Shortcut').Value;
        vrData.Params   := VariableById('Params').Value;
        vrData.Destiny  := VariableById('Destiny').AsInteger;
        vrData.ZIndex   := VariableById('ZIndex').AsInteger;

        Self.Internal_CheckRender(prOwner, prPrefix, vrData);
      end;
    end;
  finally
    vrQry.Close;
    FreeAndNil(vrQry);
  end;
end;

procedure TJupiterTreeViewMenuGenerator.Internal_CheckRender(prOwner: TTreeNode; prPrefix: String; prRouteData: TJupiterRouteData);
var
  vrNodeItem : TTreeNode;
begin
  if prRouteData.Route = prPrefix then
    Exit;

  if Self.Internal_GetLevel(StringReplace(prRouteData.Route, prPrefix, EmptyStr, [rfIgnoreCase, rfReplaceAll])) > 1 then
    Exit;

  if Assigned(prOwner) then
    vrNodeItem := Self.TreeView.Items.AddChild(prOwner, prRouteData.Title)
  else
    vrNodeItem := Self.TreeView.Items.Add(nil, prRouteData.Title);

  if prRouteData.ID = NULL_KEY then
    vrNodeItem.Data := prRouteData
  else
    vrNodeItem.Data := TJupiterDatabaseReference.Create('ROUTES', prRouteData.ID);

  if prRouteData.Icon <> NULL_KEY then
  begin
    vrNodeItem.ImageIndex := prRouteData.Icon;
    vrNodeItem.SelectedIndex := prRouteData.Icon;
  end;

  Self.Internal_RenderRoute(vrNodeItem, prRouteData.Route);
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
  vrDestiny : Integer;
  vrParam : String;
  vrRouteData : TJupiterRouteData;
begin
  if not Assigned(Sender) then
    Exit;

  if not (Sender is TTreeView) then
    Exit;

  if not Assigned(TTreeView(Sender).Selected) then
    Exit;

  if not Assigned(TTreeView(Sender).Selected.Data) then
    Exit;

  if TJupiterObject(TTreeView(Sender).Selected.Data) is TJupiterDatabaseReference then
  begin
    vrReference := TJupiterDatabaseReference(TTreeView(Sender).Selected.Data);

    vrWizard := vrJupiterApp.NewWizard;
    try
      if not vrWizard.Exists('ROUTES', Format(' ID = %0:d AND DESTINY IS NOT NULL ', [vrReference.ID])) then
        Exit;

      vrParam := EmptyStr;

      vrDestiny := vrWizard.GetField('ROUTES', 'DESTINY', ' ID = ' + IntToStr(vrReference.ID));

      if vrWizard.GetField('ROUTES', 'PARAMS', ' ID = ' + IntToStr(vrReference.ID)) <> Null then
        vrParam := vrWizard.GetField('ROUTES', 'PARAMS', ' ID = ' + IntToStr(vrReference.ID));

      vrJupiterApp.RunMacro(vrDestiny, CreateVariableListOfParam(vrParam));
    finally
      FreeAndNil(vrWizard);
    end;
  end;

  if TJupiterObject(TTreeView(Sender).Selected.Data) is TJupiterRouteData then
  begin
    vrRouteData := TJupiterRouteData(TTreeView(Sender).Selected.Data);

    if vrRouteData.Params <> '' then
      vrJupiterApp.RunMacro(vrRouteData.Destiny, CreateVariableListOfParam(vrRouteData.Params))
    else
      vrJupiterApp.RunMacro(vrRouteData.Destiny, TJupiterVariableList.Create);
  end;
end;

procedure TJupiterTreeViewMenuGenerator.Internal_OnKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    Self.Internal_OnClick(Sender);
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

procedure TJupiterTreeViewMenuGenerator.DoClick(Sender: TObject);
begin
  Self.Internal_OnClick(Sender);
end;

end.

