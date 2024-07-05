unit jupiterMainMenuGenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, jupiterDatabaseWizard, JupiterConsts,
  Menus, SQLDB;

type

  { TJupiterMainMenuGenerator }

  TJupiterMainMenuGenerator = class(TJupiterDatabaseWizard)
  private
    FMainMenu : TMainMenu;
    FOnClick  : TNotifyEvent;

    procedure Internal_RenderRoute(prOwner : TMenuItem; prPrefix : String);
    function Internal_GetLevel(prRoute : String) : Integer;

    procedure Internal_OnClick(Sender: TObject);
  published
    property MainMenu : TMainMenu    read FMainMenu write FMainMenu;
    property OnClick  : TNotifyEvent read FOnClick  write FOnClick;
  public
    procedure Render;
  end;

implementation

uses JupiterApp;

{ TJupiterMainMenuGenerator }

procedure TJupiterMainMenuGenerator.Internal_RenderRoute(prOwner: TMenuItem; prPrefix: String);
var
  vrQry      : TSQLQuery;
  vrMenuItem : TMenuItem;
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
        vrMenuItem := TMenuItem.Create(prOwner)
      else
        vrMenuItem := TMenuItem.Create(Self.MainMenu);

      vrMenuItem.Caption := vrQry.FieldByName('TITLE').AsString;
      vrMenuItem.Tag     := vrQry.FieldByName('ID').AsInteger;

      if ((not vrQry.FieldByName('ICON').IsNull) and (vrQry.FieldByName('ICON').AsInteger <> NULL_KEY)) then
        vrMenuItem.ImageIndex := vrQry.FieldByName('ICON').AsInteger;

      if Assigned(Self.OnClick) then
        vrMenuItem.OnClick := Self.OnClick
      else
        vrMenuItem.OnClick := @Internal_OnClick;

      if Assigned(prOwner) then
        prOwner.Add(vrMenuItem)
      else
        Self.MainMenu.Items.Add(vrMenuItem);

      Self.Internal_RenderRoute(vrMenuItem, vrQry.FieldByName('ROUTE').AsString);

      vrQry.Next;
    end;
  finally
    vrQry.Close;
    FreeAndNil(vrQry);
  end;
end;

procedure TJupiterMainMenuGenerator.Render;
begin
  Self.Internal_RenderRoute(nil, '/menu/');
end;

function TJupiterMainMenuGenerator.Internal_GetLevel(prRoute: String): Integer;
var
  vrVez : Integer;
begin
  Result := 0;

  for vrVez := 1 to Length(prRoute) do
    if prRoute[vrVez] = '/' then
      Result := Result + 1;
end;

procedure TJupiterMainMenuGenerator.Internal_OnClick(Sender: TObject);
var
  vrWizard : TJupiterDatabaseWizard;
begin
  if not Assigned(Sender) then
    Exit;

  if not (Sender is TMenuItem) then
    Exit;

  vrWizard := vrJupiterApp.NewWizard;
  try
    if not vrWizard.Exists('ROUTES', Format(' ID = %0:d AND DESTINY IS NOT NULL ', [TMenuItem(Sender).Tag])) then
      Exit;

    vrJupiterApp.RunMacro(vrWizard.GetField('ROUTES', 'DESTINY', ' ID = ' + IntToStr(TMenuItem(Sender).Tag)));
  finally
    FreeAndNil(vrWizard);
  end;
end;

end.

