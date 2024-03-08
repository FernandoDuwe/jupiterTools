unit JupiterAction;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterObject, JupiterRoute, JupiterRunnable, JupiterVariable,
  JupiterConsts, jupiterformutils, SysUtils, Forms, Buttons, Menus;

type

  { TJupiterAction }

  TJupiterAction = class(TJupiterObject)
  private
    FRunnable      : TJupiterRunnable;
    FLocation      : TJupiterRoute;
    FRoute         : TJupiterRoute;
    FVariableList  : TJupiterVariableList;
    FTitle         : String;
    FHint          : String;
    FIcon          : SmallInt;
    FType          : Integer;
    FOnClick       : TNotifyEvent;
    FOnClickScript : TStrings;

    FOnBeforeExecute : TJupiterAction;
    FOnAfterExecute  : TJupiterAction;

    FConfirmBeforeExecute : Boolean;

    procedure Internal_SetOnClickScript(Value : TStrings);
  published
    property Title      : String   read FTitle write FTitle;
    property Hint       : String   read FHint  write FHint;
    property Icon       : SmallInt read FIcon  write FIcon;
    property ActionType : Integer  read FType  write FType;

    property Runnable : TJupiterRunnable read FRunnable write FRunnable;
    property Location : TJupiterRoute    read FLocation write FLocation;
    property Route    : TJupiterRoute    read FRoute    write FRoute;

    property ConfirmBeforeExecute : Boolean read FConfirmBeforeExecute write FConfirmBeforeExecute;

    property OnAfterExecute  : TJupiterAction read FOnAfterExecute  write FOnAfterExecute;
    property OnBeforeExecute : TJupiterAction read FOnBeforeExecute write FOnBeforeExecute;
    property OnClick         : TNotifyEvent   read FOnClick         write FOnClick;
    property OnClickScript   : TStrings       read FOnClickScript   write Internal_SetOnClickScript;

    property VariableList : TJupiterVariableList read FVariableList write FVariableList;
  public
    procedure Execute;

    constructor Create(prTitle : String; prRunnable : TJupiterRunnable; prLocation : TJupiterRoute = nil);
    constructor Create(prTitle : String; prRoute   : TJupiterRoute; prLocation : TJupiterRoute = nil);
    constructor Create(prTitle : String; prOnClick : TNotifyEvent; prLocation : TJupiterRoute = nil);

    destructor Destroy; override;
  end;

  { TJupiterActionList }

  TJupiterActionList = class(TJupiterObjectList)
  private
    FPopupMenu : TPopupMenu;
    FCompactMode : Boolean;
  published
    property CompactMode : Boolean read FCompactMode write FCompactMode;

    property PopupMenu : TPopupMenu read FPopupMenu write FPopupMenu;
  protected
    procedure Internal_OnClick(Sender: TObject);

    function Internal_CreateButton(prAction : TJupiterAction; prIndex, prLeft : Integer; prOwner : TScrollBox) : TBitBtn;
  public
    procedure CopyFromList(prCopyList : TJupiterActionList; prOnBeforeExecute : TJupiterAction = nil);

    procedure BuildActions(prOwner : TScrollBox);
    procedure UpdateActions(prOwner : TScrollBox);

    function GetActionButton(prActionIndex : Integer; prOwner : TScrollBox) : TBitBtn;
    function GetMenuItem(prActionIndex : Integer) : TMenuItem;

    procedure DisableAction(prActionIndex : Integer; prOwner : TScrollBox);
    procedure EnableAction(prActionIndex : Integer; prOwner : TScrollBox);
    procedure EnableDisableAction(prActionIndex : Integer; prOwner : TScrollBox; prEnabled : Boolean);

    constructor Create; override;
  end;

implementation

uses LCLType, JupiterApp;

{ TJupiterActionList }

procedure TJupiterActionList.Internal_OnClick(Sender: TObject);
var
  vrAction : TJupiterAction;
begin
  if Sender is TBitBtn then
    vrAction := TJupiterAction(Self.GetAtIndex(TBitBtn(Sender).Tag))
  else
    vrAction := TJupiterAction(Self.GetAtIndex(TMenuItem(Sender).Tag));

  if Assigned(vrAction) then
    vrAction.Execute;
end;

function TJupiterActionList.Internal_CreateButton(prAction: TJupiterAction; prIndex, prLeft: Integer; prOwner: TScrollBox): TBitBtn;
var
  vrWidth : Integer;
  vrPopupMenu : TMenuItem;
  vrShortcutButton : String;
  vrShortCutCompact : String;
begin
  vrShortcutButton  := EmptyStr;
  vrShortCutCompact := EmptyStr;

  if (prIndex < 12) then
    vrShortcutButton := Format(' (F%0:d)', [prIndex + 1]);

  vrShortCutCompact := Format('F%0:d', [prIndex + 1]);

  Result            := TBitBtn.Create(prOwner);
  Result.Parent     := prOwner;
  Result.Top        := FORM_MARGIN_TOP;

  if not Self.CompactMode then
    Result.Caption := prAction.Title + vrShortcutButton
  else
    Result.Caption := vrShortCutCompact;

  Result.Hint       := prAction.Hint + vrShortcutButton;
  Result.ShowHint   := Trim(prAction.Hint) <> EmptyStr;
  Result.ImageIndex := prAction.Icon;
  Result.Left       := prLeft;

  if Assigned(vrJupiterApp) then
    Result.Font.Size  := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

  {$IFNDEF JUPITERCLI}
  if ((Assigned(vrJupiterApp)) and (Result.ImageIndex <> - 1)) then
    Result.Images := vrJupiterApp.MainIcons;
  {$ENDIF}

  Result.AutoSize := True;

  vrWidth := Result.Width + 40;

  vrWidth := 40 + GetTextWidth(Result.Caption, Result.Font) + Result.Glyph.Width;

  Result.AutoSize := False;

  if vrWidth < FORM_ACTION_MINWIDTH then
    Result.Width := FORM_ACTION_MINWIDTH
  else
    Result.Width := vrWidth;

  Result.Height  := prOwner.Height - (FORM_MARGIN_TOP + FORM_MARGIN_BOTTOM);

  if Self.CompactMode then
    Result.Width := FORM_ACTION_MINWIDTH_COMPACT;

  Result.Tag     := prIndex;
  Result.OnClick := @Self.Internal_OnClick;

  vrPopupMenu := TMenuItem.Create(Self.PopupMenu);
  vrPopupMenu.Caption    := prAction.Title + vrShortcutButton;
  vrPopupMenu.Hint       := prAction.Hint;
  vrPopupMenu.ImageIndex := prAction.Icon;
  vrPopupMenu.Tag        := prIndex;
  vrPopupMenu.OnClick    := @Self.Internal_OnClick;

  Self.PopupMenu.Items.Add(vrPopupMenu);
end;

procedure TJupiterActionList.CopyFromList(prCopyList: TJupiterActionList; prOnBeforeExecute : TJupiterAction = nil);
var
  vrVez           : Integer;
  vrAction        : TJupiterAction;
  vrCurrentAction : TJupiterAction;
  vrLocation      : TJupiterRoute;
begin
  for vrVez := 0 to prCopyList.Size - 1 do
  begin
    vrCurrentAction := TJupiterAction(prCopyList.GetAtIndex(vrVez));
    vrLocation      := nil;

    if Assigned(vrCurrentAction.Location) then
      vrLocation := TJupiterRoute.Create(vrCurrentAction.Location.Path);

    if Assigned(vrCurrentAction.Route) then
      vrAction := TJupiterAction.Create(vrCurrentAction.Title, TJupiterRoute.Create(vrCurrentAction.Route.Path), vrLocation);

    if Assigned(vrCurrentAction.Runnable) then
      vrAction := TJupiterAction.Create(vrCurrentAction.Title, TJupiterRunnable.Create(vrCurrentAction.Runnable.CommandLine), vrLocation);

    if Assigned(vrCurrentAction.OnClick) then
      vrAction := TJupiterAction.Create(vrCurrentAction.Title, vrCurrentAction.OnClick, vrLocation);

    vrAction.Icon := vrCurrentAction.Icon;
    vrAction.ConfirmBeforeExecute := vrCurrentAction.ConfirmBeforeExecute;
    vrAction.Hint := vrCurrentAction.Hint;
    vrAction.ActionType := vrCurrentAction.ActionType;

    if Assigned(vrCurrentAction.OnClickScript) then
      vrAction.OnClickScript.AddStrings(vrCurrentAction.OnClickScript);

    vrAction.OnBeforeExecute := prOnBeforeExecute;

    Self.Add(vrAction);
  end;
end;

procedure TJupiterActionList.BuildActions(prOwner: TScrollBox);
var
  vrLeft      : Integer;
  vrVez       : Integer;
  vrSpeed     : TBitBtn;
  vrPopupMenu : TMenuItem;
  vrRight     : Integer;
begin
  if Self.Size = 0 then
    Exit;

  vrLeft := FORM_MARGIN_LEFT;

  vrPopupMenu := TMenuItem.Create(Self.PopupMenu);
  vrPopupMenu.Caption := '-';

  Self.PopupMenu.Items.Add(vrPopupMenu);

  for vrVez := 0 to Self.Size - 1 do
  begin
    vrSpeed := Self.Internal_CreateButton(TJupiterAction(Self.GetAtIndex(vrVez)), vrVez, vrLeft, prOwner);

    vrLeft := vrSpeed.Left + vrSpeed.Width + FORM_MARGIN_LEFT;
  end;

  // Se o último botão estiver mais longe que o tamanho da tela, aumenta o scrollbox
  if vrLeft > prOwner.Width then
    prOwner.Height := prOwner.Height + 20;
end;

procedure TJupiterActionList.UpdateActions(prOwner: TScrollBox);
var
  vrLastButton : TBitBtn;
  vrLeft : Integer;
begin
  if Self.Count = 0 then
    Exit;

  vrLastButton := Self.GetActionButton(Self.Count - 1, prOwner);

  vrLeft :=  vrLastButton.Left + vrLastButton.Width + FORM_MARGIN_LEFT;

  // Se o último botão estiver mais longe que o tamanho da tela, aumenta o scrollbox
  if vrLeft > prOwner.Width then
    prOwner.Height := 80
  else
    prOwner.Height := 60;
end;

function TJupiterActionList.GetActionButton(prActionIndex: Integer; prOwner: TScrollBox): TBitBtn;
var
  vrVez : Integer;
begin
  Result := nil;

  for vrVez := 0 to prOwner.ControlCount - 1 do
  begin
    if not (prOwner.Controls[vrVez] is TBitBtn) then
      Continue;

    if TBitBtn(prOwner.Controls[vrVez]).Tag = prActionIndex then
      Result := TBitBtn(prOwner.Controls[vrVez]);
  end;
end;

function TJupiterActionList.GetMenuItem(prActionIndex: Integer): TMenuItem;
var
  vrVez : Integer;
begin
  Result := nil;

  for vrVez := 0 to Self.PopupMenu.Items.Count -1 do
  begin
    if Self.PopupMenu.Items[vrVez].Tag = prActionIndex then
      Result := Self.PopupMenu.Items[vrVez];
  end;
end;

procedure TJupiterActionList.DisableAction(prActionIndex: Integer; prOwner: TScrollBox);
begin
  Self.GetActionButton(prActionIndex, prOwner).Enabled := False;

  Self.GetMenuItem(prActionIndex).Enabled := False;
end;

procedure TJupiterActionList.EnableAction(prActionIndex: Integer; prOwner: TScrollBox);
begin
  Self.GetActionButton(prActionIndex, prOwner).Enabled := True;

  Self.GetMenuItem(prActionIndex).Enabled := True;
end;

procedure TJupiterActionList.EnableDisableAction(prActionIndex: Integer; prOwner: TScrollBox; prEnabled: Boolean);
begin
  Self.GetActionButton(prActionIndex, prOwner).Enabled := prEnabled;

  Self.GetMenuItem(prActionIndex).Enabled := prEnabled;
end;

constructor TJupiterActionList.Create;
begin
  inherited Create;

  Self.FCompactMode := False;
end;

{ TJupiterAction }

procedure TJupiterAction.Internal_SetOnClickScript(Value: TStrings);
begin
  Self.FOnClickScript.Clear;
  Self.FOnClickScript.AddStrings(Value);
end;

procedure TJupiterAction.Execute;
begin
  if Self.ConfirmBeforeExecute then
    if Application.MessageBox(PAnsiChar('Deseja realmente ' + Self.Title + '?'), PAnsiChar(Self.Title), MB_ICONQUESTION + MB_YESNO) = ID_NO then
      Exit;

  if Assigned(Self.OnBeforeExecute) then
    Self.OnBeforeExecute.Execute;

  if Assigned(Self.FOnClick) then
    Self.OnClick(Application.MainForm);

  if ((Assigned(Self.Runnable)) and (Self.Runnable.CommandLine <> EmptyStr)) then
    Self.Runnable.Execute;

  if Assigned(Self.Route) then
    vrJupiterApp.NavigateTo(Self.Route, not Application.MainForm.Showing);

  if (Self.OnClickScript.Text <> EmptyStr) then
    vrJupiterApp.RunScript(Self.OnClickScript);

  if Assigned(Self.OnAfterExecute) then
    Self.OnAfterExecute.Execute;
end;

constructor TJupiterAction.Create(prTitle : String; prRunnable: TJupiterRunnable; prLocation : TJupiterRoute);
begin
  Self.FTitle     := prTitle;
  Self.FRunnable  := prRunnable;
  Self.FLocation  := prLocation;
  Self.Icon       := -1;
  Self.ActionType := 0;

  Self.FConfirmBeforeExecute := False;

  Self.FVariableList := TJupiterVariableList.Create;

  Self.FOnClickScript := TStringList.Create;
  Self.FOnClickScript.Clear;
end;

constructor TJupiterAction.Create(prTitle : String; prRoute: TJupiterRoute; prLocation : TJupiterRoute);
begin
  Self.FTitle     := prTitle;
  Self.FRoute     := prRoute;
  Self.FLocation  := prLocation;
  Self.Icon       := -1;
  Self.ActionType := 0;

  Self.FConfirmBeforeExecute := False;

  Self.FVariableList := TJupiterVariableList.Create;

  Self.FOnClickScript := TStringList.Create;
  Self.FOnClickScript.Clear;
end;

constructor TJupiterAction.Create(prTitle: String; prOnClick: TNotifyEvent; prLocation: TJupiterRoute);
begin
  Self.FTitle     := prTitle;
  Self.FOnClick   := prOnClick;
  Self.FLocation  := prLocation;
  Self.Icon       := -1;
  Self.ActionType := 0;

  Self.FConfirmBeforeExecute := False;

  Self.FVariableList := TJupiterVariableList.Create;

  Self.FOnClickScript := TStringList.Create;
  Self.FOnClickScript.Clear;
end;

destructor TJupiterAction.Destroy;
begin
  FreeAndNil(Self.FVariableList);

  inherited Destroy;
end;

end.

