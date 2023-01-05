unit JupiterAction;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterObject, JupiterRoute, JupiterRunnable, JupiterVariable,
  JupiterConsts, SysUtils, Forms, Buttons;

type

  { TJupiterAction }

  TJupiterAction = class(TJupiterObject)
  private
    FRunnable     : TJupiterRunnable;
    FLocation     : TJupiterRoute;
    FRoute        : TJupiterRoute;
    FVariableList : TJupiterVariableList;
    FTitle        : String;
    FHint         : String;
    FIcon         : SmallInt;
    FOnClick      : TNotifyEvent;

    FOnBeforeExecute : TJupiterAction;
    FOnAfterExecute  : TJupiterAction;

    FConfirmBeforeExecute : Boolean;
  published
    property Title : String   read FTitle write FTitle;
    property Hint  : String   read FHint  write FHint;
    property Icon  : SmallInt read FIcon  write FIcon;

    property Runnable : TJupiterRunnable read FRunnable write FRunnable;
    property Location : TJupiterRoute    read FLocation write FLocation;
    property Route    : TJupiterRoute    read FRoute    write FRoute;

    property ConfirmBeforeExecute : Boolean read FConfirmBeforeExecute write FConfirmBeforeExecute;

    property OnAfterExecute  : TJupiterAction read FOnAfterExecute  write FOnAfterExecute;
    property OnBeforeExecute : TJupiterAction read FOnBeforeExecute write FOnBeforeExecute;
    property OnClick         : TNotifyEvent   read FOnClick         write FOnClick;

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
  protected
    procedure Internal_OnClick(Sender: TObject);
    function Internal_CreateButton(prAction : TJupiterAction; prIndex, prLeft : Integer; prOwner : TScrollBox) : TBitBtn;
  public
    procedure CopyFromList(prCopyList : TJupiterActionList; prOnBeforeExecute : TJupiterAction = nil);

    procedure BuildActions(prOwner : TScrollBox);

    function GetActionButton(prActionIndex : Integer; prOwner : TScrollBox) : TBitBtn;
  end;

implementation

uses LCLType, JupiterApp;

{ TJupiterActionList }

procedure TJupiterActionList.Internal_OnClick(Sender: TObject);
var
  vrAction : TJupiterAction;
begin
  vrAction := TJupiterAction(Self.GetAtIndex(TBitBtn(Sender).Tag));

  if Assigned(vrAction) then
    vrAction.Execute;
end;

function TJupiterActionList.Internal_CreateButton(prAction: TJupiterAction; prIndex, prLeft: Integer; prOwner: TScrollBox): TBitBtn;
var
  vrWidth : Integer;
begin
  Result            := TBitBtn.Create(prOwner);
  Result.Parent     := prOwner;
  Result.Top        := FORM_MARGIN_TOP;
  Result.Caption    := prAction.Title;
  Result.Hint       := prAction.Hint;
  Result.ShowHint   := Trim(prAction.Hint) <> EmptyStr;
  Result.ImageIndex := prAction.Icon;
  Result.Left       := prLeft;

  if Result.ImageIndex <> - 1 then
    Result.Images := vrJupiterApp.MainIcons;

  Result.AutoSize := True;
  Application.ProcessMessages;

  vrWidth := Result.Width;

  Result.AutoSize := False;

  if vrWidth < FORM_ACTION_MINWIDTH then
    Result.Width := FORM_ACTION_MINWIDTH
  else
    Result.Width := vrWidth;

  Result.Height  := prOwner.Height - (FORM_MARGIN_TOP + FORM_MARGIN_BOTTOM);
  Result.Tag     := prIndex;
  Result.OnClick := @Self.Internal_OnClick;
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

    Self.Add(vrAction);
  end;
end;

procedure TJupiterActionList.BuildActions(prOwner: TScrollBox);
var
  vrLeft  : Integer;
  vrVez   : Integer;
  vrSpeed : TBitBtn;
begin
  vrLeft := FORM_MARGIN_LEFT;

  for vrVez := 0 to Self.Size - 1 do
  begin
    vrSpeed := Self.Internal_CreateButton(TJupiterAction(Self.GetAtIndex(vrVez)), vrVez, vrLeft, prOwner);

    vrLeft := vrSpeed.Left + vrSpeed.Width + FORM_MARGIN_LEFT;
  end;
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

{ TJupiterAction }

procedure TJupiterAction.Execute;
begin
  if Self.ConfirmBeforeExecute then
    if Application.MessageBox(PAnsiChar('Deseja realmente ' + Self.Title + '?'), PAnsiChar(Self.Title), MB_ICONQUESTION + MB_YESNO) = ID_NO then
      Exit;

  if Assigned(Self.OnBeforeExecute) then
    Self.OnBeforeExecute.Execute;

  if Assigned(Self.FOnClick) then
    Self.OnClick(Application.MainForm);

  if Assigned(Self.Runnable) then
    Self.Runnable.Execute;

  if Assigned(Self.Route) then
    vrJupiterApp.NavigateTo(Self.Route, not Application.MainForm.Showing);

  if Assigned(Self.OnAfterExecute) then
    Self.OnAfterExecute.Execute;
end;

constructor TJupiterAction.Create(prTitle : String; prRunnable: TJupiterRunnable; prLocation : TJupiterRoute);
begin
  Self.FTitle    := prTitle;
  Self.FRunnable := prRunnable;
  Self.FLocation := prLocation;
  Self.Icon      := -1;

  Self.FConfirmBeforeExecute := False;

  Self.FVariableList := TJupiterVariableList.Create;
end;

constructor TJupiterAction.Create(prTitle : String; prRoute: TJupiterRoute; prLocation : TJupiterRoute);
begin
  Self.FTitle    := prTitle;
  Self.FRoute    := prRoute;
  Self.FLocation := prLocation;
  Self.Icon      := -1;

  Self.FConfirmBeforeExecute := False;

  Self.FVariableList := TJupiterVariableList.Create;
end;

constructor TJupiterAction.Create(prTitle: String; prOnClick: TNotifyEvent; prLocation: TJupiterRoute);
begin
  Self.FTitle    := prTitle;
  Self.FOnClick  := prOnClick;
  Self.FLocation := prLocation;
  Self.Icon      := -1;

  Self.FConfirmBeforeExecute := False;

  Self.FVariableList := TJupiterVariableList.Create;
end;

destructor TJupiterAction.Destroy;
begin
  FreeAndNil(Self.FVariableList);

  inherited Destroy;
end;

end.

