unit jupiterthread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, JupiterConsts, jupiterScript;

type
  TJupiterThreadOnExecute = procedure(prThreadId : Integer; prParams : String) of object;
  TJupiterThreadOnUpdateMonitor = procedure of object;

  { TJupiterThread }

  TJupiterThread = class(TThread)
  private
    FID                : Integer;
    FTitle             : String;
    FScript            : TJupiterScript;
    FStatus            : TJupiterThreadsStatus;
    FStartedAt         : TDateTime;
    FEndedAt           : TDateTime;
    FJupiterThreadList : TJupiterObject;
    FOnExecute         : TJupiterThreadOnExecute;
    FOnExecuted        : TJupiterThreadOnExecute;
    FParams            : String;
  protected
    procedure Internal_Execute; virtual;

    procedure Execute; override;
  published
    property ID        : Integer                 read FID        write FID;
    property Title     : String                  read FTitle     write FTitle;
    property StartedAt : TDateTime               read FStartedAt;
    property EndedAt   : TDateTime               read FEndedAt;
    property Params    : String                  read FParams    write FParams;

    property OnExecute  : TJupiterThreadOnExecute read FOnExecute  write FOnExecute;
    property OnExecuted : TJupiterThreadOnExecute read FOnExecuted write FOnExecuted;

    property Script            : TJupiterScript        read FScript            write FScript;
    property JupiterThreadList : TJupiterObject        read FJupiterThreadList write FJupiterThreadList;
    property Status            : TJupiterThreadsStatus read FStatus;
  public
    constructor Create(CreateSuspended : Boolean);
    constructor Create(CreateSuspended : Boolean; prScript : TJupiterScript);
  end;

  { TJupiterThreadList }

  TJupiterThreadList = class(TJupiterObject)
  protected
    FInternal_ID     : Integer;
    FList            : TList;

    function Internal_GetSize : Integer;
    function Internal_IsRunning : Boolean;
  published
    property Count   : Integer read Internal_GetSize;
    property Running : Boolean read Internal_IsRunning;
    property Size    : Integer read Internal_GetSize;
  public
    function ThreadByIndex(prIndex : Integer) : TJupiterThread;
    function ThreadByD(prID : Integer) : TJupiterThread;
    procedure AddThread(prThread : TJupiterThread);
    procedure NewThread(prTitle : String; prScript : TJupiterScript);
    procedure NewThread(prTitle, prParams : String; prOnExecute : TJupiterThreadOnExecute);

    procedure DeleteAtIndex(prIndex : Integer);
    procedure DeleteListItem(prIndex : Integer);

    procedure StopAll;

    constructor Create;
    destructor Destroy; override;
  end;


implementation

{ TJupiterThread }

procedure TJupiterThread.Internal_Execute;
begin

end;

procedure TJupiterThread.Execute;
begin
  Self.FStatus := jtsRunning;

  Self.FStartedAt := Now;
  try
    if Assigned(Self.FScript) then
       if Assigned(Self.FScript.RunMessages) then
         Self.FScript.Execute;

    Self.Internal_Execute;

    if Assigned(Self.OnExecute) then
      Self.OnExecute(Self.ThreadID, Self.Params);
  finally
    Self.FStatus := jtsFinished;

    Self.FEndedAt := Now;

    if Assigned(Self.OnExecuted) then
      Self.OnExecuted(Self.ThreadID, Self.Params);

    Self.Suspend;
  end;
end;

constructor TJupiterThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);

  Self.FStatus    := jtsNotStarted;
  Self.FStartedAt := 0.0;
  Self.FEndedAt   := 0.0;

  FreeOnTerminate := False;
end;

constructor TJupiterThread.Create(CreateSuspended: Boolean; prScript: TJupiterScript);
begin
  inherited Create(CreateSuspended);

  Self.FStatus    := jtsNotStarted;
  Self.FStartedAt := 0.0;
  Self.FEndedAt   := 0.0;
  Self.FScript    := prScript;

  FreeOnTerminate := False;
end;

{ TJupiterThreadList }

function TJupiterThreadList.Internal_GetSize: Integer;
begin
  Result := Self.FList.Count;
end;

function TJupiterThreadList.Internal_IsRunning: Boolean;
var
  vrVez : Integer;
begin
  Result := False;

  if Self.Size = 0 then
    Exit;

  for vrVez := 0 to Self.Size - 1 do
    if ((not Self.ThreadByIndex(vrVez).Suspended) and (Self.ThreadByIndex(vrVez).Status <> jtsFinished)) then
    begin
      Result := True;
      Exit;
    end;
end;

function TJupiterThreadList.ThreadByIndex(prIndex: Integer): TJupiterThread;
begin
  Result := TJupiterThread(Self.FList[prIndex]);
end;

function TJupiterThreadList.ThreadByD(prID: Integer): TJupiterThread;
var
  vrVez : Integer;
begin
  Result := nil;

  for vrVez := 0 to Self.Size - 1 do
    if Self.ThreadByIndex(vrVez).ID = prID then
    begin
      Result := Self.ThreadByIndex(vrVez);
      Exit;
    end;
end;

procedure TJupiterThreadList.AddThread(prThread: TJupiterThread);
begin
  Self.FInternal_ID := Self.FInternal_ID + 1;

  prThread.ID := Self.FInternal_ID;

  Self.FList.Add(prThread);

  prThread.Resume;
end;

procedure TJupiterThreadList.NewThread(prTitle: String; prScript : TJupiterScript);
var
  vrThread : TJupiterThread;
begin
  Self.FInternal_ID := Self.FInternal_ID + 1;

  try
    vrThread       := TJupiterThread.Create(True, prScript);
    vrThread.ID    := Self.FInternal_ID;
    vrThread.Title := prTitle;
    vrThread.JupiterThreadList := Self;

    Self.FList.Add(vrThread);

    vrThread.Resume;
  finally

  end;
end;

procedure TJupiterThreadList.NewThread(prTitle, prParams: String; prOnExecute: TJupiterThreadOnExecute);
var
  vrThread : TJupiterThread;
begin
  Self.FInternal_ID := Self.FInternal_ID + 1;

  try
    vrThread       := TJupiterThread.Create(True);
    vrThread.ID    := Self.FInternal_ID;
    vrThread.Title := prTitle;
    vrThread.JupiterThreadList := Self;
    vrThread.Params := prParams;
    vrThread.OnExecute := prOnExecute;

    Self.FList.Add(vrThread);

    vrThread.Resume;
  finally

  end;
end;

procedure TJupiterThreadList.DeleteAtIndex(prIndex: Integer);
var
  vrObj : TThread;
begin
  vrObj := Self.ThreadByIndex(prIndex);

  FreeAndNil(vrObj);

  Self.DeleteListItem(prIndex);

end;

procedure TJupiterThreadList.DeleteListItem(prIndex: Integer);
begin
  Self.FList.Delete(prIndex);
end;

procedure TJupiterThreadList.StopAll;
var
  vrVez : Integer;
begin
  for vrVez := Self.Count - 1 downto 0 do
  begin
    if not Self.ThreadByIndex(vrVez).Suspended then
      Self.ThreadByIndex(vrVez).Suspend;

    Self.DeleteAtIndex(vrVez);
  end;
end;

constructor TJupiterThreadList.Create;
begin
  Self.FInternal_ID := 0;

  Self.FList := TList.Create;
  Self.FList.Clear;
end;

destructor TJupiterThreadList.Destroy;
begin
  while Self.FList.Count > 0 do
  begin
    Self.ThreadByIndex(0).Terminate;
    Self.FList.Delete(0);
  end;

  Self.FList.Clear;
  FreeAndNil(Self.FList);

  inherited Destroy;
end;

end.

