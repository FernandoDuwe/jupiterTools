unit jupiterThread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterConsts, JupiterObject, JupiterRunnable, {$ifdef unix} cthreads, {$endif} SysUtils;

type
  TJupiterThreadOnUpdateMonitor = procedure of object;

  { TJupiterThread }

  TJupiterThread = class(TThread)
  private
    FID                : Integer;
    FTitle             : String;
    FRunnable          : TJupiterRunnable;
    FStatus            : TJupiterThreadsStatus;
    FStartedAt         : TDateTime;
    FEndedAt           : TDateTime;
    FJupiterThreadList : TJupiterObject;
  protected
    procedure Execute; override;
  published
    property ID        : Integer   read FID    write FID;
    property Title     : String    read FTitle write FTitle;
    property StartedAt : TDateTime read FStartedAt;
    property EndedAt   : TDateTime read FEndedAt;

    property JupiterThreadList : TJupiterObject        read FJupiterThreadList write FJupiterThreadList;
    property Runnable          : TJupiterRunnable      read FRunnable          write FRunnable;
    property Status            : TJupiterThreadsStatus read FStatus;
  public
    constructor Create(CreateSuspended : Boolean);
    constructor Create(CreateSuspended : Boolean; prRunnable : TJupiterRunnable);
  end;

  { TJupiterThreadList }

  TJupiterThreadList = class(TJupiterObject)
  protected
    FInternal_ID     : Integer;
    FList            : TList;

    function Internal_GetSize : Integer;
  published
    property Count : Integer read Internal_GetSize;
    property Size  : Integer read Internal_GetSize;
  public
    function ThreadByIndex(prIndex : Integer) : TJupiterThread;
    function ThreadByD(prID : Integer) : TJupiterThread;
    procedure NewThread(prTitle : String; prRunnable : TJupiterRunnable);

    constructor Create;
    destructor Destroy; override;
  end;


implementation

{ TJupiterThreadList }

function TJupiterThreadList.Internal_GetSize: Integer;
begin
  Result := Self.FList.Count;
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

procedure TJupiterThreadList.NewThread(prTitle : String; prRunnable: TJupiterRunnable);
var
  vrThread : TJupiterThread;
begin
  Self.FInternal_ID := Self.FInternal_ID + 1;

  try
    vrThread       := TJupiterThread.Create(True, prRunnable);
    vrThread.ID    := Self.FInternal_ID;
    vrThread.Title := prTitle;
    vrThread.JupiterThreadList := Self;

    Self.FList.Add(vrThread);

    vrThread.Resume;
  finally

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

{ TJupiterThread }

procedure TJupiterThread.Execute;
begin
  Self.FStatus := jtsRunning;

  Self.FStartedAt := Now;
  try
    if Assigned(Self.FRunnable) then
      Self.FRunnable.Execute;
  finally
    Self.FStatus := jtsFinished;

    Self.FEndedAt := Now;
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

constructor TJupiterThread.Create(CreateSuspended: Boolean; prRunnable: TJupiterRunnable);
begin
  inherited Create(CreateSuspended);

  Self.FStatus    := jtsNotStarted;
  Self.FStartedAt := 0.0;
  Self.FEndedAt   := 0.0;
  Self.FRunnable  := prRunnable;

  FreeOnTerminate := False;
end;

end.

