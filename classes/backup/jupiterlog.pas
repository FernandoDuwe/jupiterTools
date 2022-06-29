unit jupiterLog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TJupiterLogNode }

  TJupiterLogNode = class(TObject)
  private
    FData    : TDateTime;
    FMessage : String;
    FDetails : String;
  published
    property Data    : TDateTime read FData    write FData;
    property Message : String    read FMessage write FMessage;
    property Details : String    read FDetails write FDetails;

  public
    constructor Create(prData : TDateTime; prMessage : String; prDetails : String = '');
  end;

  TJupiterLog = class(TObject)
  private
    FList : TList;
    function Internal_GetCount: Integer;
  published
    property Count : Integer read Internal_GetCount;
  public
    function GetByIndex(prIndex : Integer) : TJupiterLogNode;

    procedure AddLog(prData : TDateTime; prMessage : String; prDetails : String = '');

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TJupiterLog }

function TJupiterLog.Internal_GetCount: Integer;
begin
  Result := Self.FList.Count;
end;

function TJupiterLog.GetByIndex(prIndex: Integer): TJupiterLogNode;
begin
  Result := TJupiterLogNode(Self.FList[prData]);
end;

procedure TJupiterLog.AddLog(prData: TDateTime; prMessage: String; prDetails: String);
begin
  Self.FList.Add(TJupiterLogNode.Create(prData, prMessage, prDetails));
end;

constructor TJupiterLog.Create;
begin
  Self.FList := TList.Create;
  Self.FList.Clear;
end;

destructor TJupiterLog.Destroy;
begin
  while Self.FList.Count > 0 do
  begin
    TJupiterLogNode(Self.FList[0]).Free;

    Self.FList.Delete(0);
  end;

  Self.FList.Clear;

  FreeAndNil(Self.FList);

  inherited Destroy;
end;

{ TJupiterLogNode }

constructor TJupiterLogNode.Create(prData: TDateTime; prMessage: String;
  prDetails: String);
begin
  Self.Data    := prData;
  Self.Message := prMessage;
  Self.Details := prDetails;
end;

end.

