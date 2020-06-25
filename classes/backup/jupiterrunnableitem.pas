unit JupiterRunnableItem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jupiterconsts, JupiterParams;

type
  { TJupiterRunnableItem }

  TJupiterRunnableItem = class(TThread)
  private
    FParam     : TJupiterAction;
    FStatus    : TJupiterRunnableItemStatus;
    FOnChange  : TJupiterRunnableItemChangeStatus;
    FOnAddItem : TJupiterRunnableItemAddItem;

    procedure Internal_ChangeStatus(prNewStatus : TJupiterRunnableItemStatus);
  protected
    procedure Execute; override;
    procedure Internal_Execute; virtual;

    procedure Internal_AddItem(prItem : TJupiterListItem);
  published
    property Param  : TJupiterParamsNode read FParam write FParam;
    property Status : TJupiterRunnableItemStatus read FStatus;

    property OnAddItem      : TJupiterRunnableItemAddItem      read FOnAddItem write FOnAddItem;
    property OnChangeStatus : TJupiterRunnableItemChangeStatus read FOnChange  write FOnChange;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);

    class function ListAction : String; virtual; static;
  end;

implementation

{ TJupiterRunnableItem }

procedure TJupiterRunnableItem.Internal_ChangeStatus(prNewStatus: TJupiterRunnableItemStatus);
begin
  Self.FStatus := prNewStatus;

  if Assigned(Self.FOnChange) then
    Self.FOnChange(Self, prNewStatus);
end;

procedure TJupiterRunnableItem.Execute;
begin
  try
    Self.Internal_ChangeStatus(jrsRunning);

    Self.Internal_Execute;

    Self.Internal_ChangeStatus(jrsDone);
  except
    Self.Internal_ChangeStatus(jrsError);
  end;
end;

procedure TJupiterRunnableItem.Internal_Execute;
begin
  //
end;

procedure TJupiterRunnableItem.Internal_AddItem(prItem: TJupiterListItem);
begin
  if Assigned(Self.FOnAddItem) then
    Self.FOnAddItem(Self, prItem);
end;

constructor TJupiterRunnableItem.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);

  Self.FStatus := jrsNotStarted;

  Self.FreeOnTerminate := True;
end;

class function TJupiterRunnableItem.ListAction: String;
begin
  Result := ClassName;
end;


end.

