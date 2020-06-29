unit JupiterRunnableItem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jupiterconsts, JupiterParams;

type
  { TJupiterRunnableItem }

  TJupiterRunnableItem = class(TThread)
  private
    FItem      : TJupiterListItem;
    FParam     : TJupiterAction;
    FStatus    : TJupiterRunnableItemStatus;
    FOnChange  : TJupiterRunnableItemChangeStatus;
    FOnAddItem : TJupiterRunnableItemAddItem;
    FOnMessage : TJupiterRunnableItemMessage;
    FOnOutput  : TJupiterRunnableItemMessage;

    procedure Internal_ChangeStatus(prNewStatus : TJupiterRunnableItemStatus);
  protected
    procedure Execute; override;
    procedure Internal_Execute; virtual;

    procedure Internal_AddItem(prItem : TJupiterListItem);
    function  Internal_HasFlag(prFlag : String) : Boolean;
    function  Internal_GetFlagParam(prFlag : String) : String;
  published
    property Param  : TJupiterAction   read FParam write FParam;
    property Item   : TJupiterListItem read FItem  write FItem;
    property Status : TJupiterRunnableItemStatus read FStatus;

    property OnAddItem      : TJupiterRunnableItemAddItem      read FOnAddItem write FOnAddItem;
    property OnChangeStatus : TJupiterRunnableItemChangeStatus read FOnChange  write FOnChange;
    property OnMessage      : TJupiterRunnableItemChangeStatus read FOnMessage write FOnMessage;
    property OnOutput       : TJupiterRunnableItemChangeStatus read FOnOutput  write FOnOutput;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);

    class function ListAction : String; virtual; static;
  end;

  ClassRunnableItem = Class OF TJupiterRunnableItem;

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

function TJupiterRunnableItem.Internal_HasFlag(prFlag: String): Boolean;
begin
  Result := Pos(prFlag, Self.Param.Flags) > 0;
end;

function TJupiterRunnableItem.Internal_GetFlagParam(prFlag: String): String;
var
  vrStr : TStrings;
  vrVez : Integer;
begin
  Result := EmptyStr;

  if not Self.Internal_HasFlag(prFlag) then
    Exit;

  vrStr := TStringList.Create;
  try
    vrStr.Delimiter     := '|';
    vrStr.DelimitedText := StringReplace(Self.Param.Flags, ' ', '|', [rfReplaceAll, rfIgnoreCase]);

    for vrVez := 0 to vrStr.Count - 1 do
      if Pos(prFlag, vrStr[vrVez]) > 0 then
        Result := StringReplace(vrStr[vrVez], prFlag + ':', EmptyStr, [rfReplaceAll, rfIgnoreCase]);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
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

