unit JupiterObject;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TJupiterObject }

  TJupiterObject = class(TObject)
  private
    FTag : Integer;
  published
    property Tag : Integer read FTag write FTag;
  public
    procedure StrToObject(prObjStr : String); virtual;
    function ObjectToStr : String; virtual;
  end;

  { TJupiterObjectList }

  TJupiterObjectList = class(TJupiterObject)
  protected
    FList : TList;

    function Internal_GetSize : Integer;
  published
    property Count : Integer read Internal_GetSize;
    property Size  : Integer read Internal_GetSize;
  public
    function GetAtIndex(prIndex : Integer) : TJupiterObject;
    function GetAtIndexAsObject(prIndex : Integer) : TObject;

    procedure Add(prJupiterObject : TJupiterObject);
    procedure AddSimpleObject(prObject : TObject);

    procedure Merge(prOtherList : TJupiterObjectList);
    procedure DeleteAtIndex(prIndex : Integer);
    procedure DeleteListItem(prIndex : Integer);

    function IsEmpty : Boolean;

    procedure ClearListItens;

    function GetLastObject : TJupiterObject;

    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TJupiterObjectList }

function TJupiterObjectList.Internal_GetSize: Integer;
begin
  Result := Self.FList.Count;
end;

function TJupiterObjectList.GetAtIndex(prIndex: Integer): TJupiterObject;
begin
  Result := nil;

  if ((prIndex >= 0) and (prIndex <= (Self.Size - 1))) then
    Result := TJupiterObject(Self.FList[prIndex]);
end;

function TJupiterObjectList.GetAtIndexAsObject(prIndex: Integer): TObject;
begin
  Result := nil;

  if ((prIndex >= 0) and (prIndex <= (Self.Size - 1))) then
    Result := TObject(Self.FList[prIndex]);
end;

procedure TJupiterObjectList.Add(prJupiterObject: TJupiterObject);
begin
  Self.FList.Add(prJupiterObject);
end;

procedure TJupiterObjectList.AddSimpleObject(prObject: TObject);
begin
  Self.FList.Add(prObject);
end;

procedure TJupiterObjectList.Merge(prOtherList: TJupiterObjectList);
var
  vrVez : Integer;
begin
  for vrVez := 0 to prOtherList.Size - 1 do
    Self.Add(prOtherList.GetAtIndex(vrVez));
end;

procedure TJupiterObjectList.DeleteAtIndex(prIndex: Integer);
var
  vrObj : TJupiterObject;
begin
  vrObj := Self.GetAtIndex(prIndex);

  FreeAndNil(vrObj);

  Self.DeleteListItem(prIndex);
end;

procedure TJupiterObjectList.DeleteListItem(prIndex: Integer);
begin
  Self.FList.Delete(prIndex);
end;

function TJupiterObjectList.IsEmpty: Boolean;
begin
  Result := Self.Count = 0;
end;

procedure TJupiterObjectList.ClearListItens;
begin
  Self.FList.Clear;
end;

function TJupiterObjectList.GetLastObject: TJupiterObject;
begin
  Result := nil;

  if Self.Size > 0 then
    Result := Self.GetAtIndex(Self.Size -1);
end;

constructor TJupiterObjectList.Create;
begin
  Self.FList := TList.Create;
  Self.FList.Clear;
end;

destructor TJupiterObjectList.Destroy;
begin
  while Self.FList.Count > 0 do
  begin
    if Assigned(Self.GetAtIndexAsObject(0)) then
      Self.GetAtIndexAsObject(0).Free;

    Self.FList.Delete(0);
  end;

  Self.FList.Clear;
  FreeAndNil(Self.FList);

  inherited Destroy;
end;

{ TJupiterObject }

procedure TJupiterObject.StrToObject(prObjStr: String);
begin
  //
end;

function TJupiterObject.ObjectToStr: String;
begin
  //
end;

end.

