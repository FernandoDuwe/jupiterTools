unit jupiter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JupiterRunnableItem, JupiterRunnableItemListDirectory,
  JupiterRunnableItemListFromFile;

type

  { TJupiter }

  TJupiter = class(TObject)
  private
    FList : TList;
  published
    property
  public
    procedure RegisterRunnable(prRunnable : ClassRunnableItem);
    procedure Prepare;
    function  CreateRunnable(prRunnableName : String) : TJupiterRunnableItem;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TJupiter }

procedure TJupiter.RegisterRunnable(prRunnable: ClassRunnableItem);
begin
  Self.FList.Add(prRunnable);
end;

procedure TJupiter.Prepare;
begin
  Self.RegisterRunnable(TJupiterRunnableItemListDirectory);
  Self.RegisterRunnable(TJupiterRunnableItemListFromFile);
end;

function TJupiter.CreateRunnable(prRunnableName: String): TJupiterRunnableItem;
var
  vrVez   : Integer;
  vrClass : ClassRunnableItem;
begin
  Result := nil;

  for vrVez := 0 to Self.FList.Count - 1 do
  begin
    vrClass := ClassRunnableItem(Self.FList[vrVez]);

    if AnsiUpperCase(prRunnableName) = AnsiUpperCase(vrClass.ListAction) then
      Result := vrClass.Create(True);
  end;
end;

constructor TJupiter.Create;
begin
  Self.FList := TList.Create;

  Self.Prepare;
end;

destructor TJupiter.Destroy;
begin
  Self.FList.Clear;
  FreeAndNil(Self.FList);

  inherited Destroy;
end;

end.

