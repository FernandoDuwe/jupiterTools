unit JupiterEdit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TJupiterEdit }

  TJupiterEdit = class(TEdit)
  private
    FActionList : TList;

    procedure ResizeControl;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddAction(prNewAction : TSpeedButton);
  published
    property ActionList : TList read FActionList write FActionList;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Jupiter', [TJupiterEdit]);
end;

{ TJupiterEdit }

procedure TJupiterEdit.ResizeControl;
var
  vrVez : Integer;
  vrWidth : Integer;
begin
  vrWidth := 0;

  if not Assigned(Self.ActionList) then
    Exit;

  for vrVez := Self.ActionList.Count - 1 downto 0 do
  begin
    TSpeedButton(Self.ActionList.Items[vrVez]).Height := Self.Height - 4;
    TSpeedButton(Self.ActionList.Items[vrVez]).Top    := 2;
    TSpeedButton(Self.ActionList.Items[vrVez]).Left   := Width - (TSpeedButton(Self.ActionList.Items[vrVez]).Width + vrWidth) - 2;

    vrWidth := vrWidth + TSpeedButton(Self.ActionList.Items[vrVez]).Width;
  end;
end;

procedure TJupiterEdit.Resize;
begin
  inherited Resize;

  Self.ResizeControl;
end;

constructor TJupiterEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.FActionList := TList.Create;

  Self.ResizeControl;
end;

destructor TJupiterEdit.Destroy;
begin
  FreeAndNil(Self.FActionList);

  inherited Destroy;
end;

procedure TJupiterEdit.AddAction(prNewAction: TSpeedButton);
begin
  try
    Self.ActionList.Add(prNewAction);
  finally
    Self.ResizeControl;
  end;
end;

end.
