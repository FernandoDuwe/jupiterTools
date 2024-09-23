unit uJupiterAction;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, jupiterformutils, JupiterConsts, JupiterApp,
  jupiterDatabaseWizard, JupiterVariable, ExtCtrls, Controls, Buttons;

type

  { TJupiterAction }

  TJupiterActionOnRequestData = function : TJupiterVariableList of object;

  TJupiterAction = class(TJupiterObject)
  private
    FIcon      : Integer;
    FCaption   : String;
    FHint      : String;
    FOnClick   : TNotifyEvent;
    FButton    : TSpeedButton;
    FReference : TJupiterDatabaseReference;
    FOnRequestData : TJupiterActionOnRequestData;

    procedure Internal_OnDatabaseClick(Sender: TObject);
  published
    property Caption : String       read FCaption write FCaption;
    property Icon    : Integer      read FIcon    write FIcon;
    property Hint    : String       read FHint    write FHint;
    property OnClick : TNotifyEvent read FOnClick write FOnClick;
    property Reference : TJupiterDatabaseReference read FReference write FReference;

    property OnRequestData : TJupiterActionOnRequestData read FOnRequestData write FOnRequestData;
  public
    constructor Create(prCaption, prHint : String; prIcon : Integer);
    constructor Create(prCaption, prHint : String; prIcon : Integer; prOnClick : TNotifyEvent);
    constructor Create(prCaption, prHint : String; prIcon : Integer; prReference : TJupiterDatabaseReference);

    procedure Render(prFlow : TFlowPanel; prImageList : TImageList);

    procedure UpdateAction;

    procedure Disable;
    procedure Enable;

    procedure SetInvisibility;
    procedure SetVisibility;
  end;

  { TJupiterActionGroup }

  TJupiterActionGroup = class(TJupiterObjectList)
  private
    FFlowPanel : TFlowPanel;
    FImageList : TImageList;
    FTableName : String;
    FOnRequestData : TJupiterActionOnRequestData;

    procedure Internal_SetTableName(prTableName : String);
  published
    property FlowPanel : TFlowPanel read FFlowPanel write FFlowPanel;
    property ImageList : TImageList read FImageList write FImageList;
    property TableName : String     read FTableName write Internal_SetTableName;

    property OnRequestData : TJupiterActionOnRequestData read FOnRequestData write FOnRequestData;
  public
    procedure UpdateActions;

    procedure AddAction(prAction : TJupiterAction);

    function GetActionAtIndex(prIndex : Integer) : TJupiterAction;

    procedure Render;
  end;

implementation

uses SQLDB;

{ TJupiterAction }

procedure TJupiterAction.Internal_OnDatabaseClick(Sender: TObject);
begin
  if not (Sender is TSpeedButton) then
    Exit;

  if Assigned(Self.OnRequestData) then
    vrJupiterApp.RunAction(TSpeedButton(Sender).Tag, Self.OnRequestData())
  else
    vrJupiterApp.RunAction(TSpeedButton(Sender).Tag, TJupiterVariableList.Create);
end;

constructor TJupiterAction.Create(prCaption, prHint: String; prIcon : Integer);
begin
  Self.Caption := prCaption;
  Self.Hint    := prHint;
  Self.Icon    := prIcon;
end;

constructor TJupiterAction.Create(prCaption, prHint: String; prIcon: Integer; prOnClick: TNotifyEvent);
begin
  Create(prCaption, prHint, prIcon);

  Self.OnClick := prOnClick;
end;

constructor TJupiterAction.Create(prCaption, prHint: String; prIcon: Integer; prReference: TJupiterDatabaseReference);
begin
  Create(prCaption, prHint, prIcon);

  Self.Reference := prReference;
  Self.OnClick   := @Internal_OnDatabaseClick;
end;

procedure TJupiterAction.Render(prFlow: TFlowPanel; prImageList : TImageList);
var
  vrSpeedButton :  TSpeedButton;
begin
  vrSpeedButton            := TSpeedButton.Create(prFlow);
  vrSpeedButton.Parent     := prFlow;
  vrSpeedButton.Caption    := Self.Caption;
  vrSpeedButton.Hint       := Self.Hint;
  vrSpeedButton.ShowHint   := Self.Hint <> EmptyStr;
  vrSpeedButton.Flat       := True;
  vrSpeedButton.Height     := GetTextHeight(vrSpeedButton.Caption, vrSpeedButton.Font) + FORM_MARGIN_TOP + FORM_MARGIN_BOTTOM + 10;
  vrSpeedButton.Width      := GetTextWidth(vrSpeedButton.Caption, vrSpeedButton.Font) + FORM_MARGIN_LEFT + FORM_MARGIN_RIGHT + 30;
  vrSpeedButton.OnClick    := OnClick;

  if Assigned(prImageList) then
  begin
    vrSpeedButton.ImageIndex := Self.Icon;
    vrSpeedButton.Images     := prImageList;
    vrSpeedButton.Width      := vrSpeedButton.Width + 16;
  end;

  if Assigned(Self.Reference) then
    vrSpeedButton.Tag := Self.Reference.ID;

  Self.FButton := vrSpeedButton;
end;

procedure TJupiterAction.UpdateAction;
var
  vrEnabled : Boolean;
  vrVisibile : Boolean;
begin
  vrEnabled := True;

  if not Assigned(Self.Reference) then
    Exit;

  if Assigned(Self.OnRequestData) then
    vrVisibile := vrJupiterApp.RunAcitonVisible(Self.Reference.ID, Self.OnRequestData())
  else
    vrVisibile := vrJupiterApp.RunAcitonVisible(Self.Reference.ID, TJupiterVariableList.Create);

  if vrVisibile then
    Self.SetVisibility
  else
  begin
    Self.SetInvisibility;

    Exit;
  end;

  if Assigned(Self.OnRequestData) then
    vrEnabled := vrJupiterApp.RunAcitonEnabled(Self.Reference.ID, Self.OnRequestData())
  else
    vrEnabled := vrJupiterApp.RunAcitonEnabled(Self.Reference.ID, TJupiterVariableList.Create);

  if vrEnabled then
    Self.Enable
  else
    Self.Disable;
end;

procedure TJupiterAction.Disable;
begin
  if Assigned(Self.FButton) then
    Self.FButton.Enabled := False;
end;

procedure TJupiterAction.Enable;
begin
  if Assigned(Self.FButton) then
    Self.FButton.Enabled := True;
end;

procedure TJupiterAction.SetInvisibility;
begin
  if Assigned(Self.FButton) then
    Self.FButton.Visible := False;
end;

procedure TJupiterAction.SetVisibility;
begin
  if Assigned(Self.FButton) then
    Self.FButton.Visible := True;
end;

{ TJupiterActionGroup }

procedure TJupiterActionGroup.Internal_SetTableName(prTableName: String);
var
  vrWizard : TJupiterDatabaseWizard;
  vrQuery : TSQLQuery;
begin
  Self.FTableName := prTableName;

  if Trim(Self.TableName) = EmptyStr then
    Exit;

  vrWizard := vrJupiterApp.NewWizard;
  vrQuery := vrWizard.NewQuery;
  try
    vrQuery.SQL.Add(' SELECT A1.ID, A1.NAME, A1.TITLE, A1.ICON FROM ACTIONS A1 WHERE A1.TABLENAME = :PRTABLENAME ORDER BY A1.ZINDEX ');
    vrQuery.ParamByName('PRTABLENAME').AsString := prTableName;
    vrQuery.Open;
    vrQuery.First;

    while not vrQuery.EOF do
    begin
      Self.AddAction(TJupiterAction.Create(vrQuery.FieldByName('TITLE').AsString,
                                           vrQuery.FieldByName('TITLE').AsString,
                                           vrQuery.FieldByName('ICON').AsInteger,
                                           TJupiterDatabaseReference.Create(prTableName, vrQuery.FieldByName('ID').AsInteger)
                                           ));

      vrQuery.Next;
    end;
  finally
    FreeAndNil(vrWizard);
    FreeAndNil(vrQuery);
  end;
end;

procedure TJupiterActionGroup.UpdateActions;
var
  vrVez : Integer;
begin
  for vrVez := 0 to Self.Count -1 do
    with TJupiterAction(Self.GetAtIndex(vrVez)) do
      UpdateAction;
end;

procedure TJupiterActionGroup.AddAction(prAction: TJupiterAction);
begin
  Self.Add(prAction);

  TJupiterAction(Self.GetLastObject).OnRequestData := Self.OnRequestData;
end;

function TJupiterActionGroup.GetActionAtIndex(prIndex: Integer): TJupiterAction;
begin
  Result := Self.GetAtIndex(prIndex) as TJupiterAction;
end;

procedure TJupiterActionGroup.Render;
var
  vrVez : Integer;
begin
  for vrVez := 0 to Self.Count - 1 do
    TJupiterAction(Self.GetAtIndex(vrVez)).Render(Self.FlowPanel, Self.ImageList);
end;

end.

