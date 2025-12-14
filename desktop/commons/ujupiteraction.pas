unit uJupiterAction;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, jupiterformutils, JupiterConsts, JupiterApp,
  jupiterDatabaseWizard, JupiterVariable, ExtCtrls, Controls, Buttons, ActnList,
  LCLProc, Menus;

type

  { TJupiterAction }

  TJupiterActionOnRequestData = function : TJupiterVariableList of object;

  TJupiterAction = class(TJupiterObject)
  private
    FIcon      : Integer;
    FCaption   : String;
    FHint      : String;
    FMacroId   : String;
    FMacro     : TStrings;
    FOnClick   : TNotifyEvent;
    FButton    : TSpeedButton;
    FPopup     : TMenuItem;
    FReference : TJupiterDatabaseReference;
    FOnRequestData : TJupiterActionOnRequestData;
    FOnPopupRequestData : TJupiterActionOnRequestData;
    FOnAfterExecute : TNotifyEvent;
    FAction : TAction;

    procedure Internal_OnMacroClick(Sender : TObject);
    procedure Internal_OnMacroScriptClick(Sender : TObject);
    procedure Internal_OnDatabaseClick(Sender: TObject);
  published
    property Caption : String       read FCaption write FCaption;
    property Icon    : Integer      read FIcon    write FIcon;
    property Hint    : String       read FHint    write FHint;
    property OnClick : TNotifyEvent read FOnClick write FOnClick;
    property Reference : TJupiterDatabaseReference read FReference write FReference;
    property MacroId : String       read FMacroId write FMacroId;
    property MacroScript : TStrings read FMacro;
    property Action : TAction read FAction write FAction;
    property Button : TSpeedButton read FButton write FButton;

    property OnRequestData  : TJupiterActionOnRequestData read FOnRequestData  write FOnRequestData;
    property OnPopupRequestData : TJupiterActionOnRequestData read FOnPopupRequestData write FOnPopupRequestData;
    property OnAfterExecute : TNotifyEvent                read FOnAfterExecute write FOnAfterExecute;
  public
    constructor Create(prCaption, prHint : String; prIcon : Integer);
    constructor Create(prCaption, prHint : String; prIcon : Integer; prOnClick : TNotifyEvent);
    constructor Create(prCaption, prHint : String; prIcon : Integer; prReference : TJupiterDatabaseReference);
    constructor Create(prCaption, prHint : String; prIcon : Integer; prMacroId : String);
    constructor Create(prCaption, prHint : String; prIcon : Integer; prMacro : TStrings);

    procedure Render(prFlow : TFlowPanel; prImageList : TImageList; prPopupMenu : TPopupMenu);
    procedure Execute;

    procedure UpdateAction;
    procedure UpdatePopupAction;

    procedure Disable;
    procedure Enable;
    procedure DisablePopup;
    procedure EnablePopup;

    procedure SetInvisibility;
    procedure SetVisibility;
    procedure SetInvisibilityPopup;
    procedure SetVisibilityPopup;
  end;

  { TJupiterActionGroup }

  TJupiterActionGroup = class(TJupiterObjectList)
  private
    FCurrentData : String;
    FCurrentPoupData : String;

    FFlowPanel : TFlowPanel;
    FImageList : TImageList;
    FTableName : String;
    FActionList : TActionList;
    FPopupMenu : TPopupMenu;

    FOnRequestData : TJupiterActionOnRequestData;
    FOnPopupRequestData : TJupiterActionOnRequestData;
    FOnAfterExecute : TNotifyEvent;

    procedure Internal_SetTableName(prTableName : String);
  published
    property FlowPanel  : TFlowPanel  read FFlowPanel  write FFlowPanel;
    property PopupMenu  : TPopupMenu  read FPopupMenu  write FPopupMenu;
    property ImageList  : TImageList  read FImageList  write FImageList;
    property TableName  : String      read FTableName  write Internal_SetTableName;
    property ActionList : TActionList read FActionList write FActionList;

    property OnRequestData      : TJupiterActionOnRequestData read FOnRequestData      write FOnRequestData;
    property OnPopupRequestData : TJupiterActionOnRequestData read FOnPopupRequestData write FOnPopupRequestData;
    property OnAfterExecute     : TNotifyEvent                read FOnAfterExecute     write FOnAfterExecute;
  public
    procedure UpdateActions;
    procedure ResetActions;

    procedure AddAction(prAction : TJupiterAction);

    function GetActionAtIndex(prIndex : Integer) : TJupiterAction;

    procedure Render;
  end;

implementation

uses StdCtrls, SQLDB;

{ TJupiterAction }

procedure TJupiterAction.Internal_OnDatabaseClick(Sender: TObject);
begin
  if Sender is TSpeedButton then
  begin
    if Assigned(Self.OnRequestData) then
      vrJupiterApp.RunAction(TSpeedButton(Sender).Tag, Self.OnRequestData())
    else
      vrJupiterApp.RunAction(TSpeedButton(Sender).Tag, TJupiterVariableList.Create);

    if Assigned(Self.OnAfterExecute) then
       Self.OnAfterExecute(Sender);
  end;

  if Sender is TAction then
  begin
    if Assigned(Self.OnRequestData) then
      vrJupiterApp.RunAction(TAction(Sender).Tag, Self.OnRequestData())
    else
      vrJupiterApp.RunAction(TAction(Sender).Tag, TJupiterVariableList.Create);

    if Assigned(Self.OnAfterExecute) then
       Self.OnAfterExecute(Sender);
  end;

  if Sender is TMenuItem then
  begin
    if Assigned(Self.OnPopupRequestData) then
      vrJupiterApp.RunAction(TSpeedButton(Sender).Tag, Self.OnPopupRequestData())
    else
      vrJupiterApp.RunAction(TSpeedButton(Sender).Tag, TJupiterVariableList.Create);

    if Assigned(Self.OnAfterExecute) then
       Self.OnAfterExecute(Sender);
  end;
end;

procedure TJupiterAction.Internal_OnMacroClick(Sender : TObject);
begin
  if ((not (Sender is TSpeedButton)) and (not (Sender is TAction))) then
    Exit;

  if Assigned(Self.OnRequestData) then
    vrJupiterApp.RunMacro(Self.MacroId, Self.OnRequestData())
  else
    vrJupiterApp.RunMacro(Self.MacroId, TJupiterVariableList.Create);

  if Assigned(Self.OnAfterExecute) then
     Self.OnAfterExecute(Sender);
end;

procedure TJupiterAction.Internal_OnMacroScriptClick(Sender: TObject);
begin
  if ((not (Sender is TSpeedButton)) and (not (Sender is TAction))) then
    Exit;

  if Assigned(Self.OnRequestData) then
    vrJupiterApp.RunScript(Self.MacroScript, Self.OnRequestData())
  else
    vrJupiterApp.RunScript(Self.MacroScript, TJupiterVariableList.Create);

  if Assigned(Self.OnAfterExecute) then
     Self.OnAfterExecute(Sender);
end;

constructor TJupiterAction.Create(prCaption, prHint: String; prIcon : Integer);
begin
  Self.Caption := prCaption;
  Self.Hint    := prHint;
  Self.Icon    := prIcon;

  Self.MacroId := EmptyStr;
end;

constructor TJupiterAction.Create(prCaption, prHint : String; prIcon : Integer; prMacroId : String);
begin
  Create(prCaption, prHint, prIcon);

  Self.MacroId := prMacroId;
  Self.OnClick := @Internal_OnMacroClick;
end;

constructor TJupiterAction.Create(prCaption, prHint: String; prIcon: Integer; prMacro: TStrings);
begin
  Create(prCaption, prHint, prIcon);

  Self.FMacro := TStringList.Create;
  Self.FMacro.Clear;
  Self.FMacro.AddStrings(prMacro);

  Self.OnClick := @Internal_OnMacroScriptClick;
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

procedure TJupiterAction.Render(prFlow: TFlowPanel; prImageList : TImageList; prPopupMenu : TPopupMenu);
var
  vrSpeedButton :  TSpeedButton;
  vrMenuItem : TMenuItem;
begin
  Self.FPopup := nil;

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

  if Assigned(prPopupMenu) then
  begin
    vrMenuItem := TMenuItem.Create(prPopupMenu);
    vrMenuItem.Caption    := Self.Caption;
    vrMenuItem.Hint       := Self.Hint;
    vrMenuItem.OnClick    := OnClick;

    if Assigned(prImageList) then
    begin
      prPopupMenu.Images := prImageList;

      vrMenuItem.ImageIndex := Self.Icon;
    end;

    if Assigned(Self.Reference) then
      vrMenuItem.Tag := Self.Reference.ID;

    prPopupMenu.Items.Add(vrMenuItem);

    Self.FPopup := vrMenuItem;
  end;

  Self.FButton := vrSpeedButton;
end;

procedure TJupiterAction.Execute;
begin
  if Assigned(Self.MacroScript) then
  begin
    if Assigned(Self.OnRequestData) then
      vrJupiterApp.RunScript(Self.MacroScript, Self.OnRequestData())
    else
      vrJupiterApp.RunScript(Self.MacroScript, TJupiterVariableList.Create);

    Exit;
  end;

  if Trim(Self.MacroId) <> EmptyStr then
  begin
    if Assigned(Self.OnRequestData) then
      vrJupiterApp.RunMacro(Self.MacroId, Self.OnRequestData())
    else
      vrJupiterApp.RunMacro(Self.MacroId, TJupiterVariableList.Create);

    Exit;
  end;
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

procedure TJupiterAction.UpdatePopupAction;
var
  vrEnabled : Boolean;
  vrVisibile : Boolean;
begin
  vrEnabled := True;

  if not Assigned(Self.Reference) then
    Exit;

  if Assigned(Self.OnPopupRequestData) then
    vrVisibile := vrJupiterApp.RunAcitonVisible(Self.Reference.ID, Self.OnPopupRequestData())
  else
    vrVisibile := vrJupiterApp.RunAcitonVisible(Self.Reference.ID, TJupiterVariableList.Create);

  if vrVisibile then
    Self.SetVisibilityPopup
  else
  begin
    Self.SetInvisibilityPopup;

    Exit;
  end;

  if Assigned(Self.OnPopupRequestData) then
    vrEnabled := vrJupiterApp.RunAcitonEnabled(Self.Reference.ID, Self.OnPopupRequestData())
  else
    vrEnabled := vrJupiterApp.RunAcitonEnabled(Self.Reference.ID, TJupiterVariableList.Create);

  if vrEnabled then
    Self.EnablePopup
  else
    Self.DisablePopup;
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

  if Assigned(Self.Action) then
    Self.Action.Enabled := True;
end;

procedure TJupiterAction.DisablePopup;
begin
  if Assigned(Self.FPopup) then
    Self.FPopup.Enabled := False;
end;

procedure TJupiterAction.EnablePopup;
begin
  if Assigned(Self.FPopup) then
    Self.FPopup.Enabled := True;
end;

procedure TJupiterAction.SetInvisibility;
begin
  if Assigned(Self.FButton) then
    Self.FButton.Visible := False;

  if Assigned(Self.Action) then
    Self.Action.Enabled := False;
end;

procedure TJupiterAction.SetVisibility;
begin
  if Assigned(Self.FButton) then
    Self.FButton.Visible := True;
end;

procedure TJupiterAction.SetInvisibilityPopup;
begin
  if Assigned(Self.FPopup) then
    Self.FPopup.Visible := False;
end;

procedure TJupiterAction.SetVisibilityPopup;
begin
  if Assigned(Self.FPopup) then
    Self.FPopup.Visible := True;
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
  vrVez  : Integer;
  vrList : TJupiterVariableList;
  vrNewData : String;
  vrNewPopupData : String;
begin
  vrNewData := EmptyStr;
  vrNewPopupData := EmptyStr;

  if Assigned(Self.FOnRequestData) then
  begin
    vrList := Self.FOnRequestData();

    if Assigned(vrList) then
      vrNewData := vrList.ToString;
  end;

  if Assigned(Self.FOnPopupRequestData) then
  begin
    vrList := Self.FOnPopupRequestData();

    if Assigned(vrList) then
      vrNewPopupData := vrList.ToString;
  end;

  try
    for vrVez := 0 to Self.Count -1 do
      with TJupiterAction(Self.GetAtIndex(vrVez)) do
      begin
        if Self.FCurrentData <> vrNewData then
          UpdateAction;

        if Assigned(PopupMenu) then
          if Self.FCurrentPoupData <> vrNewPopupData then
            UpdatePopupAction;
      end;
  finally
    Self.FCurrentData     := vrNewData;
    Self.FCurrentPoupData := vrNewPopupData;
  end;
end;

procedure TJupiterActionGroup.ResetActions;
begin
  Self.FCurrentData     := 'EMPTY';
  Self.FCurrentPoupData := 'EMPTY';
end;

procedure TJupiterActionGroup.AddAction(prAction: TJupiterAction);
var
  vrShortcut : String;
begin
  Self.Add(prAction);

  TJupiterAction(Self.GetLastObject).OnRequestData  := Self.OnRequestData;
  TJupiterAction(Self.GetLastObject).OnAfterExecute := Self.OnAfterExecute;
  TJupiterAction(Self.GetLastObject).OnPopupRequestData  := Self.OnPopupRequestData;

  if not Assigned(Self.ActionList) then
    Exit;

  if Self.Count > 12 then
    Exit;

  vrShortcut := 'F' + IntToStr(Self.Count);

  with TJupiterAction(Self.GetLastObject) do
  begin
    Caption := Caption + ' (' + vrShortcut + ')';

    Action := TAction.Create(Self.ActionList);
    Action.ActionList := Self.ActionList;
    Action.Caption := Caption;
    Action.Hint := Hint;
    Action.OnExecute := OnClick;
    Action.ShortCut := TextToShortCut(vrShortcut);

    if Assigned(Reference) then
      Action.Tag := Reference.ID;
  end;
end;

function TJupiterActionGroup.GetActionAtIndex(prIndex: Integer): TJupiterAction;
begin
  Result := Self.GetAtIndex(prIndex) as TJupiterAction;
end;

procedure TJupiterActionGroup.Render;
var
  vrVez : Integer;
begin
  Self.ResetActions;

  for vrVez := 0 to Self.Count - 1 do
    TJupiterAction(Self.GetAtIndex(vrVez)).Render(Self.FlowPanel, Self.ImageList, Self.PopupMenu);
end;

end.

