unit JupiterFormGenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, JupiterConsts, JupiterObject, JupiterVariable,
  JupiterFormField, JupiterVariableForm, JupiterAction, jupiterformaction,
  SysUtils, Controls, Graphics;

type

  { TJupiterFormGenerator }

  TJupiterFormGenerator = class(TJupiterObject)
  private
    FContainer           : TScrollBox;
    FVariables           : TJupiterVariableFormList;
    FClearContainerOnSet : Boolean;
    FLastTop             : Integer;
    FAlreadyDrawed       : Boolean;
    FActionsInForm       : Boolean;
    FOnKeyUp             : TKeyEvent;
    FActionList          : TJupiterActionList;
    FRaised              : Boolean;
    FActionColor         : TColor;

    procedure Internal_CreateComponent(prVariable : TJupiterVariableForm; prTabOrder : Integer);
    procedure Internal_CreateAction(prAction : TJupiterAction; prTabOrder : Integer);
    procedure Internal_ClearContainer;
    procedure Internal_DrawForm;
  published
    property ActionList : TJupiterActionList read FActionList write FActionList;
    property ActionsInForm : Boolean read FActionsInForm write FActionsInForm;
    property ClearContainerOnSet : Boolean read FClearContainerOnSet write FClearContainerOnSet default False;
    property Raised   : Boolean        read FRaised   write FRaised;
    property ActionColor : TColor read FActionColor write FActionColor;

    property Container : TScrollBox               read FContainer write FContainer;
    property Variables : TJupiterVariableFormList read FVariables write FVariables;

    property OnKeyUp : TKeyEvent read FOnKeyUp write FOnKeyUp;
  public
    procedure DrawForm;

    procedure SetVariables(prVariables : TJupiterVariableFormList);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses ExtCtrls, JupiterApp;

{ TJupiterFormGenerator }

procedure TJupiterFormGenerator.Internal_CreateComponent(prVariable: TJupiterVariableForm; prTabOrder : Integer);
var
  vrField : TJupiterFormField;
begin
  if vrJupiterApp.Params.Exists(prVariable.ID) then
    prVariable.Value := vrJupiterApp.Params.VariableById(prVariable.ID).Value;

  vrField := TJupiterFormField.Create;
  vrField.Variable := prVariable;
  vrField.TabOrder := prTabOrder;
  vrField.Draw(Self.Container);
  vrField.Panel.Top := Self.FLastTop;
  vrField.OnKeyUp := Self.OnKeyUp;

  Self.FLastTop := vrField.Panel.Top + vrField.Panel.Height + 1;
end;

procedure TJupiterFormGenerator.Internal_CreateAction(prAction: TJupiterAction; prTabOrder : Integer);
var
  vrAction : TJupiterFormAction;
begin
  vrAction := TJupiterFormAction.Create;
  vrAction.Raised := Self.Raised;
  vrAction.Action :=  prAction;
  vrAction.TabOrder := prTabOrder;
  vrAction.Draw(Self.Container);
  vrAction.Panel.Top := Self.FLastTop;

  Self.FLastTop := vrAction.Panel.Top + vrAction.Panel.Height + 1;
end;

procedure TJupiterFormGenerator.Internal_ClearContainer;
begin
  while Self.Container.ControlCount > 0 do
    Self.Container.Controls[0].Free;
end;

procedure TJupiterFormGenerator.Internal_DrawForm;
var
  vrVez : Integer;
  vrTabOrder : Integer;
  vrContainer : TPanel;
begin
  Self.FLastTop := 0;

  for vrVez := 0 to Self.Variables.Size - 1 do
  begin
    Self.Internal_CreateComponent(Self.Variables.VariableFormByIndex(vrVez), vrVez + 1);

    vrTabOrder := vrVez + 1;
  end;

  vrTabOrder := vrTabOrder + 1;;

  if Assigned(Self.ActionList) then
  begin
    for vrVez := 0 to Self.ActionList.Count - 1 do
    begin
      Self.Internal_CreateAction(TJupiterAction(Self.ActionList.GetAtIndex(vrVez)), vrTabOrder);

      vrTabOrder := vrTabOrder + 1;;
    end;

    vrContainer        := TPanel.Create(Self.FContainer);
    vrContainer.Parent := Self.FContainer;
    vrContainer.Align  := alClient;

    if not Self.Raised then
      vrContainer.BevelOuter := bvNone
    else
      vrContainer.BevelOuter := bvRaised;
  end;
end;

procedure TJupiterFormGenerator.SetVariables(prVariables: TJupiterVariableFormList);
begin
  Self.FVariables := prVariables;

  if Self.ClearContainerOnSet then
  begin
    Self.FAlreadyDrawed := False;

    Self.Internal_ClearContainer;
  end;

  Self.Internal_DrawForm;
end;

procedure TJupiterFormGenerator.DrawForm;
begin
  if Self.FAlreadyDrawed then
    Exit;

  Self.Internal_DrawForm;
end;

constructor TJupiterFormGenerator.Create;
begin
  Self.FAlreadyDrawed := False;
  Self.FActionColor := clDefault;

  Self.FVariables := TJupiterVariableFormList.Create;
end;

destructor TJupiterFormGenerator.Destroy;
begin
  FreeAndNil(Self.FVariables);

  inherited Destroy;
end;

end.

