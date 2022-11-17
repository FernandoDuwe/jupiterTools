unit JupiterFormGenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, JupiterConsts, JupiterObject, JupiterVariable,
  JupiterFormField, JupiterVariableForm, SysUtils;

type

  { TJupiterFormGenerator }

  TJupiterFormGenerator = class(TJupiterObject)
  private
    FContainer           : TScrollBox;
    FVariables           : TJupiterVariableFormList;
    FClearContainerOnSet : Boolean;
    FLastTop             : Integer;

    procedure Internal_CreateComponent(prVariable : TJupiterVariableForm; prTabOrder : Integer);
    procedure Internal_ClearContainer;
    procedure Internal_DrawForm;
    procedure Internal_SetVariables(prVariables : TJupiterVariableFormList);
  published
    property ClearContainerOnSet : Boolean read FClearContainerOnSet write FClearContainerOnSet default False;

    property Container : TScrollBox               read FContainer write FContainer;
    property Variables : TJupiterVariableFormList read FVariables write Internal_SetVariables;
  end;

implementation

{ TJupiterFormGenerator }

procedure TJupiterFormGenerator.Internal_CreateComponent(prVariable: TJupiterVariableForm; prTabOrder : Integer);
var
  vrField : TJupiterFormField;
begin
  vrField := TJupiterFormField.Create;
  vrField.Variable := prVariable;
  vrField.TabOrder := prTabOrder;
  vrField.Draw(Self.Container);

  Self.FLastTop := vrField.Panel.Top + vrField.Panel.Height + 1;
end;

procedure TJupiterFormGenerator.Internal_ClearContainer;
begin
  while Self.Container.ControlCount > 0 do
    Self.Container.Controls[0].Free;
end;

procedure TJupiterFormGenerator.Internal_DrawForm;
var
  vrVez : Integer;
begin
  Self.FLastTop := 0;

  for vrVez := 0 to Self.Variables.Size - 1 do
  begin
    Self.Internal_CreateComponent(Self.Variables.VariableFormByIndex(vrVez), vrVez + 1);
  end;
end;

procedure TJupiterFormGenerator.Internal_SetVariables(prVariables: TJupiterVariableFormList);
begin
  Self.FVariables := prVariables;

  if Self.ClearContainerOnSet then
    Self.Internal_ClearContainer;

  Self.Internal_DrawForm;
end;

end.

