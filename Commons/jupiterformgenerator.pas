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
    FAlreadyDrawed       : Boolean;

    procedure Internal_CreateComponent(prVariable : TJupiterVariableForm; prTabOrder : Integer);
    procedure Internal_ClearContainer;
    procedure Internal_DrawForm;
  published
    property ClearContainerOnSet : Boolean read FClearContainerOnSet write FClearContainerOnSet default False;

    property Container : TScrollBox               read FContainer write FContainer;
    property Variables : TJupiterVariableFormList read FVariables write FVariables;
  public
    procedure DrawForm;

    procedure SetVariables(prVariables : TJupiterVariableFormList);

    constructor Create;
    destructor Destroy; override;
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
  vrField.Panel.Top := Self.FLastTop;

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

  Self.FVariables := TJupiterVariableFormList.Create;
end;

destructor TJupiterFormGenerator.Destroy;
begin
  FreeAndNil(Self.FVariables);

  inherited Destroy;
end;

end.

