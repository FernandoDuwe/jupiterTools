unit JupiterVariableForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterConsts, JupiterVariable, JupiterEnviroment;

type

  { TJupiterVariableForm }

  TJupiterVariableForm = class(TJupiterVariable)
  private
    FCleanOnShow   : Boolean;
    FRequired      : Boolean;
    FReadOnly      : Boolean;
    FCopyButton    : Boolean;
    FRunButton     : Boolean;

    FComponentType : String;
    FListVariable  : String;

  published
    property CleanOnShow   : Boolean read FCleanOnShow   write FCleanOnShow default False;
    property CopyButton    : Boolean read FCopyButton    write FCopyButton  default False;
    property ComponentType : String  read FComponentType write FComponentType;
    property ListVariable  : String  read FListVariable  write FListVariable;
    property Required      : Boolean read FRequired      write FRequired  default False;
    property ReadOnly      : Boolean read FReadOnly      write FReadOnly  default False;
    property RunButton     : Boolean read FRunButton     write FRunButton default False;
  public
    function HasValue : Boolean;

    constructor CreateFromVariable(prVariable : TJupiterVariable);
  end;

  { TJupiterVariableFormList }

  TJupiterVariableFormList = class(TJupiterVariableList)
  public
    function VariableFormById(prID : String) : TJupiterVariableForm;
    function VariableFormByIndex(prIndex : Integer) : TJupiterVariableForm;

    procedure AddField(prID, prTitle, prValue : String; prRequired : Boolean = True; prReadOnly : Boolean = False);

    constructor CreateFromVariableList(prList : TJupiterVariableList);
    procedure CopyFromVariableList(prList : TJupiterVariableList);
    procedure Validate; virtual;
    procedure Save; virtual;
  end;

implementation

uses JupiterApp;

{ TJupiterVariableForm }

function TJupiterVariableForm.HasValue: Boolean;
begin
  Result := Trim(Self.Value) <> EmptyStr;
end;

constructor TJupiterVariableForm.CreateFromVariable(prVariable: TJupiterVariable);
begin
  inherited Create;

  Self.ID    := prVariable.ID;
  Self.Value := prVariable.Value;
  Self.Title := prVariable.Title;
  Self.Save  := prVariable.Save;

  Self.ReadOnly      := False;
  Self.Required      := False;

  Self.ComponentType := FIELD_TYPE_EDIT;
  Self.CleanOnShow   := False;
  Self.RunButton     := False;
  Self.CopyButton    := True;

  Self.OnGetValue := prVariable.OnGetValue;
end;

{ TJupiterVariableFormList }

function TJupiterVariableFormList.VariableFormById(prID: String): TJupiterVariableForm;
var
  vrVez : Integer;
  vrVezModule : Integer;
begin
  Result := nil;

  for vrVez := 0 to Self.VariableCount - 1 do
    if Self.VariableFormByIndex(vrVez).ID = prID then
    begin
      Result := Self.VariableFormByIndex(vrVez);
      Exit;
    end;
end;

function TJupiterVariableFormList.VariableFormByIndex(prIndex: Integer): TJupiterVariableForm;
begin
  Result := TJupiterVariableForm(Self.GetAtIndex(prIndex));
end;

procedure TJupiterVariableFormList.AddField(prID, prTitle, prValue: String; prRequired: Boolean; prReadOnly: Boolean);
var
  vrField : TJupiterVariableForm;
begin
  vrField               := TJupiterVariableForm.Create;
  vrField.Title         := prTitle;
  vrField.ID            := prID;
  vrField.Value         := prValue;
  vrField.Save          := False;
  vrField.ReadOnly      := prReadOnly;
  vrField.Required      := prRequired;
  vrField.ComponentType := FIELD_TYPE_EDIT;

  Self.Add(vrField);
end;

constructor TJupiterVariableFormList.CreateFromVariableList(prList: TJupiterVariableList);
begin
  inherited Create;

  Self.CopyFromVariableList(prList);
end;

procedure TJupiterVariableFormList.CopyFromVariableList(prList: TJupiterVariableList);
var
  vrVez  : Integer;
  vrForm : TJupiterVariableForm;
  vrCurrentVariable : TJupiterVariableForm;
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    for vrVez := 0 to prList.Size - 1 do
    begin
      if prList.VariableByIndex(vrVez) is TJupiterVariableForm then
        vrCurrentVariable := TJupiterVariableForm(prList.VariableByIndex(vrVez))
      else
        vrCurrentVariable := TJupiterVariableForm.CreateFromVariable(prList.VariableByIndex(vrVez));

      vrForm          := TJupiterVariableForm.Create;
      vrForm.Title    := vrCurrentVariable.Title;
      vrForm.ID       := vrCurrentVariable.ID;
      vrForm.Value    := vrCurrentVariable.Value;
      vrForm.Save     := vrCurrentVariable.Save;
      vrForm.ReadOnly := vrCurrentVariable.ReadOnly;
      vrForm.Required := vrCurrentVariable.Required;

      vrForm.ComponentType := vrCurrentVariable.ComponentType;
      vrForm.CleanOnShow   := vrCurrentVariable.CleanOnShow;
      vrForm.RunButton     := vrCurrentVariable.RunButton;
      vrForm.CopyButton    := vrCurrentVariable.CopyButton;
      vrForm.ListVariable  := vrCurrentVariable.ListVariable;

      if vrJupiterApp.Params.Exists(vrCurrentVariable.ID) then
        if vrJupiterApp.Params.VariableById(vrCurrentVariable.ID).AutoGenerated then
          vrForm.OnGetValue := vrJupiterApp.Params.VariableById(vrCurrentVariable.ID).OnGetValue;

      if ((not vrCurrentVariable.RunButton) and (vrEnviroment.Exists(vrCurrentVariable.Value))) then
        vrForm.RunButton := True;

      Self.Add(vrForm);
    end;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterVariableFormList.Validate;
var
  vrVez : Integer;
begin
  for vrVez := 0 to Self.Size - 1 do
    with Self.VariableFormByIndex(vrVez) do
      if ((Required) and (not HasValue)) then
        raise Exception.Create(Format('O campo %0:s é obrigatório.', [Title]));
end;

procedure TJupiterVariableFormList.Save;
var
  vrVez : Integer;
begin
  for vrVez := 0 to Self.Size - 1 do
  begin
    if not vrJupiterApp.Params.Exists(Self.VariableByIndex(vrVez).ID) then
      vrJupiterApp.Params.AddVariable(Self.VariableByIndex(vrVez).ID, Self.VariableByIndex(vrVez).Value, Self.VariableByIndex(vrVez).Title)
    else
      vrJupiterApp.Params.VariableById(Self.VariableByIndex(vrVez).ID).Value := Self.VariableByIndex(vrVez).Value;
  end;
end;

end.

