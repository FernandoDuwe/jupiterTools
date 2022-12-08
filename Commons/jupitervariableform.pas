unit JupiterVariableForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterConsts, JupiterVariable;

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
  end;

  { TJupiterVariableFormList }

  TJupiterVariableFormList = class(TJupiterVariableList)
  public
    function VariableFormById(prID : String) : TJupiterVariableForm;
    function VariableFormByIndex(prIndex : Integer) : TJupiterVariableForm;

    procedure AddField(prID, prTitle, prValue : String; prRequired : Boolean = True; prReadOnly : Boolean = False);

    constructor CreateFromVariableList(prList : TJupiterVariableList);
    procedure Validate; virtual;
  end;

implementation

{ TJupiterVariableForm }

function TJupiterVariableForm.HasValue: Boolean;
begin
  Result := Trim(Self.Value) <> EmptyStr;
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
  vrField          := TJupiterVariableForm.Create;
  vrField.Title    := prTitle;
  vrField.ID       := prID;
  vrField.Value    := prValue;
  vrField.Save     := False;
  vrField.ReadOnly := prReadOnly;
  vrField.Required := prRequired;

  Self.Add(vrField);
end;

constructor TJupiterVariableFormList.CreateFromVariableList(prList: TJupiterVariableList);
var
  vrVez  : Integer;
  vrForm : TJupiterVariableForm;
begin
  inherited Create;

  for vrVez := 0 to prList.Size - 1 do
  begin
    vrForm          := TJupiterVariableForm.Create;
    vrForm.Title    := prList.VariableByIndex(vrVez).Title;
    vrForm.ID       := prList.VariableByIndex(vrVez).ID;
    vrForm.Value    := prList.VariableByIndex(vrVez).Value;
    vrForm.Save     := prList.VariableByIndex(vrVez).Save;
    vrForm.ReadOnly := not prList.VariableByIndex(vrVez).Save;

    Self.Add(vrForm);
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

end.

