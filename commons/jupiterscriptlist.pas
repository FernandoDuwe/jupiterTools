unit jupiterScriptList;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, JupiterConsts, JupiterVariable;

type

  { TJupiterScriptInstruction }

  TJupiterScriptInstruction = class(TJupiterObject)
  private
    FComand : String;
  published
    property Comand : String read FComand write FComand;
  public
    constructor Create(prComand : String); virtual;
  end;

  { TJupiterScriptList }

  TJupiterScriptList = class(TJupiterObjectList)
  public
    procedure AddInstruction(prComand : String);

    procedure ExecuteNext;
  end;

implementation

uses JupiterApp;

{ TJupiterScriptInstruction }

constructor TJupiterScriptInstruction.Create(prComand: String);
begin
  Self.FComand := prComand;
end;

{ TJupiterScriptList }

procedure TJupiterScriptList.AddInstruction(prComand: String);
begin
  Self.Add(TJupiterScriptInstruction.Create(prComand));
end;

procedure TJupiterScriptList.ExecuteNext;
begin
  if Self.IsEmpty then
    Exit;

  vrJupiterApp.RunScript(CreateStringListToMacro(TJupiterScriptInstruction(Self.GetAtIndex(0)).Comand), TJupiterVariableList.Create);

  Self.DeleteAtIndex(0);
end;

end.

