unit JupiterVariable;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterObject, JupiterEnviroment, JupiterConsts, SysUtils;

type

  { TJupiterVariable }

  TJupiterVariableChangeValue = procedure(prID, prNewValue : String) of object;

  TJupiterVariable = class(TJupiterObject)
  private
    FID            : String;
    FValue         : String;
    FTitle         : String;
    FSave          : Boolean;
    FOwner         : TJupiterObject;
    FOnChangeValue : TJupiterVariableChangeValue;

    procedure Internal_SetValue(prNewValue : String);
  published
    property ID    : String         read FID    write FID;
    property Value : String         read FValue write Internal_SetValue;
    property Title : String         read FTitle write FTitle;
    property Save  : Boolean        read FSave  write FSave;
    property Owner : TJupiterObject read FOwner write FOwner;

    property OnChangeValue : TJupiterVariableChangeValue read FOnChangeValue write FOnChangeValue;
  public
    procedure SaveConfig;
  end;

  { TJupiterVariableList }

  TJupiterVariableList = class(TJupiterObjectList)
  private
    FChildList : TJupiterObjectList;
    FFileName : String;
    FReadingFile : Boolean;

    procedure Internal_GetFileVariables(prCompleteFileName : String);
    function  Internal_VariableCount : Integer;
    procedure Internal_SetFileName(prFileName : String);
  published
    property ChildList     : TJupiterObjectList read FChildList write FChildList;
    property FileName      : String             read FFileName write Internal_SetFileName;
    property VariableCount : Integer            read Internal_VariableCount;
  public
    procedure AddChildList(prList : TJupiterVariableList);

    procedure AddConfig(prID : String; prValue : String; prTitle : String = '');
    procedure AddVariable(prID : String; prValue : String; prTitle : String = '');

    function Exists(prID : String) : Boolean;
    function VariableById(prID : String) : TJupiterVariable;
    function VariableIndexById(prID : String) : Integer;
    function VariableByIndex(prIndex : Integer) : TJupiterVariable;
    function ResolveString(prStr : String) : String;
    procedure ResolveFile(prFile : String);
    procedure DeleteVariable(prID : String);

    procedure CopyValues(prList : TJupiterVariableList);
    procedure SaveToFile;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses JupiterCSVDataProvider;

{ TJupiterVariable }

procedure TJupiterVariable.Internal_SetValue(prNewValue: String);
begin
  Self.FValue := prNewValue;

  if Assigned(Self.OnChangeValue) then
    Self.OnChangeValue(Self.ID, Self.Value);
end;

procedure TJupiterVariable.SaveConfig;
begin
  if not Self.Save then
    Exit;

  if not Assigned(Self.Owner) then
    Exit;

  TJupiterVariableList(Self.Owner).AddConfig(Self.ID, Self.Value, Self.Title);
end;

{ TJupiterVariableList }

procedure TJupiterVariableList.Internal_GetFileVariables(prCompleteFileName : String);
var
  vrProvider : TJupiterCSVDataProvider;
  vrVez : Integer;
begin
  Self.FReadingFile := True;

  vrProvider := TJupiterCSVDataProvider.Create;
  try
    vrProvider.Filename := prCompleteFileName;
    vrProvider.ProvideData;

    for vrVez := 0 to vrProvider.Size - 1 do
      with vrProvider.GetRowByIndex(vrVez) do
        Self.AddConfig(Fields.VariableById('ID').Value,
                       Fields.VariableById('VALUE').Value,
                       Fields.VariableById('DESCRIPTION').Value);
  finally
    FreeAndNil(vrProvider);

    Self.FReadingFile := False;
  end;
end;

function TJupiterVariableList.Internal_VariableCount: Integer;
begin
  Result := Self.Size;
end;

procedure TJupiterVariableList.Internal_SetFileName(prFileName: String);
var
  vrEnviroment : TJupiterEnviroment;
begin
  Self.FFileName := prFileName;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    if FileExists(vrEnviroment.FullPath(prFileName)) then
      Self.Internal_GetFileVariables(vrEnviroment.FullPath(prFileName));
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterVariableList.AddChildList(prList: TJupiterVariableList);
begin
  Self.ChildList.Add(prList);
end;

procedure TJupiterVariableList.AddConfig(prID: String; prValue: String; prTitle : String = '');
var
  vrObj : TJupiterVariable;
begin
  try
    if Assigned(Self.VariableById(prID)) then
    begin
      with Self.VariableById(prID) do
      begin
        Owner := Self;
        Title := prTitle;
        Value := prValue;
        Save  := True;
      end;
    end
    else
    begin
      vrObj := TJupiterVariable.Create;
      vrObj.ID    := prID;
      vrObj.Value := prValue;
      vrObj.Title := prTitle;
      vrObj.Save  := True;
      vrObj.Owner := Self;

      Self.Add(vrObj);
    end;
  finally
    if not Self.FReadingFile then
      Self.SaveToFile;
  end;
end;

procedure TJupiterVariableList.AddVariable(prID: String; prValue: String; prTitle : String = '');
var
  vrObj : TJupiterVariable;
begin
  if Assigned(Self.VariableById(prID)) then
  begin
    with Self.VariableById(prID) do
    begin
      Title := prTitle;
      Value := prValue;
      Save  := False;
      Owner := Self;
    end;
  end
  else
  begin
    vrObj := TJupiterVariable.Create;
    vrObj.ID    := prID;
    vrObj.Value := prValue;
    vrObj.Title := prTitle;
    vrObj.Save  := False;
    vrObj.Owner := Self;

    Self.Add(vrObj);
  end;
end;

function TJupiterVariableList.Exists(prID: String): Boolean;
begin
  Result := Self.VariableById(prID) <> nil;
end;

function TJupiterVariableList.VariableById(prID: String): TJupiterVariable;
var
  vrVez : Integer;
  vrVezModule : Integer;
begin
  Result := nil;

  for vrVez := 0 to Self.VariableCount - 1 do
    if AnsiUpperCase(Self.VariableByIndex(vrVez).ID) = AnsiUpperCase(prID) then
    begin
      Result := Self.VariableByIndex(vrVez);
      Exit;
    end;

  for vrVezModule := 0 to Self.ChildList.Size -1 do
  begin
    Result := TJupiterVariableList(Self.ChildList.GetAtIndex(vrVezModule)).VariableById(prID);

    if Assigned(Result) then
       Exit;
  end;
end;

function TJupiterVariableList.VariableIndexById(prID: String): Integer;
var
  vrVez : Integer;
  vrVezModule : Integer;
begin
  Result := NULL_KEY;

  for vrVez := 0 to Self.VariableCount - 1 do
    if Self.VariableByIndex(vrVez).ID = prID then
    begin
      Result := vrVez;
      Exit;
    end;
end;

function TJupiterVariableList.VariableByIndex(prIndex: Integer): TJupiterVariable;
begin
  Result := TJupiterVariable(Self.GetAtIndex(prIndex));
end;

function TJupiterVariableList.ResolveString(prStr: String): String;
var
  vrVez : Integer;
  vrVezModule : Integer;
begin
  Result := prStr;

  for vrVez := 0 to Self.Size - 1 do
    Result := StringReplace(Result, '{' + Self.VariableByIndex(vrVez).ID + '}', Self.VariableByIndex(vrVez).Value, [rfIgnoreCase, rfReplaceAll]);

  for vrVezModule := 0 to Self.ChildList.Size -1 do
    Result := TJupiterVariableList(Self.ChildList.GetAtIndex(vrVezModule)).ResolveString(Result);
end;

procedure TJupiterVariableList.ResolveFile(prFile: String);
var
  vrStr : TStrings;
  vrVez : Integer;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.LoadFromFile(prFile);

    for vrVez := 0 to vrStr.Count - 1 do
      vrStr[vrVez] := Self.ResolveString(vrStr[vrVez]);

    vrStr.SaveToFile(prFile);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterVariableList.DeleteVariable(prID: String);
var
  vrIndex : Integer;
  vrObj   : TJupiterVariable;
begin
  vrIndex := Self.VariableIndexById(prID);
  vrObj   := Self.VariableByIndex(vrIndex);

  FreeAndNil(vrObj);

  Self.FList.Delete(vrIndex);
end;

procedure TJupiterVariableList.CopyValues(prList: TJupiterVariableList);
var
  vrVez : Integer;
begin
  for vrVez := 0 to prList.Size - 1 do
    with prList.VariableByIndex(vrVez) do
    begin
      if Save then
        Self.AddConfig(ID, Value, Title)
      else
        Self.AddVariable(ID, Value, Title);
    end;
end;

procedure TJupiterVariableList.SaveToFile;
var
  vrStr        : TStringList;
  vrEnviroment : TJupiterEnviroment;
  vrVez        : Integer;
  vrStrLine    : String;
begin
  if Self.FileName = EmptyStr then
    Exit;

  vrEnviroment := TJupiterEnviroment.Create;
  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add('ID;DESCRIPTION;VALUE;');

    for vrVez := 0 to Self.Size - 1 do
    begin
      if not Self.VariableByIndex(vrVez).Save then
        Continue;

      vrStrLine := Format('%0:s;%1:s;%2:s;', [StringReplace(Self.VariableByIndex(vrVez).ID, ';', ',', [rfIgnoreCase, rfReplaceAll]),
                                              StringReplace(Self.VariableByIndex(vrVez).Title, ';', ',', [rfIgnoreCase, rfReplaceAll]),
                                              StringReplace(Self.VariableByIndex(vrVez).Value, ';', ',', [rfIgnoreCase, rfReplaceAll])]);

      vrStrLine := StringReplace(vrStrLine, #13, EmptyStr, [rfReplaceAll, rfIgnoreCase]);
      vrStrLine := StringReplace(vrStrLine, #10, EmptyStr, [rfReplaceAll, rfIgnoreCase]);

      vrStr.Add(vrStrLine);
    end;

    vrStr.SaveToFile(vrEnviroment.FullPath(Self.FileName));
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);

    FreeAndNil(vrEnviroment);
  end;
end;

constructor TJupiterVariableList.Create;
begin
  inherited Create;

  Self.FReadingFile := False;

  Self.FFileName  := EmptyStr;
  Self.FChildList := TJupiterObjectList.Create;
end;

destructor TJupiterVariableList.Destroy;
begin
  FreeAndNil(Self.FChildList);

  inherited Destroy;
end;

end.

