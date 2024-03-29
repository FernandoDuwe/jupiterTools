unit JupiterConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, JupiterConsts, SysUtils;

type

  { TJupiterConfig }

  { TJupiterConfigItem }

  TJupiterConfigItem = class(TObject)
  private
    FID          : String;
    FDescription : String;
    FValue       : String;
    FCanSave     : Boolean;
  published
    property ID          : String  read FID;
    property Description : String  read FDescription;
    property Value       : String  read FValue write FValue;
    property CanSave     : Boolean read FCanSave;
  public
    constructor Create(prID, prDescription, prValue : String; prCanSave : Boolean = True);
  end;

  TJupiterConfig = class(TObject)
  private
    FList : TList;
  protected
    procedure Internal_ReadList;
    procedure Internal_WriteListFile;
  public
    function Exists(prID : String) : Boolean;
    procedure AddConfig(prID, prValue : String; prDescription : String = '');
    procedure AddVariable(prID, prValue : String; prDescription : String = '');
    function Count : Integer;
    function GetByIndex(prIndex : Integer) : TJupiterConfigItem;
    function GetByID(prID : String) : TJupiterConfigItem;
    function ResolveString(prStr : String) : String;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TJupiterConfigItem }

constructor TJupiterConfigItem.Create(prID, prDescription, prValue: String; prCanSave: Boolean);
begin
  Self.FID          := prID;
  Self.FDescription := prDescription;
  Self.FValue       := prValue;
  Self.FCanSave     := prCanSave;
end;

{ TJupiterConfig }

procedure TJupiterConfig.Internal_ReadList;
var
  vrStr    : TStrings;
  vrVez    : Integer;
  vrStrAux : TStrings;
begin
  if not FileExists(ExtractFileDir(Application.ExeName) + '/datasets/config.csv') then
     Exit;

  vrStr := TStringList.Create();
  try
    vrStr.LoadFromFile(ExtractFileDir(Application.ExeName) + '/datasets/config.csv');

    for vrVez := 1 to vrStr.Count - 1 do
    begin
      vrStrAux := TStringList.Create;
      vrStrAux.Delimiter     := ';';
      vrStrAux.DelimitedText := StringReplace(vrStr[vrVez], ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]);

      Self.FList.Add(TJupiterConfigItem.Create(StringReplace(vrStrAux[0], EMPTY_SPACE_SEPARATOR, ' ', [rfIgnoreCase, rfReplaceAll]),
                                               StringReplace(vrStrAux[1], EMPTY_SPACE_SEPARATOR, ' ', [rfIgnoreCase, rfReplaceAll]),
                                               StringReplace(vrStrAux[2], EMPTY_SPACE_SEPARATOR, ' ', [rfIgnoreCase, rfReplaceAll])));
    end;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterConfig.Internal_WriteListFile;
var
  vrStr : TStrings;
  vrVez : Integer;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add('ID;DESCRIPTION;VALUE;');

    for vrVez := 0 to Self.FList.Count - 1 do
      if (TJupiterConfigItem(Self.FList[vrVez]).CanSave) then
         vrStr.Add(Format('%0:s;%1:s;%2:s;', [TJupiterConfigItem(Self.FList[vrVez]).ID,
                                              TJupiterConfigItem(Self.FList[vrVez]).Description,
                                              TJupiterConfigItem(Self.FList[vrVez]).Value]));

    vrStr.SaveToFile(ExtractFileDir(Application.ExeName) + '/datasets/config.csv');
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

function TJupiterConfig.Exists(prID: String): Boolean;
var
  vrVez : Integer;
begin
  Result := False;

  for vrVez := 0 to Self.FList.Count - 1 do
      if TJupiterConfigItem(Self.FList[vrVez]).ID = prID then
      begin
        Result := True;
        Exit;
      end;
end;

procedure TJupiterConfig.AddConfig(prID, prValue: String; prDescription: String);
begin
  try
    if Self.Exists(prID) then
      Self.GetByID(prID).Value := prValue
    else
      Self.FList.Add(TJupiterConfigItem.Create(prID, prDescription, prValue));
  finally
    Self.Internal_WriteListFile;
  end;
end;

procedure TJupiterConfig.AddVariable(prID, prValue: String; prDescription: String);
begin
  if Self.Exists(prID) then
    Self.GetByID(prID).Value := prValue
  else
    Self.FList.Add( TJupiterConfigItem.Create(prID, prDescription, prValue, False));
end;

function TJupiterConfig.Count: Integer;
begin
  Result := Self.FList.Count;
end;

function TJupiterConfig.GetByIndex(prIndex : Integer): TJupiterConfigItem;
begin
  Result := TJupiterConfigItem(Self.FList[prIndex]);
end;

function TJupiterConfig.GetByID(prID: String): TJupiterConfigItem;
var
  vrVez : Integer;
begin
  Result := nil;

  for vrVez := 0 to Self.FList.Count - 1 do
      if TJupiterConfigItem(Self.FList[vrVez]).ID = prID then
      begin
        Result := TJupiterConfigItem(Self.FList[vrVez]);
        Exit;
      end;
end;

function TJupiterConfig.ResolveString(prStr: String): String;
var
  vrVez : Integer;
begin
  Result := prStr;

  for vrVez := 0 to Self.Count - 1 do
    Result := StringReplace(Result, '{' + Self.GetByIndex(vrVez).ID + '}', Self.GetByIndex(vrVez).Value, [rfIgnoreCase, rfReplaceAll]);
end;

constructor TJupiterConfig.Create;
begin
  Self.FList := TList.Create;

  if not DirectoryExists(ExtractFileDir(Application.ExeName) + '/datasets/') then
     CreateDir(ExtractFileDir(Application.ExeName) + '/datasets/');

  Self.Internal_ReadList;
end;

destructor TJupiterConfig.Destroy;
begin
  while Self.FList.Count > 0 do
  begin
    TJupiterConfigItem(Self.FList[0]).Free;
    Self.FList.Delete(0);
  end;

  FreeAndNil(Self.FList);

  inherited Destroy;
end;

end.

