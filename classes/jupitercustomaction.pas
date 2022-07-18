unit JupiterCustomAction;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterConsts, jupiterUtils, SysUtils;

type

  { TJupiterCustomActionField }

  { TJupiterCustomActionAction }

  TJupiterCustomActionAction = class(TJupiterObject)
  private
    FTitle      : String;
    FHint       : String;
    FScriptFile : String;
    FImageIndex : Integer;
  published
    property Title      : String  read FTitle      write FTitle;
    property Hint       : String  read FHint       write FHint;
    property ImageIndex : Integer read FImageIndex write FImageIndex;
    property ScriptFile : String  read FScriptFile write FScriptFile;
  public
    function ObjectToStr : String; override;
    procedure StrToObject(prStr : String); override;
  end;

  TJupiterCustomActionField = class(TJupiterObject)
  private
    FTitle    : String;
    FHint     : String;
    FConfigID : String;
  published
    property Title    : String read FTitle    write FTitle;
    property Hint     : String read FHint     write FHint;
    property ConfigID : String read FConfigID write FConfigID;
  public
    function ObjectToStr : String; override;
    procedure StrToObject(prStr : String); override;
  end;

  { TJupiterCustomAction }

  TJupiterCustomAction = class(TJupiterAction)
  private
    FFileName : String;
    FList     : TList;
    FActions  : TList;

    function Internal_GetFieldCount : Integer;
    function Internal_GetActionCount : Integer;
  published
    property FileName : String read FFileName write FFileName;

    property FieldCount  : Integer read Internal_GetFieldCount;
    property ActionCount : Integer read Internal_GetActionCount;
  public
    function FieldByIndex(prIndex : Integer) : TJupiterCustomActionField;
    function ActionByIndex(prIndex : Integer) : TJupiterCustomActionAction;

    procedure AddField(prField : TJupiterCustomActionField);
    procedure AddAction(prAction : TJupiterCustomActionAction);

    procedure LoadFromFile;
    procedure SaveToFile;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TJupiterCustomActionAction }

function TJupiterCustomActionAction.ObjectToStr: String;
begin
  Result := Self.Internal_DoCrypt(StringReplace(Format('v1.0;%0:s;%1:s;%2:s;%3:d;', [Self.Title, Self.Hint, Self.ScriptFile, Self.ImageIndex]), ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]));
end;

procedure TJupiterCustomActionAction.StrToObject(prStr: String);
var
  vrText : String;
begin
  vrText := Self.Internal_DoDeCrypt(prStr);

  Self.Title      := GetCSVColumn(vrText, 1);
  Self.Hint       := GetCSVColumn(vrText, 2);
  Self.ScriptFile := GetCSVColumn(vrText, 3);
  Self.ImageIndex := StrToIntDef(GetCSVColumn(vrText, 4), -1);
end;

{ TJupiterCustomActionField }

function TJupiterCustomActionField.ObjectToStr: String;
begin
  Result := Self.Internal_DoCrypt(StringReplace(Format('v1.0;%0:s;%1:s;%2:s;', [Self.Title, Self.Hint, Self.ConfigID]), ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]));
end;

procedure TJupiterCustomActionField.StrToObject(prStr: String);
var
  vrText : String;
begin
  vrText := Self.Internal_DoDeCrypt(prStr);

  Self.Title    := GetCSVColumn(vrText, 1);
  Self.Hint     := GetCSVColumn(vrText, 2);
  Self.ConfigID := GetCSVColumn(vrText, 3);
end;

{ TJupiterCustomAction }

function TJupiterCustomAction.Internal_GetFieldCount: Integer;
begin
  Result := Self.FList.Count;
end;

function TJupiterCustomAction.Internal_GetActionCount: Integer;
begin
  Result := Self.FActions.Count;
end;

function TJupiterCustomAction.FieldByIndex(prIndex: Integer): TJupiterCustomActionField;
begin
  Result := TJupiterCustomActionField(Self.FList[prIndex]);
end;

function TJupiterCustomAction.ActionByIndex(prIndex: Integer): TJupiterCustomActionAction;
begin
  Result := TJupiterCustomActionAction(Self.FActions[prIndex]);
end;

procedure TJupiterCustomAction.AddField(prField: TJupiterCustomActionField);
begin
  Self.FList.Add(prField);
end;

procedure TJupiterCustomAction.AddAction(prAction: TJupiterCustomActionAction);
begin
  Self.FActions.Add(prAction);
end;

procedure TJupiterCustomAction.LoadFromFile;
var
  vrStr    : TStrings;
  vrVez    : Integer;
  vrField  : TJupiterCustomActionField;
  vrAction : TJupiterCustomActionAction;
begin
  if Trim(Self.FileName) = EmptyStr then
    raise Exception.Create('FileName must be informed.');

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.LoadFromFile(Self.FileName);

    for vrVez := 1 to vrStr.Count - 1 do
    begin
      if Trim(vrStr[vrVez]) = EmptyStr then
        Continue;

      if GetCSVColumn(vrStr[vrVez], 0) = Self.ClassName then
        Self.StrToObject(GetCSVColumn(vrStr[vrVez], 1));

      if GetCSVColumn(vrStr[vrVez], 0) = TJupiterCustomActionField.ClassName then
      begin
        vrField := TJupiterCustomActionField.Create;
        vrField.StrToObject(GetCSVColumn(vrStr[vrVez], 1));

        Self.FList.Add(vrField);
      end;

      if GetCSVColumn(vrStr[vrVez], 0) = TJupiterCustomActionAction.ClassName then
      begin
        vrAction := TJupiterCustomActionAction.Create;
        vrAction.StrToObject(GetCSVColumn(vrStr[vrVez], 1));

        Self.FActions.Add(vrAction);
      end;
    end;
  finally
    vrStr.Clear;
  end;
end;

procedure TJupiterCustomAction.SaveToFile;
var
  vrStr : TStrings;
  vrVez : Integer;
begin
  if Trim(Self.FileName) = EmptyStr then
    raise Exception.Create('FileName must be informed.');

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add('type;objectStr;');
    vrStr.Add(Self.ClassName + ';' + Self.ObjectToStr + ';');

    for vrVez := 0 to Self.FieldCount - 1 do
      vrStr.Add(TJupiterCustomActionField.ClassName + ';' + Self.FieldByIndex(vrVez).ObjectToStr + ';');

    for vrVez := 0 to Self.ActionCount - 1 do
      vrStr.Add(TJupiterCustomActionAction.ClassName + ';' + Self.ActionByIndex(vrVez).ObjectToStr + ';');

    vrStr.SaveToFile(Self.FileName);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

constructor TJupiterCustomAction.Create;
begin
  Self.FList := TList.Create;
  Self.FList.Clear;

  Self.FActions := TList.Create;
  Self.FActions.Clear;
end;

destructor TJupiterCustomAction.Destroy;
begin
  while Self.FList.Count > 0 do
    Self.FList.Delete(0);

  FreeAndNil(Self.FList);

  while Self.FActions.Count > 0 do
    Self.FActions.Delete(0);

  FreeAndNil(Self.FActions);

  inherited Destroy;
end;

end.

