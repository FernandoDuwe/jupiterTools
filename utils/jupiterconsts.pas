unit JupiterConsts;

{$mode ObjFPC}{$H+}

interface

uses
  Base64, Classes, SysUtils, jupiterUtils;

const
  ICON_FOLDER      : Smallint = 0;
  ICON_PACKAGE     : Smallint = 1;
  ICON_ADD         : Smallint = 18;
  ICON_ADDFILE     : Smallint = 2;
  ICON_SCRIPTS     : Smallint = 4;
  ICON_FAVORITE    : Smallint = 6;
  ICON_CHECKED     : Smallint = 8;
  ICON_UPDATE      : SmallInt = 11;
  ICON_CONFIG      : SmallInt = 12;
  ICON_CURRENTTASK : Smallint = 13;
  ICON_DOCS        : SmallInt = 14;
  ICON_SQL         : Smallint = 15;
  ICON_CHECKLIST   : Smallint = 17;
  ICON_WIZARD      : Smallint = 20;
  ICON_FORM        : SmallInt = 21;

  FORM_EXPLORER : Smallint = 0;

  NULL_KEY : SmallInt = -1;

  EMPTY_SPACE_SEPARATOR : String = '/JUPITERTOOLS\|/JUPITERTOOLS\';

type
  TJupiterHintType = (htNone, htSuccess, htError);

  { TJupiterListem }

  { TJupiterObject }

  TJupiterObject = class(TObject)
  protected
    function Internal_DoCrypt(prValue : String) : String;
    function Internal_DoDeCrypt(prValue : String) : String;
  public
    function ObjectToStr : String; virtual;
    procedure StrToObject(prStr : String); virtual;
  end;

  TJupiterListem = class(TJupiterObject)
  public
    Module   : String;
    Task     : String;
    Params   : String;
    Tag      : Integer;
    Hint     : String;
    HintType : TJupiterHintType;

    constructor Create(prModule, prTask : String; prParams : String = ''; prTag : Integer = 0; prHint : String = '');

    procedure CopyFrom(prParams : TJupiterListem);

    function ObjectToStr : String; override;
    procedure StrToObject(prStr : String); override;
  end;

  { TJupiterListableItem }

  TJupiterListableItem = class(TJupiterObject)
  public
    Item        : String;
    Descricao   : String;
    Param       : String;
    Selecionado : Boolean;
    Tag         : Integer;
    SubItens    : TStrings;
    ImageIndex  : Integer;

    constructor Create;
    destructor Destroy; override;
  end;

  { TJupiterAction }

  TJupiterAction = class(TJupiterObject)
  private
    FImageIndex : Integer;
    FTitle : String;
    FHint  : String;
  published
    property ImageIndex : Integer read FImageIndex write FImageIndex;
    property Title      : String  read FTitle      write FTitle;
    property Hint       : String  read FHint       write FHint;
  public
    procedure Run(prParams : TJupiterListem); virtual;

    function ObjectToStr : String; override;
    procedure StrToObject(prStr : String); override;
  end;

implementation

{ TJupiterObject }

function TJupiterObject.Internal_DoCrypt(prValue: String): String;
begin
  Result := EncodeStringBase64(prValue);
end;

function TJupiterObject.Internal_DoDeCrypt(prValue: String): String;
begin
  Result := DecodeStringBase64(prValue);
end;

function TJupiterObject.ObjectToStr: String;
begin
  Result := EmptyStr;
end;

procedure TJupiterObject.StrToObject(prStr: String);
begin
  // Implement here
end;

{ TJupiterAction }

procedure TJupiterAction.Run(prParams: TJupiterListem);
begin
  // Implement here
end;

function TJupiterAction.ObjectToStr: String;
begin
  Result := Self.Internal_DoCrypt(StringReplace(Format('v1.0;%0:d;%1:s;%2:s;', [Self.ImageIndex, Self.Title, Self.Hint]), ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]));
end;

procedure TJupiterAction.StrToObject(prStr: String);
var
  vrText : String;
begin
  vrText := Self.Internal_DoDeCrypt(prStr);

  Self.ImageIndex := StrToIntDef(GetCSVColumn(vrText, 1), 0);
  Self.Title      := GetCSVColumn(vrText, 2);
  Self.Hint       := GetCSVColumn(vrText, 3);
end;

{ TJupiterListableItem }

constructor TJupiterListableItem.Create;
begin
  Self.SubItens := TStringList.Create;
  Self.SubItens.Clear;

  Self.ImageIndex := NULL_KEY;
end;

destructor TJupiterListableItem.Destroy;
begin
  Self.SubItens.Clear;
  FreeAndNil(Self.SubItens);

  inherited Destroy;
end;

{ TJupiterListem }

constructor TJupiterListem.Create(prModule, prTask: String; prParams: String; prTag: Integer; prHint : String = '');
begin
  Self.Module   := prModule;
  Self.Task     := prTask;
  Self.Params   := prParams;
  Self.Tag      := prTag;
  Self.Hint     := prHint;
  Self.HintType := htNone;
end;

procedure TJupiterListem.CopyFrom(prParams: TJupiterListem);
begin
  Module   := prParams.Module;
  Task     := prParams.Task;
  Params   := prParams.Params;
  Tag      := prParams.Tag;
  Hint     := prParams.Hint;
  HintType := prParams.HintType;
end;

function TJupiterListem.ObjectToStr: String;
begin
   Result := Self.Internal_DoCrypt(StringReplace(Format('v1.0;%0:s;%1:s;%2:s;%3:d;%4:s;', [Self.Module, Self.Task, Self.Params, Self.Tag, Self.Hint]), ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]));
end;

procedure TJupiterListem.StrToObject(prStr: String);
var
  vrText : String;
begin
  vrText := Self.Internal_DoDeCrypt(prStr);

  Self.Module := GetCSVColumn(vrText, 1);
  Self.Task   := GetCSVColumn(vrText, 2);
  Self.Params := GetCSVColumn(vrText, 3);
  Self.Tag    := StrToIntDef(GetCSVColumn(vrText, 4), 0);
  Self.Hint   := GetCSVColumn(vrText, 5);
end;

end.

