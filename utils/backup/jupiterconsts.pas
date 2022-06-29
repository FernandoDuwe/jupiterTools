unit JupiterConsts;

{$mode ObjFPC}{$H+}

interface

uses
  Base64, Classes, SysUtils;

const
  ICON_FOLDER      : Smallint = 0;
  ICON_PACKAGE     : Smallint = 1;
  ICON_ADD         : Smallint = 2;
  ICON_SCRIPTS     : Smallint = 4;
  ICON_FAVORITE    : Smallint = 6;
  ICON_CHECKED     : Smallint = 8;
  ICON_UPDATE      : SmallInt = 11;
  ICON_CONFIG      : SmallInt = 12;
  ICON_CURRENTTASK : Smallint = 13;
  ICON_DOCS        : SmallInt = 14;
  ICON_SQL         : Smallint = 15;
  ICON_CHECKLIST   : Smallint = 17;

  FORM_EXPLORER : Smallint = 0;

  NULL_KEY : SmallInt = -1;

  EMPTY_SPACE_SEPARATOR : String = '/JUPITERTOOLS\|/JUPITERTOOLS\';
type

  { TJupiterListem }

  TJupiterListem = class(TObject)
  public
    Module : String;
    Task   : String;
    Params : String;
    Tag    : Integer;
    Hint   : String;

    constructor Create(prModule, prTask : String; prParams : String = ''; prTag : Integer = 0; prHint : String = '');

    function ObjectToStr : String;
    procedure StrToObject(prStr : String);
  end;

  { TJupiterListableItem }

  TJupiterListableItem = class(TObject)
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

  TJupiterAction = class(TObject)
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
  end;

implementation

{ TJupiterAction }

procedure TJupiterAction.Run(prParams: TJupiterListem);
begin
  //
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
  Self.Module := prModule;
  Self.Task   := prTask;
  Self.Params := prParams;
  Self.Tag    := prTag;
  Self.Hint   := prHint;
end;

function TJupiterListem.ObjectToStr: String;
begin
   Result := EncodeStringBase64(StringReplace(Format('v1.0;%0:s;%1:s;%2:s;%3:d;%4:s;', [Self.Module, Self.Task, Self.Params, Self.Tag, Self.Hint]), ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]));
end;

procedure TJupiterListem.StrToObject(prStr: String);
var
  vrStr : TStrings;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Delimiter := ';';
    vrStr.DelimitedText := DecodeStringBase64(StringReplace(prStr, ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]));

    Self.Module := StringReplace(vrStr[1], EMPTY_SPACE_SEPARATOR, ' ', [rfReplaceAll, rfIgnoreCase]);
    Self.Task   := StringReplace(vrStr[2], EMPTY_SPACE_SEPARATOR, ' ', [rfReplaceAll, rfIgnoreCase]);
    Self.Params := StringReplace(vrStr[3], EMPTY_SPACE_SEPARATOR, ' ', [rfReplaceAll, rfIgnoreCase]);
    Self.Tag    := StrToIntDef(StringReplace(vrStr[4], EMPTY_SPACE_SEPARATOR, ' ', [rfReplaceAll, rfIgnoreCase]), 0);
    Self.Hint   := StringReplace(vrStr[5], EMPTY_SPACE_SEPARATOR, ' ', [rfReplaceAll, rfIgnoreCase]);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

end.

