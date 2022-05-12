unit JupiterConsts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  ICON_FOLDER      : Smallint = 0;
  ICON_PACKAGE     : Smallint = 1;
  ICON_SCRIPTS     : Smallint = 4;
  ICON_FAVORITE    : Smallint = 6;
  ICON_CHECKED     : Smallint = 8;
  ICON_CURRENTTASK : Smallint = 10;

  FORM_EXPLORER : Smallint = 0;

  EMPTY_SPACE_SEPARATOR : String = '/JUPITERTOOLS\|/JUPITERTOOLS\';

type

  { TJupiterListem }

  TJupiterListem = class(TObject)
  public
    Module : String;
    Task   : String;
    Params : String;
    Tag    : Integer;

    constructor Create(prModule, prTask : String; prParams : String = ''; prTag : Integer = 0);
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

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TJupiterListableItem }

constructor TJupiterListableItem.Create;
begin
  Self.SubItens := TStringList.Create;
  Self.SubItens.Clear;
end;

destructor TJupiterListableItem.Destroy;
begin
  Self.SubItens.Clear;
  FreeAndNil(Self.SubItens);

  inherited Destroy;
end;

{ TJupiterListem }

constructor TJupiterListem.Create(prModule, prTask: String; prParams: String; prTag: Integer);
begin
  Self.Module := prModule;
  Self.Task   := prTask;
  Self.Params := prParams;
  Self.Tag    := prTag;
end;

end.

