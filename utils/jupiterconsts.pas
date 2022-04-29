unit JupiterConsts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  ICON_FOLDER : Smallint = 0;

  FORM_EXPLORER : Smallint = 0;

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

  TJupiterListableItem = class(TObject)
  public
    Item        : String;
    Descricao   : String;
    Param       : String;
    Selecionado : Boolean;
  end;

implementation

{ TJupiterListem }

constructor TJupiterListem.Create(prModule, prTask: String; prParams: String; prTag: Integer);
begin
  Self.Module := prModule;
  Self.Task   := prTask;
  Self.Params := prParams;
  Self.Tag    := prTag;
end;

end.

