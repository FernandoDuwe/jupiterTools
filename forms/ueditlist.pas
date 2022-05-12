unit uEditList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  uJupiterForm;

type

  { TFEditList }

  TFEditList = class(TJupiterForm)
    mmList: TMemo;
    StatusBar1: TStatusBar;
  private
    function  Internal_ReadList: String;
    procedure Internal_WriteList(prStr : String);
  published
    property List : String read Internal_ReadList write Internal_WriteList;
  end;

var
  FEditList: TFEditList;

implementation

{$R *.lfm}

{ TFEditList }

function TFEditList.Internal_ReadList: String;
var
  vrVez : Integer;
begin
  Result := EmptyStr;

  for vrVez := 0 to mmList.Lines.Count - 1 do
  begin
    if vrVez > 0 then
      Result := Result + '|';

    Result := Result + mmList.Lines[vrVez];
  end;
end;

procedure TFEditList.Internal_WriteList(prStr: String);
var
  vrStr : TStrings;
begin
  mmList.Lines.Clear;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Delimiter     := '|';
    vrStr.DelimitedText := prStr;

    mmList.Lines.AddStrings(vrStr);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

end.

