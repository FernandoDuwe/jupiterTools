unit uRichEditUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RichMemo, Graphics;

  procedure RichMemoUtils_AddBoldLine(prLine : String; var prRichMemo : TRichMemo);
  procedure RichMemoUtils_AddItalicLine(prLine : String; var prRichMemo : TRichMemo);
  procedure RichMemoUtils_AddLine(prLine : String; var prRichMemo : TRichMemo);

implementation

procedure RichMemoUtils_AddBoldLine(prLine: String; var prRichMemo: TRichMemo);
var
  vrStart  : Integer;
  vrParams : TFontParams;
begin
  vrStart := prRichMemo.GetTextLen;

  prRichMemo.GetTextAttributes(prRichMemo.SelStart, vrParams);

  prRichMemo.Lines.Add(prLine);

  vrParams.Style := [fsBold];

  prRichMemo.SetTextAttributes(vrStart, Length(prLine), vrParams);
end;

procedure RichMemoUtils_AddItalicLine(prLine: String; var prRichMemo: TRichMemo);
var
  vrStart  : Integer;
  vrParams : TFontParams;
begin
  vrStart := prRichMemo.GetTextLen;

  prRichMemo.GetTextAttributes(prRichMemo.SelStart, vrParams);

  prRichMemo.Lines.Add(prLine);

  vrParams.Style := [fsItalic];

  prRichMemo.SetTextAttributes(vrStart, Length(prLine), vrParams);
end;

procedure RichMemoUtils_AddLine(prLine: String; var prRichMemo: TRichMemo);
var
  vrStart  : Integer;
  vrParams : TFontParams;
begin
  vrStart := prRichMemo.GetTextLen;

  prRichMemo.GetTextAttributes(prRichMemo.SelStart, vrParams);

  prRichMemo.Lines.Add(prLine);

  vrParams.Style := [];

  prRichMemo.SetTextAttributes(vrStart, Length(prLine), vrParams);
end;

end.

