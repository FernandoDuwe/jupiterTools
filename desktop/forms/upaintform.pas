unit uPaintForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, uJupiterForm,
  FPCanvas, LCLIntf, LCLType;

type

  { TFPaintForm }

  TFPaintForm = class(TFJupiterForm)
    pbImage: TPaintBox;
  private
    procedure Internal_PrepareForm; override;
  public
    procedure DrawLine(prX, prY : Integer);
    procedure DrawLineTo(prX1, prX2, prY1, prY2 : Integer);
  end;

var
  FPaintForm: TFPaintForm;

implementation

{$R *.lfm}

{ TFPaintForm }

procedure TFPaintForm.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  SetBkMode(pbImage.Canvas.Handle, TRANSPARENT);
  SetBkColor(pbImage.Canvas.Handle, clWhite);
end;

procedure TFPaintForm.DrawLine(prX, prY : Integer);
begin
  pbImage.Canvas.LineTo(prX, prY);
end;

procedure TFPaintForm.DrawLineTo(prX1, prX2, prY1, prY2: Integer);
begin
  pbImage.Canvas.Line(prX1, prY1, prX2, prY2);
end;

end.

