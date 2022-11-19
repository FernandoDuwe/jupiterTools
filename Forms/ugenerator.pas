unit uGenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  uCustomJupiterForm;

type

  { TFGenerator }

  TFGenerator = class(TFCustomJupiterForm)
    mmLines: TMemo;
    pcTabs: TPageControl;
    tsForms: TTabSheet;
    tsInicio: TTabSheet;
  private
    procedure Internal_PrepareForm; override;
  public

  end;

var
  FGenerator: TFGenerator;

implementation

{$R *.lfm}

{ TFGenerator }

procedure TFGenerator.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;
end;

end.

