unit uChartViewer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, TAGraph,
  uJupiterForm;

type

  { TFChartViewer }

  TFChartViewer = class(TFJupiterForm)
    chBody: TChart;
    pcBody: TPageControl;
    tsChart: TTabSheet;
  private

  public

  end;

var
  FChartViewer: TFChartViewer;

implementation

{$R *.lfm}

end.

