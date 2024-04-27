unit uchartViewer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, TAGraph, TASources,
  TASeries, JupiterForm;

type

  { TFChartViewer }

  TFChartViewer = class(TFJupiterForm)
    chChart: TChart;
    lsChartSource: TListChartSource;
  private

  public

  end;

var
  FChartViewer: TFChartViewer;

implementation

{$R *.lfm}

end.

