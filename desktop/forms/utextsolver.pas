unit uTextSolver;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  SynEdit, uJupiterForm;

type

  { TFTextSolver }

  TFTextSolver = class(TFJupiterForm)
    mmText: TMemo;
    pcTabs: TPageControl;
    SynEdit1: TSynEdit;
    tsText: TTabSheet;
    tsCode: TTabSheet;
  private

  public

  end;

var
  FTextSolver: TFTextSolver;

implementation

{$R *.lfm}

end.

