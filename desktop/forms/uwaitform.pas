unit uWaitForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, uJupiterForm;

type

  { TFWaitForm }

  TFWaitForm = class(TFJupiterForm)
    Image2: TImage;
    pnBottom1: TPanel;
  private

  public

  end;

var
  FWaitForm: TFWaitForm;

implementation

{$R *.lfm}

end.

