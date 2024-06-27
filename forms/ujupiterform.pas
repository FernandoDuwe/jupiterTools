unit uJupiterForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, ExtCtrls;

type

  { TFJupiterForm }

  TFJupiterForm = class(TForm)
    acOptions: TActionList;
    tmrAutoUpdater: TTimer;
  private

  public

  end;

var
  FJupiterForm: TFJupiterForm;

implementation

{$R *.lfm}

end.

