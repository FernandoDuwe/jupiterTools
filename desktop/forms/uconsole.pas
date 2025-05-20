unit uconsole;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, SynEdit,
  SynHighlighterPas, SynCompletion, uJupiterForm;

type

  { TFConsole }

  TFConsole = class(TFJupiterForm)
    seScript: TSynEdit;
    SynAutoComplete1: TSynAutoComplete;
    SynCompletion1: TSynCompletion;
    SynFreePascalSyn1: TSynFreePascalSyn;
  private

  public

  end;

var
  FConsole: TFConsole;

implementation

{$R *.lfm}

end.

