unit uGenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  DBGrids, uJupiterForm, JupiterApp;

type

  { TFGenerator }

  TFGenerator = class(TFJupiterForm)
    dbGridRoutes: TDBGrid;
    dbGridMacros: TDBGrid;
    mmDetails: TMemo;
    pcOptions: TPageControl;
    tsMacros: TTabSheet;
    tsRoutes: TTabSheet;
    tsGenerator: TTabSheet;
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

  Self.ShowSearchBar := True;

  mmDetails.Lines.Clear;
  mmDetails.Lines.Add('Aplicação');
  mmDetails.Lines.Add(vrJupiterApp.AppID + ' - ' + vrJupiterApp.AppName);
end;

end.

