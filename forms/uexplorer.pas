unit uExplorer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, uJupiterForm,
  JupiterConsts;

type

  { TFExplorer }

  TFExplorer = class(TJupiterForm)
    lvReport: TListView;
  private
    FParams : TJupiterListem;

    procedure Internal_UpdateDatasets; override;
  published
    property Params : TJupiterListem read FParams write FParams;
  end;

var
  FExplorer: TFExplorer;

implementation

{$R *.lfm}

{ TFExplorer }

procedure TFExplorer.Internal_UpdateDatasets;
begin
  inherited Internal_UpdateDatasets;


end;

end.

