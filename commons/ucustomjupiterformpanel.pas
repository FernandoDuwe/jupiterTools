unit uCustomJupiterFormPanel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  uCustomJupiterForm;

type

  { TFCustomJupiterFormPanel }

  TFCustomJupiterFormPanel = class(TFCustomJupiterForm)
  private

  public
    procedure PrepareForm; override;
  end;

var
  FCustomJupiterFormPanel: TFCustomJupiterFormPanel;

implementation

{$R *.lfm}

{ TFCustomJupiterFormPanel }

procedure TFCustomJupiterFormPanel.PrepareForm;
begin
  Self.FFormGenerator.PanelMode := True;

  inherited PrepareForm;
end;

end.

