unit uTextEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, SynEdit, uJupiterForm;

type

  { TFTextEditor }

  TFTextEditor = class(TFJupiterForm)
    seEditor: TSynEdit;
  private
    procedure Internal_PrepareForm; override;
  public

  end;

var
  FTextEditor: TFTextEditor;

implementation

{$R *.lfm}

{ TFTextEditor }

procedure TFTextEditor.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  seEditor.Lines.Clear;
  seEditor.Lines.Add(Self.Params.VariableById('path').Value);
end;

end.

