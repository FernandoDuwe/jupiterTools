unit uTextEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, SynEdit, uJupiterForm,
  JupiterConsts, uJupiterAction;

type

  { TFTextEditor }

  TFTextEditor = class(TFJupiterForm)
    seEditor: TSynEdit;
  private
    procedure Internal_PrepareForm; override;

    procedure Internal_OnSave(Sender: TObject);
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

  Self.ActionGroup.AddAction(TJupiterAction.Create('Salvar', 'Clique aqui para abrir salvar o arquivo', ICON_SAVE, @Internal_OnSave));

  Self.Caption := ExtractFileName(Self.Params.VariableById('path').Value);
  Self.Hint := Self.Params.VariableById('path').Value;

  seEditor.Lines.Clear;
  seEditor.Lines.LoadFromFile(Self.Params.VariableById('path').Value);
end;

procedure TFTextEditor.Internal_OnSave(Sender: TObject);
begin
  seEditor.Lines.SaveToFile(Self.Params.VariableById('path').Value);
end;

end.

