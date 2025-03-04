unit uTextEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, SynEdit, uJupiterForm,
  JupiterConsts, JupiterEnviroment, uJupiterAction;

type

  { TFTextEditor }

  TFTextEditor = class(TFJupiterForm)
    seEditor: TSynEdit;
  private
    procedure Internal_PrepareForm; override;

    procedure Internal_OnSave(Sender: TObject);
    procedure Internal_SetHighligther;
  public

  end;

var
  FTextEditor: TFTextEditor;

implementation

uses SynHighLighterPas, SynHighLighterCpp, SynHighLighterJScript, SynHighLighterSQL, SynHighLighterBat;

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

  Self.Internal_SetHighligther;
end;

procedure TFTextEditor.Internal_OnSave(Sender: TObject);
begin
  seEditor.Lines.SaveToFile(Self.Params.VariableById('path').Value);
end;

procedure TFTextEditor.Internal_SetHighligther;
var
  vrExtension : String;
begin
  vrExtension := AnsiUpperCase(ExtractFileExt(Self.Params.VariableById('path').Value));

  if ((vrExtension = '.PAS') or (vrExtension = '.JPAS')) then
    seEditor.Highlighter := TSynPasSyn.Create(seEditor);

  if (vrExtension = '.CS') then
    seEditor.Highlighter := TSynCppSyn.Create(seEditor);

  if (vrExtension = '.JS') then
    seEditor.Highlighter := TSynJScriptSyn.Create(seEditor);

  if (vrExtension = '.SQL') then
    seEditor.Highlighter := TSynSQLSyn.Create(seEditor);

  if (vrExtension = '.BAT') then
    seEditor.Highlighter := TSynBatSyn.Create(seEditor);
end;

end.

