unit uTextEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, SynEdit,
  SynHighlighterMulti, SynHighlighterAny, uJupiterForm, JupiterConsts,
  JupiterEnviroment, JupiterModule, jupiterDatabaseWizard, JupiterApp,
  jupiterformutils, uJupiterStringUtilsScript, uJupiterAction, LCLType,
  StdCtrls;

type

  { TFTextEditor }

  TFTextEditor = class(TFJupiterForm)
    seEditor: TSynEdit;
    SynAnySyn1: TSynAnySyn;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure seEditorChange(Sender: TObject);
  private
    FEdited : Boolean;

    procedure Internal_UpdateComponents; override;
    procedure Internal_PrepareForm; override;

    procedure Internal_OnSave(Sender: TObject);
    procedure Internal_OnAumentarFonte(Sender: TObject);
    procedure Internal_OnDiminuirFonte(Sender: TObject);
    procedure Internal_SetHighligther;

    function Internal_EnableWorkMenu : Boolean; override;
    procedure Internal_AddToWorkMenu; override;

    function Internal_GetRouteName : String;
    function Internal_GetMacroName : String;
  public

  end;

var
  FTextEditor: TFTextEditor;

implementation

uses SynHighLighterPas, SynHighLighterCpp, SynHighLighterJScript, SynHighLighterSQL, SynHighLighterBat;

{$R *.lfm}

{ TFTextEditor }

procedure TFTextEditor.seEditorChange(Sender: TObject);
begin
  if not Self.Prepared then
    Exit;

  if Self.FEdited then
    Exit;

  Self.FEdited := True;

  Self.UpdateForm();
end;

procedure TFTextEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Self.FEdited then
    if Application.MessageBox('Deseja salvar?', 'Confirmação', MB_YESNO + MB_ICONQUESTION) = ID_YES then
      Self.Internal_OnSave(Sender);
end;

procedure TFTextEditor.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if Self.FEdited then
    Self.ActionGroup.GetActionAtIndex(0).Enable;

  if not Self.FEdited then
    Self.ActionGroup.GetActionAtIndex(0).Disable;
end;

procedure TFTextEditor.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.FEdited := False;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Salvar', 'Clique aqui para abrir salvar o arquivo', ICON_SAVE, @Internal_OnSave));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Aumentar fonte', 'Clique aqui para aumentar a fonte', ICON_CURTASK, @Internal_OnAumentarFonte));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Diminuir fonte', 'Clique aqui para diminuir a fonte', ICON_CURTASK, @Internal_OnDiminuirFonte));

  Self.Caption := ExtractFileName(Self.Params.VariableById('path').Value);
  Self.Hint := Self.Params.VariableById('path').Value;

  Self.Internal_SetHighligther;

  seEditor.Lines.Clear;
  seEditor.Lines.LoadFromFile(Self.Params.VariableById('path').Value);

  Self.FEdited := False;
end;

procedure TFTextEditor.Internal_OnSave(Sender: TObject);
begin
  Self.FEdited := False;

  try
    seEditor.Lines.SaveToFile(Self.Params.VariableById('path').Value);
  finally
    Self.UpdateForm();
  end;
end;

procedure TFTextEditor.Internal_OnAumentarFonte(Sender: TObject);
begin
  seEditor.Font.Size := seEditor.Font.Size + 1;
end;

procedure TFTextEditor.Internal_OnDiminuirFonte(Sender: TObject);
begin
  seEditor.Font.Size := seEditor.Font.Size - 1;
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

  if (vrExtension = '.MD') then
    seEditor.Highlighter := CreateSynHighlighterMarkDown(seEditor);
end;

function TFTextEditor.Internal_EnableWorkMenu: Boolean;
begin
  Result := True;
end;

procedure TFTextEditor.Internal_AddToWorkMenu;
var
  vrModule : TJupiterModule;
  vrWizard : TJupiterDatabaseWizard;
begin
  inherited Internal_AddToWorkMenu;

  vrWizard := vrJupiterApp.NewWizard;
  vrModule := TJupiterModule.Create;
  try
    if vrModule.CreateMacroIfDontExists(Self.Internal_GetMacroName, 'Clique do item de menu ' + ExtractFileName(Self.Params.VariableById('path').Value), CreateStringListToMacro(' OpenTextEditorForm(''' + Self.Params.VariableById('path').Value + '''); ')) then
      vrModule.CreateRouteIfDontExists(Self.Caption, Self.Internal_GetRouteName, vrWizard.GetLastID('MACROS'), ICON_DOCFILE, 1000);
  finally
    FreeAndNil(vrModule);
    FreeAndNil(vrWizard);
  end;
end;

function TFTextEditor.Internal_GetRouteName: String;
begin
  Result := vrJupiterApp.Params.VariableById('Menus.Work.Route').Value + '/file_' + FormatDateTime('ddmmyyyy_hhnnss', Now);
end;

function TFTextEditor.Internal_GetMacroName: String;
begin
  Result := JupiterStringUtilsScript_Replace(Copy(Self.Internal_GetRouteName, 2), '/', '.');
end;

end.

