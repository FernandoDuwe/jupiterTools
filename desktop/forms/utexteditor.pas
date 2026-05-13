unit uTextEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, SynEdit,
  SynHighlighterMulti, SynHighlighterAny, uJupiterForm, JupiterConsts,
  JupiterEnviroment, JupiterModule, jupiterDatabaseWizard, JupiterApp,
  jupiterformutils, uJupiterStringUtilsScript, uJupiterAction,
  jupiterDesktopApp, LCLType, StdCtrls, ExtCtrls, SQLDB;

type

  { TFTextEditor }

  TFTextEditor = class(TFJupiterForm)
    cbHighlighter: TComboBox;
    pnTools: TPanel;
    seEditor: TSynEdit;
    SynAnySyn1: TSynAnySyn;
    procedure cbHighlighterChange(Sender: TObject);
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

procedure TFTextEditor.cbHighlighterChange(Sender: TObject);
begin
  if Assigned(seEditor.Highlighter) then
    seEditor.Highlighter.Free;

  case cbHighlighter.ItemIndex of
    1 : seEditor.Highlighter := TSynPasSyn.Create(seEditor);
    2 : seEditor.Highlighter := TSynCppSyn.Create(seEditor);
    3 : seEditor.Highlighter := TSynJScriptSyn.Create(seEditor);
    4 : seEditor.Highlighter := TSynSQLSyn.Create(seEditor);
    5 : seEditor.Highlighter := TSynBatSyn.Create(seEditor);
    6 : seEditor.Highlighter := CreateSynHighlighterMarkDown(seEditor);
  end;
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

  if Self.Params.Exists('path') then
  begin
    Self.Internal_SetHighligther;

    seEditor.Lines.Clear;

    Self.Caption := ExtractFileName(Self.Params.VariableById('path').Value);
    Self.Hint := Self.Params.VariableById('path').Value;

    seEditor.Lines.LoadFromFile(Self.Params.VariableById('path').Value);

    pnTools.Visible := False;
  end
  else
  begin
    seEditor.Lines.Clear;
    seEditor.Lines.Text := vrJupiterApp.NewWizard.Resolve(Self.Params.VariableById('table').Value,
                                                          Self.Params.VariableById('field').Value,
                                                          ' ID = ' + Self.Params.VariableById('id').Value);

    Self.Caption := String.Format('%0:s', [TJupiterDesktopApp(vrJupiterApp).NewWizard.GetTableDescription(Self.Params.VariableById('table').Value, StrToInt(Self.Params.VariableById('id').Value))]);
    Self.Hint := Self.Caption;

    if Params.Exists('highLighter') then
      if not Params.VariableById('highLighter').IsEmpty then
      begin
        cbHighlighter.ItemIndex := cbHighlighter.Items.IndexOf(Params.VariableById('highLighter').Value);
        cbHighlighterChange(Self);
        pnTools.Visible := False;
      end;
  end;

  Self.FEdited := False;
end;

procedure TFTextEditor.Internal_OnSave(Sender: TObject);
var
  vrQry : TSQLQuery;
  vrWizard : TJupiterDatabaseWizard;
begin
  Self.FEdited := False;

  vrWizard := vrJupiterApp.NewWizard;
  vrQry    := vrWizard.NewQuery;
  try
    if Self.Params.Exists('path') then
      seEditor.Lines.SaveToFile(Self.Params.VariableById('path').Value)
    else
    begin
      vrQry.SQL.Add(' UPDATE ' + Self.Params.VariableById('table').Value);
      vrQry.SQL.Add(' SET ' + Self.Params.VariableById('field').Value + ' = :PRTEXT ');
      vrQry.SQL.Add(' WHERE ID = ' + Self.Params.VariableById('id').Value);
      vrQry.ParamByName('PRTEXT').AsString := seEditor.Lines.Text;

      if not vrWizard.Transaction.Active then
        vrWizard.Transaction.StartTransaction;

      try
        vrQry.ExecSQL;

        vrWizard.Transaction.CommitRetaining;
      except
        vrWizard.Transaction.RollbackRetaining;

        raise;
      end;
    end;
  finally
    FreeAndNil(vrWizard);
    FreeAndNil(vrQry);

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
    cbHighlighter.ItemIndex := 1;

    seEditor.Highlighter := TSynPasSyn.Create(seEditor);

  if (vrExtension = '.CS') then
    cbHighlighter.ItemIndex := 2;

    seEditor.Highlighter := TSynCppSyn.Create(seEditor);

  if (vrExtension = '.JS') then
    cbHighlighter.ItemIndex := 3;
    seEditor.Highlighter := TSynJScriptSyn.Create(seEditor);

  if (vrExtension = '.SQL') then
    cbHighlighter.ItemIndex := 4;
    seEditor.Highlighter := TSynSQLSyn.Create(seEditor);

  if (vrExtension = '.BAT') then
    cbHighlighter.ItemIndex := 5;
    seEditor.Highlighter := TSynBatSyn.Create(seEditor);

  if (vrExtension = '.MD') then
    cbHighlighter.ItemIndex := 6;
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
  if Self.Params.Exists('path') then
    Result := vrJupiterApp.Params.VariableById('Menus.Work.Route').Value + '/file_' + FormatDateTime('ddmmyyyy_hhnnss', Now);

  Result := Self.Caption;
end;

function TFTextEditor.Internal_GetMacroName: String;
begin
  Result := JupiterStringUtilsScript_Replace(Copy(Self.Internal_GetRouteName, 2), '/', '.');
end;

end.

