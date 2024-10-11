unit uScriptEditorForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, ValEdit, SynEdit, SynHighlighterPas, SynCompletion, uJupiterForm,
  jupiterformutils, jupiterScript, JupiterConsts, JupiterApp, JupiterRoute,
  JupiterEnviroment, jupiterDatabaseWizard, uJupiterAction, jupiterDesktopApp,
  uJupiterDesktopAppScript;

type

  { TFScriptEditorForm }

  TFScriptEditorForm = class(TFJupiterForm)
    ilIcons: TImageList;
    mmMessages: TMemo;
    mmRunMessages: TMemo;
    pcMessages: TPageControl;
    pnMessages: TPanel;
    pnBody: TPanel;
    pnLeft: TPanel;
    Splitter1: TSplitter;
    seScript: TSynEdit;
    SynAutoComplete1: TSynAutoComplete;
    SynCompletion1: TSynCompletion;
    SynFreePascalSyn1: TSynFreePascalSyn;
    TabSheet1: TTabSheet;
    tsParams: TTabSheet;
    tsMessages: TTabSheet;
    tvLibrary: TTreeView;
    vlVariables: TValueListEditor;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FFilePath : String;
    FMacroID : Integer;

    FScript : TJupiterScript;

    procedure Internal_UpdateComponents; override;
    procedure Internal_PrepareForm; override;

    procedure Internal_LoadMacroFromId();

    procedure Internal_OnNew(Sender: TObject);
    procedure Internal_OnOpen(Sender: TObject);
    procedure Internal_OnSave(Sender: TObject);
    procedure Internal_OnPlay(Sender: TObject);
  public

  end;

var
  FScriptEditorForm: TFScriptEditorForm;

implementation

{$R *.lfm}

{ TFScriptEditorForm }

procedure TFScriptEditorForm.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FScript := TJupiterDesktopApp(vrJupiterApp).NewScript;

  Self.FFilePath := EmptyStr;
  Self.FMacroID := NULL_KEY;
end;

procedure TFScriptEditorForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Self.FScript);

  inherited;
end;

procedure TFScriptEditorForm.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  SynCompletion1.Width := PercentOfScreen(Self.Width, 50);

  pnLeft.Width := PercentOfScreen(Self.Width, 30);
  pnMessages.Height := PercentOfScreen(Self.Height, 30);

  vlVariables.DefaultColWidth := PercentOfScreen(vlVariables.Width, 40);

  if ((Self.FFilePath = EmptyStr) and (Self.FMacroID = NULL_KEY)) then
  begin
    Self.Caption := 'Editor de Scripts';

    Self.ActionGroup.GetActionAtIndex(0).Enable;
    Self.ActionGroup.GetActionAtIndex(1).Enable;

    Self.ActionGroup.GetActionAtIndex(2).Disable;
  end
  else
  begin
    if Self.FFilePath <> EmptyStr then
      Self.Caption := 'Editor de Scripts - ' + ExtractFileName(Self.FFilePath)
    else
      Self.Caption := 'Editor de Scripts - Macro #' + IntToStr(Self.FMacroID);

    Self.ActionGroup.GetActionAtIndex(0).Disable;
    Self.ActionGroup.GetActionAtIndex(1).Disable;

    Self.ActionGroup.GetActionAtIndex(2).Enable;
  end;
end;

procedure TFScriptEditorForm.Internal_PrepareForm;
var
  vrVez : Integer;
  vrVez2 : Integer;
  vrNode : TTreeNode;
  vrNodeChild : TTreeNode;
  vrList : TJupiterScriptAnalyserList;
begin
  inherited Internal_PrepareForm;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Novo', 'Clique aqui para criar um novo script', ICON_NEW, @Internal_OnNew));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Abrir', 'Clique aqui para abrir um script existente', ICON_OPEN, @Internal_OnOpen));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Salvar', 'Clique aqui para salvar o script', ICON_SAVE, @Internal_OnSave));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Executar', 'Clique aqui para executar o script', ICON_PLAY, @Internal_OnPlay));

  seScript.Lines.Clear;
  seScript.Lines.AddStrings(CreateStringListToMacro('// Seu código aqui'));

  if Self.Params.Exists('Params') then
  begin
    Self.FMacroID := Self.Params.VariableById('Params').AsInteger;

    Self.Internal_LoadMacroFromId();
  end;

  SynCompletion1.ItemList.Clear;
  SynAutoComplete1.AutoCompleteList.Clear;

  tvLibrary.Items.Clear;

  vrNode := tvLibrary.Items.Add(nil, 'Rotas');
  vrNode.ImageIndex := 2;
  vrNode.SelectedIndex := 2;

  for vrVez := 0 to TJupiterDesktopApp(vrJupiterApp).FormRoutes.Count - 1 do
    with TJupiterFormRoute(TJupiterDesktopApp(vrJupiterApp).FormRoutes.GetAtIndex(vrVez)) do
    begin
      vrNodeChild := tvLibrary.Items.AddChild(vrNode, Path);
      vrNodeChild.ImageIndex := 2;
      vrNodeChild.SelectedIndex := 2;

      SynCompletion1.ItemList.Add(Path);
      SynAutoComplete1.AutoCompleteList.Add(Path);
    end;

  for vrVez := 0 to Self.FScript.LibraryList.Count - 1 do
    with TJupiterScriptLibrary(Self.FScript.LibraryList.GetAtIndex(vrVez)) do
    begin
      vrNode := tvLibrary.Items.Add(nil, Name);
      vrNode.ImageIndex := 0;
      vrNode.SelectedIndex := 0;

      vrList := AnalyseCode;

      for vrVez2 := 0 to vrList.Count - 1 do
      begin
        vrNodeChild := tvLibrary.Items.AddChild(vrNode, TJupiterScriptAnalyserItem(vrList.GetAtIndex(vrVez2)).DocText);
        vrNodeChild.ImageIndex := 1;
        vrNodeChild.SelectedIndex := 1;

        SynCompletion1.ItemList.Add(TJupiterScriptAnalyserItem(vrList.GetAtIndex(vrVez2)).Text);
        SynAutoComplete1.AutoCompleteList.Add(TJupiterScriptAnalyserItem(vrList.GetAtIndex(vrVez2)).Text);
      end;
    end;
end;

procedure TFScriptEditorForm.Internal_LoadMacroFromId();
var
  vrWizard : TJupiterDatabaseWizard;
begin
  vrWizard := vrJupiterApp.NewWizard;
  try
    seScript.Lines.Clear;
    seScript.Lines.AddStrings(vrWizard.GetBLobField('MACROS', 'MACRO', ' ID = ' + IntToStr(Self.FMacroID)));
  finally
    FreeAndNil(vrWizard);
  end;
end;

procedure TFScriptEditorForm.Internal_OnNew(Sender: TObject);
begin
  Self.FFilePath := EmptyStr;
  Self.FMacroID := NULL_KEY;

  seScript.Lines.Clear;
  seScript.Lines.AddStrings(CreateStringListToMacro('// Seu código aqui'));

  Self.UpdateForm();
end;

procedure TFScriptEditorForm.Internal_OnOpen(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
  vrFilePath : String;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrFilePath := vrEnviroment.OpenFile('*.jpas');

    if vrFilePath <> EmptyStr then
    begin
      Self.FFilePath := vrFilePath;

      seScript.Lines.LoadFromFile(Self.FFilePath);
    end;
  finally
    FreeAndNil(vrEnviroment);

    Self.UpdateForm();
  end;
end;

procedure TFScriptEditorForm.Internal_OnSave(Sender: TObject);
var
  vrWizard : TJupiterDatabaseWizard;
begin
  if Self.FFilePath <> EmptyStr then
  begin
    seScript.Lines.SaveToFile(Self.FFilePath);
    Exit;
  end;

  vrWizard := vrJupiterApp.NewWizard;
  try
    vrWizard.UpdateBLOBField('MACROS', 'MACRO', ' ID = ' + IntToStr(Self.FMacroID), seScript.Lines);
  finally
    FreeAndNil(vrWizard);
  end;
end;

procedure TFScriptEditorForm.Internal_OnPlay(Sender: TObject);
var
  vrVez : Integer;
begin
  pcMessages.PageIndex := 0;

  Self.FScript.Script.Clear;
  Self.FScript.Script.AddStrings(seScript.Lines);
  Self.FScript.Execute;

  if not Self.FScript.Runned then
    pcMessages.PageIndex := 1;

  mmMessages.Lines.Clear;
  mmRunMessages.Lines.Clear;

  mmMessages.Lines.AddStrings(Self.FScript.RunMessages);
  mmRunMessages.Lines.AddStrings(Self.FScript.Messages);

  vlVariables.Strings.Clear;

  for vrVez := 0 to Self.FScript.Params.Count - 1 do
    with Self.FScript.Params.VariableByIndex(vrVez) do
      vlVariables.Strings.Add(ID + '=' + Value);
end;

end.

