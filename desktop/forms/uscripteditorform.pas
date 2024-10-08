unit uScriptEditorForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, ValEdit, SynEdit, SynHighlighterPas, SynCompletion, uJupiterForm,
  jupiterformutils, jupiterScript, JupiterConsts, JupiterApp, JupiterRoute,
  uJupiterAction, jupiterDesktopApp, uJupiterDesktopAppScript;

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
    FScript : TJupiterScript;

    procedure Internal_UpdateComponents; override;
    procedure Internal_PrepareForm; override;

    procedure Internal_OnNew(Sender: TObject);
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
end;

procedure TFScriptEditorForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Self.FScript);

  inherited;
end;

procedure TFScriptEditorForm.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  pnLeft.Width := PercentOfScreen(Self.Width, 30);
  pnMessages.Height := PercentOfScreen(Self.Height, 30);

  vlVariables.DefaultColWidth := PercentOfScreen(vlVariables.Width, 40);
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

  Self.ActionGroup.AddAction(TJupiterAction.Create('Salvar', 'Clique aqui para salvar o script', ICON_SAVE, @Internal_OnSave));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Executar', 'Clique aqui para executar o script', ICON_PLAY, @Internal_OnPlay));

  seScript.Lines.Clear;
  seScript.Lines.AddStrings(CreateStringListToMacro('// Seu código aqui'));

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

procedure TFScriptEditorForm.Internal_OnNew(Sender: TObject);
begin
  seScript.Lines.Clear;
  seScript.Lines.AddStrings(CreateStringListToMacro('// Seu código aqui'));
end;

procedure TFScriptEditorForm.Internal_OnSave(Sender: TObject);
begin
  //
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

