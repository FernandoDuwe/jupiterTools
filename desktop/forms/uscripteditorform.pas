unit uScriptEditorForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, ValEdit, SynEdit, SynHighlighterPas, SynCompletion, uJupiterForm,
  jupiterformutils, jupiterScript, JupiterConsts, JupiterApp, uJupiterAction,
  jupiterDesktopApp;

type

  { TFScriptEditorForm }

  TFScriptEditorForm = class(TFJupiterForm)
    mmMessages: TMemo;
    pcMessages: TPageControl;
    pnMessages: TPanel;
    pnBody: TPanel;
    pnLeft: TPanel;
    Splitter1: TSplitter;
    seScript: TSynEdit;
    SynAutoComplete1: TSynAutoComplete;
    SynCompletion1: TSynCompletion;
    SynFreePascalSyn1: TSynFreePascalSyn;
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
begin
  inherited Internal_PrepareForm;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Executar', 'Clique aqui para executar o script', ICON_PLAY, @Internal_OnPlay));

  seScript.Lines.Clear;
  seScript.Lines.AddStrings(CreateStringListToMacro('// Seu código aqui'));
end;

procedure TFScriptEditorForm.Internal_OnPlay(Sender: TObject);
begin
  pcMessages.PageIndex := 0;

  Self.FScript.Script.Clear;
  Self.FScript.Script.AddStrings(seScript.Lines);
  Self.FScript.Execute;

  if Self.FScript.Runned then
    pcMessages.PageIndex := 1;

  mmMessages.Lines.Clear;
  mmMessages.Lines.AddStrings(Self.FScript.RunMessages);
  mmMessages.Lines.AddStrings(EmptyStr);
  mmMessages.Lines.AddStrings(Self.FScript.Messages);
end;

end.

