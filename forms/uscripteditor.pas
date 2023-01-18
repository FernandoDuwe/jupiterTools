unit uScriptEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, SynEdit, SynHighlighterPas, SynCompletion, JupiterForm,
  JupiterConsts, JupiterAction, jupiterScript, JupiterApp, JupiterVariable,
  jupiterformutils, Types, LCLType;

type

  { TFScriptEditor }

  TFScriptEditor = class(TFJupiterForm)
    mmRun: TMemo;
    mmMessages: TMemo;
    pcOutput: TPageControl;
    Splitter1: TSplitter;
    syEdit: TSynEdit;
    SynAutoComplete1: TSynAutoComplete;
    SynCompletion1: TSynCompletion;
    syPas: TSynPasSyn;
    TabSheet1: TTabSheet;
    tsMessages: TTabSheet;
    procedure FormShow(Sender: TObject);
    procedure SynCompletion1CodeCompletion(var Value: string;
      SourceValue: string; var SourceStart, SourceEnd: TPoint;
      KeyChar: TUTF8Char; Shift: TShiftState);
  private
    FFileName : String;
    procedure Internal_SetFileName(prFileName : String);
    procedure Internal_ListVariables;

    procedure Internal_UpdateComponents; override;

    procedure Internal_PrepareForm; override;
    procedure Internal_ExecuteClick(Sender : TObject);
    procedure Internal_SaveFileClick(Sender : TObject);
  published
    property FileName : String  read FFileName write Internal_SetFileName;
  public

  end;

var
  FScriptEditor: TFScriptEditor;

implementation

{$R *.lfm}

{ TFScriptEditor }

procedure TFScriptEditor.SynCompletion1CodeCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
begin

end;

procedure TFScriptEditor.FormShow(Sender: TObject);
begin
  inherited;

  pcOutput.Height := PercentOfScreen(Self.Height, 30);

  SynCompletion1.Width := PercentOfScreen(Self.Width, 40);
end;

procedure TFScriptEditor.Internal_SetFileName(prFileName : String);
begin
  Self.FFileName := prFileName;

  syEdit.Lines.LoadFromFile(Self.Params.VariableById('filename').Value);
end;

procedure TFScriptEditor.Internal_ListVariables;
var
  vrVez  : Integer;
  vrVez2 : Integer;
begin
  for vrVez := 0 to vrJupiterApp.Params.Size - 1 do
  begin
    SynCompletion1.ItemList.Add(vrJupiterApp.Params.VariableByIndex(vrVez).ID);

    SynAutoComplete1.AutoCompleteList.Add(vrJupiterApp.Params.VariableByIndex(vrVez).ID);
  end;

  for vrVez := 0 to vrJupiterApp.Params.ChildList.Size - 1 do
    with TJupiterVariableList(vrJupiterApp.Params.ChildList.GetAtIndex(vrVez)) do
    begin
      for vrVez2 := 0 to Size - 1 do
      begin
        SynCompletion1.ItemList.Add(VariableByIndex(vrVez2).ID);

        SynAutoComplete1.AutoCompleteList.Add(VariableByIndex(vrVez2).ID);
      end;
    end;
end;

procedure TFScriptEditor.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  syEdit.Font.Size := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);

  mmMessages.Font.Size := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);
  mmRun.Font.Size      := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);
end;

procedure TFScriptEditor.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  if ((Self.Params.Exists('filename')) and (FileExists(Self.Params.VariableById('filename').Value))) then
  begin
    Self.Params.AddVariable(FIELD_ID_GENERADOR, 'ScriptEditor' + StringReplace(AnsiUpperCase(ExtractFileExt(Self.Params.VariableById('filename').Value)), '.', EmptyStr, [rfIgnoreCase, rfReplaceAll]) + 'Form', 'ID do formulário');

    Self.FileName := Self.Params.VariableById('filename').Value;
  end;

  Self.Actions.Add(TJupiterAction.Create('Salvar', @Internal_SaveFileClick));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para salvar o script';
    Icon := ICON_SAVE;
  end;

  Self.Actions.Add(TJupiterAction.Create('Executar', @Internal_ExecuteClick));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para executar o script';
    Icon := ICON_PLAY;
  end;

  Self.Internal_ListVariables;
end;

procedure TFScriptEditor.Internal_ExecuteClick(Sender: TObject);
var
  vrScript : TJupiterScript;
begin
  mmMessages.Lines.Clear;
  mmRun.Lines.Clear;

  vrScript := TJupiterScript.Create;
  try
   vrScript.Script.AddStrings(syEdit.Lines);
   vrScript.Execute;

   if vrScript.Runned then
     pcOutput.ActivePageIndex := 1
   else
     pcOutput.ActivePageIndex := 0;

   mmMessages.Lines.AddStrings(vrScript.Messages);
   mmRun.Lines.AddStrings(vrScript.RunMessages);
  finally
    FreeAndNil(vrScript);
  end;
end;

procedure TFScriptEditor.Internal_SaveFileClick(Sender: TObject);
begin
  syEdit.Lines.SaveToFile(Self.FileName);
end;

end.

