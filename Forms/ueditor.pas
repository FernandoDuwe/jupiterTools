unit uEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, SynEdit, SynCompletion,
  JupiterForm, JupiterConsts, JupiterAction, JupiterEnviroment, JupiterApp,
  JupiterVariable;

type

  { TFEditor }

  TFEditor = class(TFJupiterForm)
    seEditor: TSynEdit;
    SynAutoComplete1: TSynAutoComplete;
    SynCompletion1: TSynCompletion;
  private
    FFileName : String;
    FReadMode : Boolean;

    procedure Internal_ListVariables;
    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateComponents; override;

    procedure Internal_SaveFileClick(Sender : TObject);
    procedure Internal_EditFileClick(Sender : TObject);
    procedure Internal_CopyFileClick(Sender : TObject);
    procedure Internal_ResolveVariables;

    procedure Internal_SetFileName(prFileName : String);
    procedure Internal_SetReadMode(prReadMode : Boolean);
  published
    property FileName : String  read FFileName write Internal_SetFileName;
    property ReadMode : Boolean read FReadMode write Internal_SetReadMode;
  public

  end;

var
  FEditor: TFEditor;

implementation

uses SynHighLighterPas, SynHighLighterCpp, SynHighLighterJava, SynHighLighterJScript,
     SynHighLighterHTML, SynHighLighterXML, SynHighLighterCSS, SynHighLighterPHP,
     SynHighLighterSQL, SynHighLighterPython, SynHighLighterBat, SynHighLighterIni;

{$R *.lfm}

{ TFEditor }

procedure TFEditor.Internal_ListVariables;
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

procedure TFEditor.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  seEditor.Lines.Clear;

  if ((Self.Params.Exists('filename')) and (FileExists(Self.Params.VariableById('filename').Value))) then
  begin
    Self.Params.AddVariable(FIELD_ID_GENERADOR, 'Editor' + StringReplace(AnsiUpperCase(ExtractFileExt(Self.Params.VariableById('filename').Value)), '.', EmptyStr, [rfIgnoreCase, rfReplaceAll]) + 'Form', 'ID do formulário');

    Self.FileName := Self.Params.VariableById('filename').Value;
    Self.Internal_ResolveVariables;
  end;

  Self.Actions.Add(TJupiterAction.Create('Salvar', @Internal_SaveFileClick));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para salvar o arquivo ' + ExtractFileName(Self.Params.VariableById('filename').Value);
    Icon := ICON_SAVE;
  end;

  Self.Actions.Add(TJupiterAction.Create('Editar', @Internal_EditFileClick));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para editar o conteúdo do arquivo';
    Icon := ICON_EDIT;
  end;

  Self.Actions.Add(TJupiterAction.Create('Copiar', @Internal_CopyFileClick));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para copiar o conteúdo do arquivo';
    Icon := ICON_COPY;
  end;

  Self.FReadMode := True;

  Self.Internal_ListVariables;
end;

procedure TFEditor.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  seEditor.Font.Size := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);

  seEditor.ReadOnly := Self.FReadMode;

  Self.Actions.GetActionButton(0, sbActions).Enabled := not Self.FReadMode;
  Self.Actions.GetActionButton(1, sbActions).Enabled := Self.FReadMode;
end;

procedure TFEditor.Internal_SaveFileClick(Sender: TObject);
begin
  seEditor.Lines.SaveToFile(Self.FileName);
  Self.Internal_ResolveVariables;

  Self.ReadMode := True;
end;

procedure TFEditor.Internal_EditFileClick(Sender: TObject);
begin
  seEditor.Lines.LoadFromFile(Self.FileName);

  Self.ReadMode := False;
end;

procedure TFEditor.Internal_CopyFileClick(Sender: TObject);
begin
  seEditor.SelectAll;
  seEditor.CopyToClipboard;
end;

procedure TFEditor.Internal_ResolveVariables;
var
  vrVez : Integer;
begin
  for vrVez := 0 to seEditor.Lines.Count - 1 do
    seEditor.Lines[vrVez] := vrJupiterApp.Params.ResolveString(seEditor.Lines[vrVez]);
end;

procedure TFEditor.Internal_SetFileName(prFileName: String);
var
  vrEnviroment : TJupiterEnviroment;
begin
  Self.FFileName := prFileName;

  seEditor.Lines.LoadFromFile(Self.Params.VariableById('filename').Value);

  vrEnviroment := TJupiterEnviroment.Create();
  try
    if vrEnviroment.IsOfExtension(prFileName, '.pas') then
      seEditor.Highlighter := TSynPasSyn.Create(seEditor);

    if vrEnviroment.IsOfExtension(prFileName, '.cs') then
      seEditor.Highlighter := TSynCppSyn.Create(seEditor);

    if vrEnviroment.IsOfExtension(prFileName, '.java') then
      seEditor.Highlighter := TSynJavaSyn.Create(seEditor);

    if vrEnviroment.IsOfExtension(prFileName, '.js') then
      seEditor.Highlighter := TSynJScriptSyn.Create(seEditor);

    if vrEnviroment.IsOfExtension(prFileName, '.html') then
      seEditor.Highlighter := TSynHTMLSyn.Create(seEditor);

    if vrEnviroment.IsOfExtension(prFileName, '.xml') then
      seEditor.Highlighter := TSynXMLSyn.Create(seEditor);

    if vrEnviroment.IsOfExtension(prFileName, '.css') then
      seEditor.Highlighter := TSynCssSyn.Create(seEditor);

    if vrEnviroment.IsOfExtension(prFileName, '.php') then
      seEditor.Highlighter := TSynPHPSyn.Create(seEditor);

    if vrEnviroment.IsOfExtension(prFileName, '.sql') then
      seEditor.Highlighter := TSynSQLSyn.Create(seEditor);

    if vrEnviroment.IsOfExtension(prFileName, '.py') then
      seEditor.Highlighter := TSynPythonSyn.Create(seEditor);

    if vrEnviroment.IsOfExtension(prFileName, '.bat') then
      seEditor.Highlighter := TSynBatSyn.Create(seEditor);

    if vrEnviroment.IsOfExtension(prFileName, '.ini') then
      seEditor.Highlighter := TSynIniSyn.Create(seEditor);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFEditor.Internal_SetReadMode(prReadMode: Boolean);
begin
  Self.FReadMode := prReadMode;

  Self.UpdateForm;
end;

end.

