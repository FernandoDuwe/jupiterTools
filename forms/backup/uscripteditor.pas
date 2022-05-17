unit uScriptEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, SynEdit, SynHighlighterSQL, SynHighlighterBat, SynCompletion,
  uJupiterForm, JupiterApp, fileUtils;

type

  { TFScriptEditor }

  TFScriptEditor = class(TJupiterForm)
    btOpenEditor: TButton;
    btEdit: TButton;
    pnBody: TPanel;
    pnTaskBar: TPanel;
    sbRefresh: TSpeedButton;
    sbCopy: TSpeedButton;
    seEditor: TSynEdit;
    syAutocomplete: TSynAutoComplete;
    syHighBAT: TSynBatSyn;
    syHighSQL: TSynSQLSyn;
    SynCompletion1: TSynCompletion;
    procedure btEditClick(Sender: TObject);
    procedure btOpenEditorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbCopyClick(Sender: TObject);
    procedure sbRefreshClick(Sender: TObject);
  private
     FFileName : String;
     FEditMode : Boolean;

     procedure Internal_SetEditMode(prEditMode : Boolean);
     procedure Internal_SetFilename(prFileName : String);
     procedure Internal_UpdateDatasets; override;
     procedure Internal_UpdateComponents; override;
  published
    property EditMode : Boolean read FEditMode write Internal_SetEditMode;
    property FileName : String  read FFileName write Internal_SetFilename;
  public

  end;

var
  FScriptEditor: TFScriptEditor;

implementation

uses process, StrUtils, uMain;

{$R *.lfm}

{ TFScriptEditor }

procedure TFScriptEditor.FormCreate(Sender: TObject);
begin
  seEditor.Lines.Clear;

  Self.FEditMode := False;
  Self.FFileName := EmptyStr;
end;

procedure TFScriptEditor.sbCopyClick(Sender: TObject);
begin
  seEditor.CopyToClipboard;
end;

procedure TFScriptEditor.sbRefreshClick(Sender: TObject);
begin
  try
    seEditor.Lines.SaveToFile(TratarCaminho(ExtractFileDir(Application.ExeName) + '/temp/temp.bat'));
  finally
    RunCommand(TratarCaminho(ExtractFileDir(Application.ExeName) + '/temp/temp.bat'), [], vrOutput);
  end;
end;

procedure TFScriptEditor.btEditClick(Sender: TObject);
begin
  if Self.EditMode then
  begin
    seEditor.Lines.SaveToFile(Self.FileName);

    Self.FileName := Self.FileName;
  end
  else
    seEditor.Lines.LoadFromFile(Self.FileName);

  Self.EditMode := not Self.EditMode;
end;

procedure TFScriptEditor.btOpenEditorClick(Sender: TObject);
var
  vrOutput : String;
begin
  RunCommand(vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.EditorPref').Value, [Self.FileName], vrOutput)
end;

procedure TFScriptEditor.Internal_SetEditMode(prEditMode: Boolean);
begin
  try
    Self.FEditMode := prEditMode;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFScriptEditor.Internal_SetFilename(prFileName: String);
var
  vrVez : Integer;
begin
  Self.FFileName := prFileName;

  if AnsiUpperCase(ExtractFileExt(Self.FileName)) = '.SQL' then
    seEditor.Highlighter := syHighSQL
  else
    seEditor.Highlighter := syHighBAT;

  seEditor.Lines.LoadFromFile(prFileName);

  for vrVez := 0 to seEditor.Lines.Count -1 do
    seEditor.Lines[vrVez] := vrJupiterApp.Config.ResolveString(seEditor.Lines[vrVez]);
end;

procedure TFScriptEditor.Internal_UpdateDatasets;
var
  vrVez : Integer;
begin
  inherited Internal_UpdateDatasets;

  syAutocomplete.AutoCompleteList.Clear;
  SynCompletion1.ItemList.Clear;

  for vrVez := 0 to vrJupiterApp.Config.Count - 1 do
  begin
    syAutocomplete.AutoCompleteList.Add(Format('{%0:s}', [vrJupiterApp.Config.GetByIndex(vrVez).ID]));
    SynCompletion1.ItemList.Add(Format('{%0:s}', [vrJupiterApp.Config.GetByIndex(vrVez).ID]));
  end;

  if not Self.EditMode then
    Self.FileName := Self.FileName;
end;

procedure TFScriptEditor.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  seEditor.ReadOnly    := not Self.EditMode;
  sbRefresh.Enabled    := not Self.EditMode;
  btOpenEditor.Enabled := not Self.EditMode;

  btEdit.Caption := IfThen(Self.EditMode, 'Salvar', 'Editar');

  sbRefresh.Enabled := AnsiUpperCase(ExtractFileExt(Self.FileName)) = '.BAT';
end;

end.

