unit jupitershortcut;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, LCLProc, SysUtils, JupiterObject, JupiterConsts, JupiterEnviroment,
  JupiterXMLDataProvider;

type

  { TJupiterShortcut }

  TJupiterShortcut = class(TJupiterObject)
  private
    FDescription : String;
    FCodeRun : String;
    FFileName : String;
    FShortCutStr : String;
  published
    property Description  : String read FDescription write FDescription;
    property CodeRun      : String read FCodeRun     write FCodeRun;
    property FileName     : String read FFileName    write FFileName;
    property ShortCutStr  : String read FShortCutStr write FShortCutStr;
  public
    function GetShortCut : TShortCut;
  end;

  { TJupiterShortcutList }

  TJupiterShortcutList = class(TJupiterObjectList)
  protected
    procedure Internal_LoadFromFile;
    procedure Internal_SaveToFile;
  public
    function GetShortcutByIndex(prIndex : Integer) : TJupiterShortcut;

    constructor Create; override;
  end;

implementation

{ TJupiterShortcutList }

procedure TJupiterShortcutList.Internal_LoadFromFile;
var
  vrEnviroment : TJupiterEnviroment;
  vrXML : TJupiterXMLDataProvider;
  vrVez : Integer;
  vrShortcut : TJupiterShortcut;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  vrXML        := TJupiterXMLDataProvider.Create;
  try
    vrXML.SearchNode := 'shortcut';
    vrXML.Filename   := vrEnviroment.FullPath('/modules/generator/data/shortcut_list.xml');
    vrXML.ProvideData;

    for vrVez := 0 to vrXML.Count - 1 do
      with vrXML.GetRowByIndex(vrVez) do
      begin
        vrShortcut := TJupiterShortcut.Create();
        vrShortcut.Description := Fields.VariableById('description').Value;
        vrShortcut.CodeRun     := Fields.VariableById('codeRun').Value;
        vrShortcut.FileName    := Fields.VariableById('fileName').Value;
        vrShortcut.ShortCutStr := Fields.VariableById('shortCut').Value;

        Self.Add(vrShortcut);
      end;
  finally
    FreeAndNil(vrEnviroment);
    FreeAndNil(vrXML);
  end;
end;

procedure TJupiterShortcutList.Internal_SaveToFile;
var
  vrEnviroment : TJupiterEnviroment;
  vrFile      : TStringList;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  vrFile := TStringList.Create;
  try
    vrFile.Clear;
    vrFile.Add('<?xml version="1.0" encoding="UTF-8"?>');
    vrFile.Add('<content>');
    vrFile.Add('  <shortcuts>');
    vrFile.Add('  </shortcuts>');
    vrFile.Add('</content>');

    vrEnviroment.CreateFile('/modules/generator/data/shortcut_list.xml', vrFile.Text);
  finally
    vrFile.Clear;
    FreeAndNil(vrFile);

    FreeAndNil(vrEnviroment);
  end;
end;

function TJupiterShortcutList.GetShortcutByIndex(prIndex: Integer): TJupiterShortcut;
begin
  Result := TJupiterShortcut(Self.GetAtIndex(prIndex));
end;

constructor TJupiterShortcutList.Create;
var
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Create;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    if not vrEnviroment.Exists(vrEnviroment.FullPath('/modules/generator/data/')) then
      vrEnviroment.CreatePath('/modules/generator/data/');

    if not vrEnviroment.Exists(vrEnviroment.FullPath('/modules/generator/data/shortcut_list.xml')) then
      Self.Internal_SaveToFile;

    Self.Internal_LoadFromFile;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

{ TJupiterShortcut }

function TJupiterShortcut.GetShortCut: TShortCut;
begin
  Result := TextToShortCut(Self.ShortCutStr);
end;

end.

