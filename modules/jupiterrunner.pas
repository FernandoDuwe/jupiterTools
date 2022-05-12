unit jupiterRunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, Forms, JupiterModule, JupiterApp, JupiterConsts, fileUtils,
  SysUtils, process;

type

  { TJupiterRunner }

  TJupiterRunner = class(TJupiterModule)
  protected
    procedure Internal_Initialize; override;
    function  Internal_GetIdentifier: String; override;
  public
    procedure ListItems(prParams : TJupiterListem; var prList : TList); override;

    procedure GetTasks(var prTreeMenu : TTreeView); override;

    procedure RunListable(prParams : TJupiterListableItem); override;
  end;

implementation

uses LCLIntf;

{ TJupiterRunner }

procedure TJupiterRunner.Internal_Initialize;
var
  vrStr : TStrings;
begin
  inherited Internal_Initialize;

  if not DirectoryExists(ExtractFileDir(Application.ExeName) + '/modules/runner/') then
     CreateDir(ExtractFileDir(Application.ExeName) + '/modules/runner/');

  vrStr := TStringList.Create;
  try
    if not FileExists(ExtractFileDir(Application.ExeName) + '/modules/runner/folders.csv') then
    begin
      vrStr.Clear;
      vrStr.Add('PATH;DESCRIPTION;');

      vrStr.SaveToFile(ExtractFileDir(Application.ExeName) + '/modules/runner/folders.csv');
    end;

    if not FileExists(ExtractFileDir(Application.ExeName) + '/modules/runner/applications.csv') then
    begin
      vrStr.Clear;
      vrStr.Add('EXEPATH;DESCRIPTION;');

      vrStr.SaveToFile(ExtractFileDir(Application.ExeName) + '/modules/runner/applications.csv');
    end;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

function TJupiterRunner.Internal_GetIdentifier: String;
begin
  Result := 'JupiterTools.Modules.Runner';
end;

procedure TJupiterRunner.ListItems(prParams: TJupiterListem; var prList: TList);
var
  vrStr    : TStrings;
  vrStrAux : TStrings;
  vrVez    : Integer;
  vrObj    : TJupiterListableItem;
begin
  inherited ListItems(prParams, prList);

  prList.Clear;

  vrStr    := TStringList.Create;
  vrStrAux := TStringList.Create;
  try
    vrStr.Clear;

    if prParams.Task = '/' then
    begin
      vrStr.Add(EmptyStr);
      vrStr.Add(Format('%0:s;%1:s;', [ExtractFileDir(Application.ExeName) + '/modules/runner/folders.csv', 'Lista de Pastas favoritas']));
      vrStr.Add(Format('%0:s;%1:s;', [ExtractFileDir(Application.ExeName) + '/modules/runner/applications.csv', 'Lista de Aplicações favoritas']));
    end;

    if prParams.Task = '/folders' then
      vrStr.LoadFromFile(ExtractFileDir(Application.ExeName) + '/modules/runner/folders.csv');

    if prParams.Task = '/applications' then
      vrStr.LoadFromFile(ExtractFileDir(Application.ExeName) + '/modules/runner/applications.csv');

    if vrStr.Count = 0 then
      Exit;

    for vrVez := 1 to vrStr.Count - 1 do
    begin
      vrStrAux.Clear;
      vrStrAux.Delimiter := ';';
      vrStrAux.DelimitedText := StringReplace(vrStr[vrVez], ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]);

      vrObj           := TJupiterListableItem.Create();
      vrObj.Item      := StringReplace(vrStrAux[0], EMPTY_SPACE_SEPARATOR, ' ', [rfReplaceAll, rfIgnoreCase]);
      vrObj.Descricao := StringReplace(vrStrAux[1], EMPTY_SPACE_SEPARATOR, ' ', [rfReplaceAll, rfIgnoreCase]);
      vrObj.Param     := StringReplace(vrStrAux[0], EMPTY_SPACE_SEPARATOR, ' ', [rfReplaceAll, rfIgnoreCase]);

      prList.Add(vrObj);
    end;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);

    vrStrAux.Clear;
    FreeAndNil(vrStrAux);
  end;
end;

procedure TJupiterRunner.GetTasks(var prTreeMenu: TTreeView);
var
  vrNode : TTreeNode;
  vrSubNode : TTreeNode;
begin
  inherited GetTasks(prTreeMenu);

  vrNode               := prTreeMenu.Items.Add(nil, 'Scritps');
  vrNode.ImageIndex    := ICON_SCRIPTS;
  vrNode.SelectedIndex := ICON_SCRIPTS;

  vrNode.Data := TJupiterListem.Create(Self.ID, '/');

  vrNode               := prTreeMenu.Items.Add(nil, 'Favoritos');
  vrNode.ImageIndex    := ICON_FAVORITE;
  vrNode.SelectedIndex := ICON_FAVORITE;

  vrNode.Data := TJupiterListem.Create(Self.ID, '/');

  vrSubNode := prTreeMenu.Items.AddChild(vrNode, 'Pastas');
  vrSubNode.ImageIndex    := ICON_FOLDER;
  vrSubNode.SelectedIndex := ICON_FOLDER;

  vrSubNode.Data := TJupiterListem.Create(Self.ID, '/folders', EmptyStr);

  vrSubNode := prTreeMenu.Items.AddChild(vrNode, 'Executáveis');
  vrSubNode.ImageIndex    := ICON_PACKAGE;
  vrSubNode.SelectedIndex := ICON_PACKAGE;

  vrSubNode.Data := TJupiterListem.Create(Self.ID, '/applications', EmptyStr);

  vrNode.Expanded := True;
end;

procedure TJupiterRunner.RunListable(prParams: TJupiterListableItem);
var
  vrExtensions : String;
  vrOutput : AnsiString;
begin
  inherited RunListable(prParams);

  vrExtensions := vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.OpenInEditorPrefExtensions').Value;

  if DirectoryExists(prParams.Param) then
    OpenDocument(prParams.Param)
  else
    if ((Trim(ExtractFileExt(prParams.Param)) <> EmptyStr) and (Pos(AnsiUpperCase(ExtractFileExt(prParams.Param)), AnsiUpperCase(vrExtensions)) <> -1)) then
      RunCommand(vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.EditorPref').Value, [prParams.Param], vrOutput)
    else
      RunCommand(prParams.Param, [], vrOutput);
end;

end.

