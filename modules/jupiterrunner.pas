unit jupiterRunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, Forms, JupiterModule, JupiterApp, JupiterConsts, fileUtils,
  jupiterUtils, uNewFavoriteItem, SysUtils, process, UITypes, Dialogs;

type

  { TJupiterRunnerNewScriptBAT }

  TJupiterRunnerNewScriptBAT = class(TJupiterAction)
  public
    constructor Create;

    procedure Run(prParams : TJupiterListem); override;
  end;

  { TJupiterRunnerNewScriptSQL }

  TJupiterRunnerNewScriptSQL = class(TJupiterAction)
  public
    constructor Create;

    procedure Run(prParams : TJupiterListem); override;
  end;

  { TJupiterRunner }

  { TJupiterRunnerNewItemApp }

  TJupiterRunnerNewItemApp = class(TJupiterAction)
  public
    constructor Create;

    procedure Run(prParams : TJupiterListem); override;
  end;

  { TJupiterRunnerNewItemPath }

  TJupiterRunnerNewItemPath = class(TJupiterAction)
  public
    constructor Create;

    procedure Run(prParams : TJupiterListem); override;
  end;

  TJupiterRunner = class(TJupiterModule)
  protected
    procedure Internal_Initialize; override;
    function  Internal_GetIdentifier: String; override;

    procedure Internal_ListScripts(var prTreeMenu : TTreeView; prOwner : TTreeNode);
  public
    procedure ListItems(var prParams : TJupiterListem; var prList : TList); override;
    procedure ListActions(prParams : TJupiterListem; var prList : TList); override;

    procedure GetTasks(var prTreeMenu : TTreeView); override;

    procedure RunListable(var prParamsItem: TJupiterListem; var prParams : TJupiterListableItem); override;
  end;

implementation

uses LCLIntf;

{ TJupiterRunnerNewScriptBAT }

constructor TJupiterRunnerNewScriptBAT.Create;
begin
  Self.Title      := 'Novo BAT';
  Self.Hint       := 'Clique aqui para adicionar um novo arquivo de script BAT';
  Self.ImageIndex := ICON_SCRIPTS;
end;

procedure TJupiterRunnerNewScriptBAT.Run(prParams: TJupiterListem);
var
  vrFile : String;
begin
  InputQuery('Novo arquivo de script .BAT', 'Informe o nome do novo script BAT (sem extensão)', vrFile);

  if Trim(vrFile) = EmptyStr then
    Exit;

  NovoScriptBAT(vrFile);

  vrJupiterApp.Log.AddLog(Now, Self.ClassName, 'Criado script BAT: ' + vrFile);

  inherited Run(prParams);
end;

{ TJupiterRunnerNewScriptSQL }

constructor TJupiterRunnerNewScriptSQL.Create;
begin
  Self.Title      := 'Novo SQL';
  Self.Hint       := 'Clique aqui para adicionar um novo arquivo de script SQL';
  Self.ImageIndex := ICON_SQL;
end;

procedure TJupiterRunnerNewScriptSQL.Run(prParams: TJupiterListem);
var
  vrFile : String;
begin
  InputQuery('Novo arquivo de script .SQL', 'Informe o nome do novo script sql (sem extensão)', vrFile);

  if Trim(vrFile) = EmptyStr then
    Exit;

  NovoScriptSQL(vrFile);

  vrJupiterApp.Log.AddLog(Now, Self.ClassName, 'Criado script SQL: ' + vrFile);

  inherited Run(prParams);
end;

{ TJupiterRunnerNewItemPath }

constructor TJupiterRunnerNewItemPath.Create;
begin
  Self.Title      := 'Nova pasta';
  Self.Hint       := 'Clique aqui para adicionar uma nova pasta favorita';
  Self.ImageIndex := ICON_FAVORITE;
end;

procedure TJupiterRunnerNewItemPath.Run(prParams: TJupiterListem);
var
  vrStr : TStrings;
begin
  Application.CreateForm(TFNewFavoriteItem, FNewFavoriteItem);
  try
    FNewFavoriteItem.IsApplication := False;

    if FNewFavoriteItem.ShowModal = mrOK then
    begin
      vrStr := TStringList.Create;
      try
        vrStr.LoadFromFile(TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/runner/folders.csv'));
        vrStr.Add(FNewFavoriteItem.edPath.Text + ';' + FNewFavoriteItem.edTitulo.Text + ';');

        vrStr.SaveToFile(TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/runner/folders.csv'));
      finally
        vrStr.Clear;
        FreeAndNil(vrStr);
      end;
    end;
  finally
    FNewFavoriteItem.Release;
    FreeAndNil(FNewFavoriteItem);
  end;

  inherited Run(prParams);
end;

{ TJupiterRunnerNewItemApp }

constructor TJupiterRunnerNewItemApp.Create;
begin
  Self.Title      := 'Novo favorito';
  Self.Hint       := 'Clique aqui para adicionar um novo aplicativo favorito';
  Self.ImageIndex := ICON_FAVORITE;
end;

procedure TJupiterRunnerNewItemApp.Run(prParams: TJupiterListem);
var
  vrStr : TStrings;
begin
  Application.CreateForm(TFNewFavoriteItem, FNewFavoriteItem);
  try
    FNewFavoriteItem.IsApplication := True;

    if FNewFavoriteItem.ShowModal = mrOK then
    begin
      vrStr := TStringList.Create;
      try
        vrStr.LoadFromFile(TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/runner/applications.csv'));
        vrStr.Add(FNewFavoriteItem.edPath.Text + ';' + FNewFavoriteItem.edTitulo.Text + ';');

        vrStr.SaveToFile(TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/runner/applications.csv'));
      finally
        vrStr.Clear;
        FreeAndNil(vrStr);
      end;
    end;
  finally
    FNewFavoriteItem.Release;
    FreeAndNil(FNewFavoriteItem);
  end;

  inherited Run(prParams);
end;

{ TJupiterRunner }

procedure TJupiterRunner.Internal_Initialize;
var
  vrStr : TStrings;
begin
  inherited Internal_Initialize;

  if not DirectoryExists(TratarCaminho(ExtractFileDir(Application.ExeName) + '/modules/runner/')) then
     CreateDir(TratarCaminho(ExtractFileDir(Application.ExeName) + '/modules/runner/'));

  if not Self.JupiterApp.Config.Exists(Self.ID + '.ExecMethod') then
     Self.JupiterApp.Config.AddConfig(Self.ID + '.ExecMethod', 'RunCommand', 'Método de execução (RunCommand / ShellExecute / CreateProcess)');

  if not Self.JupiterApp.Config.Exists(Self.ID + '.ExecutorBATScript') then
     Self.JupiterApp.Config.AddConfig(Self.ID + '.ExecutorBATScript', 'cmd.exe', 'Aplicação que será chamada ao abrir um script bat na opção: Executar externamente');

  vrStr := TStringList.Create;
  try
    if not FileExists(TratarCaminho(ExtractFileDir(Application.ExeName) + '/modules/runner/folders.csv')) then
    begin
      vrStr.Clear;
      vrStr.Add('PATH;DESCRIPTION;');

      vrStr.SaveToFile(TratarCaminho(ExtractFileDir(Application.ExeName) + '/modules/runner/folders.csv'));
    end;

    if not FileExists(TratarCaminho(ExtractFileDir(Application.ExeName) + '/modules/runner/applications.csv')) then
    begin
      vrStr.Clear;
      vrStr.Add('EXEPATH;DESCRIPTION;');

      vrStr.SaveToFile(TratarCaminho(ExtractFileDir(Application.ExeName) + '/modules/runner/applications.csv'));
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

procedure TJupiterRunner.Internal_ListScripts(var prTreeMenu : TTreeView; prOwner: TTreeNode);
var
  vrStr  : TStrings;
  vrVez  : Integer;
  vrNode : TTreeNode;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;

    ListFiles(TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/runner/'), vrStr);

    for vrVez := 0 to vrStr.Count - 1 do
    begin
      if ((AnsiUpperCase(ExtractFileExt(vrStr[vrVez])) <> '.SQL') and (AnsiUpperCase(ExtractFileExt(vrStr[vrVez])) <> '.BAT')) then
        Continue;

      vrNode := prTreeMenu.Items.AddChild(prOwner, ExtractFileName(vrStr[vrVez]));

      if AnsiUpperCase(ExtractFileExt(vrStr[vrVez])) = '.SQL' then
      begin
        vrNode.ImageIndex    := ICON_SQL;
        vrNode.SelectedIndex := ICON_SQL;
      end
      else
      begin
        vrNode.ImageIndex    := ICON_SCRIPTS;
        vrNode.SelectedIndex := ICON_SCRIPTS;
      end;

      vrNode.Data := TJupiterListem.Create(Self.ID, EmptyStr, vrStr[vrVez], 50);
    end;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterRunner.ListItems(var prParams: TJupiterListem; var prList: TList);
var
  vrStr    : TStrings;
  vrStrAux : TStrings;
  vrVez    : Integer;
  vrObj    : TJupiterListableItem;
  vrIsDir  : Boolean;
  vrIcon   : Integer;
begin
  inherited ListItems(prParams, prList);

  vrIsDir := False;

  prList.Clear;

  vrStr    := TStringList.Create;
  vrStrAux := TStringList.Create;
  try
    vrStr.Clear;

    if prParams.Task = '/fav' then
    begin
      Self.JupiterApp.Config.AddVariable(Self.JupiterApp.AppName + '.Variables.CurrentPath', TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/runner/'), 'Diretório atual');
      Self.JupiterApp.Config.AddVariable(Self.JupiterApp.AppName + '.Variables.CurrentFile', prParams.Params, 'Arquivo atual');

      vrIcon := ICON_CONFIG;

      vrStr.Add(EmptyStr);
      vrStr.Add(Format('%0:s;%1:s;', [TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/runner/folders.csv'), 'Lista de Pastas favoritas']));
      vrStr.Add(Format('%0:s;%1:s;', [TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/runner/applications.csv'), 'Lista de Aplicações favoritas']));
    end;

    if prParams.Task = '/scripts' then
    begin
      Self.JupiterApp.Config.AddVariable(Self.JupiterApp.AppName + '.Variables.CurrentPath', TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/runner/'), 'Diretório atual');
      Self.JupiterApp.Config.AddVariable(Self.JupiterApp.AppName + '.Variables.CurrentFile', prParams.Params, 'Arquivo atual');

      vrIcon := ICON_FOLDER;

      vrStr.Add(EmptyStr);
      vrStr.Add(Format('%0:s;%1:s;', [TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/runner/'), 'Pasta de scripts']));

      vrIsDir := True;
    end;

    if prParams.Task = '/folders' then
    begin
      Self.JupiterApp.Config.AddVariable(Self.JupiterApp.AppName + '.Variables.CurrentPath', TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/runner/'), 'Diretório atual');
      Self.JupiterApp.Config.AddVariable(Self.JupiterApp.AppName + '.Variables.CurrentFile', prParams.Params, 'Arquivo atual');

      vrIcon := ICON_FOLDER;

      vrStr.LoadFromFile(TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/runner/folders.csv'));

      vrIsDir := True;
    end;

    if prParams.Task = '/applications' then
    begin
      Self.JupiterApp.Config.AddVariable(Self.JupiterApp.AppName + '.Variables.CurrentPath', TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/runner/'), 'Diretório atual');
      Self.JupiterApp.Config.AddVariable(Self.JupiterApp.AppName + '.Variables.CurrentFile', prParams.Params, 'Arquivo atual');

      vrStr.LoadFromFile(TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/runner/applications.csv'));

      vrIcon := ICON_PACKAGE;
    end;

    if vrStr.Count = 0 then
      Exit;

    for vrVez := 1 to vrStr.Count - 1 do
    begin
      vrStrAux.Clear;
      vrStrAux.Delimiter := ';';
      vrStrAux.DelimitedText := StringReplace(vrStr[vrVez], ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]);

      vrObj := TJupiterListableItem.Create();

      if vrIsDir then
        vrObj.Descricao := StringReplace(vrStrAux[0], EMPTY_SPACE_SEPARATOR, ' ', [rfReplaceAll, rfIgnoreCase])
      else
        vrObj.Descricao := ExtractFileName(StringReplace(vrStrAux[0], EMPTY_SPACE_SEPARATOR, ' ', [rfReplaceAll, rfIgnoreCase]));

      vrObj.ImageIndex := vrIcon;

      vrObj.Item  := StringReplace(vrStrAux[1], EMPTY_SPACE_SEPARATOR, ' ', [rfReplaceAll, rfIgnoreCase]);
      vrObj.Param := StringReplace(vrStrAux[0], EMPTY_SPACE_SEPARATOR, ' ', [rfReplaceAll, rfIgnoreCase]);

      vrObj.Descricao := vrJupiterApp.Config.ResolveString(vrObj.Descricao);
      vrObj.Item      := vrJupiterApp.Config.ResolveString(vrObj.Item);
      vrObj.Param     := vrJupiterApp.Config.ResolveString(vrObj.Param);

      prList.Add(vrObj);
    end;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);

    vrStrAux.Clear;
    FreeAndNil(vrStrAux);
  end;
end;

procedure TJupiterRunner.ListActions(prParams: TJupiterListem; var prList: TList);
begin
  if prParams.Task = '/scripts' then
  begin
    prList.Add(TJupiterRunnerNewScriptSQL.Create);

    prList.Add(TJupiterRunnerNewScriptBAT.Create);
  end;

  if prParams.Task = '/folders' then
    prList.Add(TJupiterRunnerNewItemPath.Create);

  if prParams.Task = '/applications' then
  prList.Add(TJupiterRunnerNewItemApp.Create);

  inherited ListActions(prParams, prList);
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

  vrNode.Data := TJupiterListem.Create(Self.ID, '/scripts', EmptyStr, 0, 'Listando agora o diretório de scripts. Dê um duplo clique para abrir.');

  Self.Internal_ListScripts(prTreeMenu, vrNode);

  vrNode.Expanded := True;

  vrNode               := prTreeMenu.Items.Add(nil, 'Favoritos');
  vrNode.ImageIndex    := ICON_FAVORITE;
  vrNode.SelectedIndex := ICON_FAVORITE;

  vrNode.Data := TJupiterListem.Create(Self.ID, '/fav', EmptyStr, 0, 'Arquivos de configuração favoritos. Dê um duplo clique para abrir.');

  vrSubNode := prTreeMenu.Items.AddChild(vrNode, 'Pastas');
  vrSubNode.ImageIndex    := ICON_FOLDER;
  vrSubNode.SelectedIndex := ICON_FOLDER;

  vrSubNode.Data := TJupiterListem.Create(Self.ID, '/folders', EmptyStr, 0, 'Pastas favoritas. Dê um duplo clique para abrir.');

  vrSubNode := prTreeMenu.Items.AddChild(vrNode, 'Executáveis');
  vrSubNode.ImageIndex    := ICON_PACKAGE;
  vrSubNode.SelectedIndex := ICON_PACKAGE;

  vrSubNode.Data := TJupiterListem.Create(Self.ID, '/applications', EmptyStr, 0, 'Aplicativos e comandos favoritos. Dê um duplo clique para abrir.');

  vrNode.Expanded := True;
end;

procedure TJupiterRunner.RunListable(var prParamsItem: TJupiterListem; var prParams: TJupiterListableItem);
begin
  inherited RunListable(prParamsItem, prParams);

  if DirectoryExists(prParams.Param) then
  begin
    Self.JupiterApp.Log.AddLog(Now, 'Runner', 'Abrindo pasta: ' + prParams.Param);

    OpenFolder(prParams.Param);
  end
  else
  begin
    Self.JupiterApp.Log.AddLog(Now, 'Runner', 'Abrindo executável: ' + prParams.Param);

    OpenFile(prParams.Param);
  end;
end;

end.

