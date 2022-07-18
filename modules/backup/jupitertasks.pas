unit JupiterTasks;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterModule, JupiterApp, JupiterConsts, fileUtils,
  uNewTask, ComCtrls, Forms, Dialogs;

type

  { TJupiterActionTasksNewTask }

  { TJupiterActionTasksNewTaskCheckList }

  { TJupiterActionTasksNewTaskScriptSQL }

  TJupiterActionTasksNewTaskScriptSQL = class(TJupiterAction)
  public
    constructor Create;

    procedure Run(prParams : TJupiterListem); override;
  end;

  { TJupiterActionTasksNewTaskScriptBAT }

  TJupiterActionTasksNewTaskScriptBAT = class(TJupiterAction)
  public
    constructor Create;

    procedure Run(prParams : TJupiterListem); override;
  end;

  TJupiterActionTasksNewTaskCheckList = class(TJupiterAction)
  public
    constructor Create;

    procedure Run(prParams : TJupiterListem); override;
  end;

  TJupiterActionTasksNewTask = class(TJupiterAction)
  public
    constructor Create;

    procedure Run(prParams : TJupiterListem); override;
  end;

  { TJupiterTaskDetails }

  { TJupiterTaskDetailsTimeNote }

  TJupiterTaskDetailsTimeNote = class(TObject)
  protected
    FStarted : Boolean;
    FPath    : String;
  published
    property Started : Boolean read FStarted;
  public
    procedure Start;
    procedure Finish;

    procedure GetTimes(var prList : TList);

    constructor Create(prPath : String);
  end;

  TJupiterTaskDetails = class(TObject)
  protected
    FPath     : String;
    FTimeNote : TJupiterTaskDetailsTimeNote;
  published
    property Path     : String                      read FPath;
    property TimeNote : TJupiterTaskDetailsTimeNote read FTimeNote write FTimeNote;
  public
    procedure ListTaskFiles(var prListOfFiles : TStrings);
    procedure ExecuteFile(prFilePath : String);

    constructor Create(prPath : String);
    destructor Destroy; override;
  end;

  { TJupiterTasks }

  TJupiterTasks = class(TJupiterModule)
  protected
    procedure Internal_Initialize; override;
    function  Internal_GetIdentifier: String; override;
  public
    procedure GetTasks(var prTreeMenu : TTreeView); override;

    procedure ListItems(var prParams : TJupiterListem; var prList : TList); override;
    procedure ListActions(prParams : TJupiterListem; var prList : TList); override;

    procedure RunListable(var prParamsItem: TJupiterListem; var prParams : TJupiterListableItem); override;
    function  CreateTaskDetail : TJupiterTaskDetails;
  end;

implementation

uses jupiterUtils, LCLIntf, uExplorer;

{ TJupiterActionTasksNewTaskScriptSQL }

constructor TJupiterActionTasksNewTaskScriptSQL.Create;
begin
  Self.Title      := 'Novo script SQL';
  Self.Hint       := 'Clique aqui para criar um novo arquivo de script SQL para a tarefa atual';
  Self.ImageIndex := ICON_SQL;
end;

procedure TJupiterActionTasksNewTaskScriptSQL.Run(prParams: TJupiterListem);
var
  vrFile : String;
begin
  InputQuery('Novo arquivo de script .SQL de Tarefa', 'Informe o nome do novo script SQL (sem extensão)', vrFile);

  if Trim(vrFile) = EmptyStr then
    Exit;

  NovoScriptSQLInTask(vrFile, vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.Current').Value);

  vrJupiterApp.Log.AddLog(Now, Self.ClassName, 'Criado script SQL: ' + vrFile);

  inherited Run(prParams);
end;

{ TJupiterActionTasksNewTaskScriptBAT }

constructor TJupiterActionTasksNewTaskScriptBAT.Create;
begin
  Self.Title      := 'Novo script BAT';
  Self.Hint       := 'Clique aqui para criar um novo arquivo de script BAT para a tarefa atual';
  Self.ImageIndex := ICON_SCRIPTS;
end;

procedure TJupiterActionTasksNewTaskScriptBAT.Run(prParams: TJupiterListem);
var
  vrFile : String;
begin
  InputQuery('Novo arquivo de script .BAT de Tarefa', 'Informe o nome do novo script BAT (sem extensão)', vrFile);

  if Trim(vrFile) = EmptyStr then
    Exit;

  NovoScriptBATInTask(vrFile, vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.Current').Value);

  vrJupiterApp.Log.AddLog(Now, Self.ClassName, 'Criado script BAT: ' + vrFile);

  inherited Run(prParams);
end;

{ TJupiterActionTasksNewTaskCheckList }

constructor TJupiterActionTasksNewTaskCheckList.Create;
begin
  Self.Title      := 'Nova Checklist';
  Self.Hint       := 'Clique aqui para criar um novo arquivo de checklist para a Tarefa atual';
  Self.ImageIndex := ICON_CHECKLIST;
end;

procedure TJupiterActionTasksNewTaskCheckList.Run(prParams: TJupiterListem);
var
  vrFile : String;
begin
  InputQuery('Nova Checklist de Tarefa', 'Informe o nome da nova checklist (sem extensão)', vrFile);

  if Trim(vrFile) = EmptyStr then
    Exit;

  NovaChecklistInTask(vrFile, vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.Current').Value);

  vrJupiterApp.Log.AddLog(Now, Self.ClassName, 'Criada checklist: ' + vrFile);

  inherited Run(prParams);
end;

{ TJupiterActionTasksNewTask }

constructor TJupiterActionTasksNewTask.Create;
begin
  Self.Title      := 'Nova Tarefa';
  Self.Hint       := 'Clique aqui para criar uma nova tarefa';
  Self.ImageIndex := ICON_CURRENTTASK;
end;

procedure TJupiterActionTasksNewTask.Run(prParams: TJupiterListem);
begin
  Application.CreateForm(TFNewTask, FNewTask);
  try
    FNewTask.ShowModal;
  finally
    FNewTask.Release;

    FreeAndNil(FNewTask);
  end;

  inherited Run(prParams);
end;

{ TJupiterTaskDetailsTimeNote }

procedure TJupiterTaskDetailsTimeNote.Start;
var
  vrStr : TStrings;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;

    if FileExists(Self.FPath + 'Tempos.txt') then
      vrStr.LoadFromFile(Self.FPath + 'Tempos.txt');

    if FileExists(Self.FPath + 'Tempos.txt') then
      vrStr.Add(EmptyStr);

    vrStr.Add(Format('I;%0:s;%1:s;', [FormatDateTime('dd/mm/yyyy', Now), FormatDateTime('hh:nn', Now)]));

    vrStr.SaveToFile(Self.FPath + 'Tempos.txt');
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterTaskDetailsTimeNote.Finish;
var
  vrStr : TStrings;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;

    if FileExists(Self.FPath + 'Tempos.txt') then
      vrStr.LoadFromFile(Self.FPath + 'Tempos.txt');

    vrStr.Add(Format('F;%0:s;%1:s;', [FormatDateTime('dd/mm/yyyy', Now), FormatDateTime('hh:nn', Now)]));

    vrStr.SaveToFile(Self.FPath + 'Tempos.txt');
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterTaskDetailsTimeNote.GetTimes(var prList: TList);
var
  vrStr : TStrings;
  vrVez : Integer;
  vrObj : TJupiterListableItem;
begin
  prList.Clear;

  if Trim(Self.FPath) = EmptyStr then
    Exit;

  if not FileExists(Self.FPath + 'Tempos.txt') then
    Exit;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.LoadFromFile(Self.FPath + 'Tempos.txt');

    vrObj := Nil;

    Self.FStarted := False;

    for vrVez := 0 to vrStr.Count -1 do
    begin
      if Trim(vrStr[vrVez]) = EmptyStr then
        Continue;

      if Copy(Trim(vrStr[vrVez]), 1, 1) = 'I' then
      begin
        vrObj := TJupiterListableItem.Create;
        vrObj.SubItens.Add(vrStr[vrVez]);

        Self.FStarted := True;
      end;

      if not Assigned(vrObj) then
        Continue;

      if Copy(Trim(vrStr[vrVez]), 1, 1) = 'F' then
      begin
        vrObj.SubItens.Add(vrStr[vrVez]);
        prList.Add(vrObj);

        vrObj := Nil;

        Self.FStarted := False;
      end;
    end;

    if Assigned(vrObj) then
      prList.Add(vrObj);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

constructor TJupiterTaskDetailsTimeNote.Create(prPath: String);
begin
  Self.FStarted := False;
  Self.FPath    := prPath;
end;

{ TJupiterTaskDetails }

procedure TJupiterTaskDetails.ListTaskFiles(var prListOfFiles: TStrings);
var
  vrSubDiretorios : TStrings;
  vrFiles : TStrings;
  vrVez : Integer;
begin
  prListOfFiles.Clear;

  ListFiles(Self.FPath, prListOfFiles);

  vrSubDiretorios := TStringList.Create;
  vrFiles := TStringList.Create;
  try
    vrSubDiretorios.Clear;

    ListDirectories(Self.FPath, vrSubDiretorios);

    for vrVez := 0 to vrSubDiretorios.Count -1 do
    begin
      vrFiles.Clear;

      ListFiles(vrSubDiretorios[vrVez], vrFiles);

      prListOfFiles.AddStrings(vrFiles);
    end;
  finally
    vrSubDiretorios.Clear;
    FreeAndNil(vrSubDiretorios);

    vrFiles.Clear;
    FreeAndNil(vrFiles);
  end;
end;

procedure TJupiterTaskDetails.ExecuteFile(prFilePath: String);
begin
  OpenFile(prFilePath);
end;

constructor TJupiterTaskDetails.Create(prPath: String);
begin
  Self.FPath := prPath;

  Self.FTimeNote := TJupiterTaskDetailsTimeNote.Create(prPath);
end;

destructor TJupiterTaskDetails.Destroy;
begin
  FreeAndNil(Self.FTimeNote);

  inherited Destroy;
end;

{ TJupiterTasks }

procedure TJupiterTasks.Internal_Initialize;
begin
  inherited Internal_Initialize;

  if not DirectoryExists(TratarCaminho(ExtractFileDir(Application.ExeName) + '/modules/tasks/')) then
     CreateDir(TratarCaminho(ExtractFileDir(Application.ExeName) + '/modules/tasks/'));

  if not DirectoryExists(TratarCaminho(ExtractFileDir(Application.ExeName) + '/modules/tasks/templates/')) then
     CreateDir(TratarCaminho(ExtractFileDir(Application.ExeName) + '/modules/tasks/templates'));

  if not Self.JupiterApp.Config.Exists(Self.ID + '.Path') then

     {$IFDEF WINDOWS}
          Self.JupiterApp.Config.AddConfig(Self.ID + '.Path', 'C:\Tarefas\', 'Diretório de tarefas');
     {$ELSE}
          Self.JupiterApp.Config.AddConfig(Self.ID + '.Path', '/home/Tarefas/', 'Diretório de tarefas');
     {$ENDIF}

  if not DirectoryExists(Self.JupiterApp.Config.GetByID(Self.ID + '.Path').Value) then
     CreateDir(Self.JupiterApp.Config.GetByID(Self.ID + '.Path').Value);

  if not Self.JupiterApp.Config.Exists(Self.ID + '.OpenInEditorPrefExtensions') then
     Self.JupiterApp.Config.AddConfig(Self.ID + '.OpenInEditorPrefExtensions', '.txt|.sql|.md', 'Extensões de arquivos abertas em editor preferencial');

  if not Self.JupiterApp.Config.Exists(Self.ID + '.EditorPref') then
     Self.JupiterApp.Config.AddConfig(Self.ID + '.EditorPref', 'notepad', 'Editor preferencial (caminho ou comando)');

  if not Self.JupiterApp.Config.Exists(Self.ID + '.DoNotChangeContentExtensions') then
     Self.JupiterApp.Config.AddConfig(Self.ID + '.DoNotChangeContentExtensions', '.doc|.docx|.pdf', 'Extensões de arquivos que não terão seu conteúdo alterado');
end;

function TJupiterTasks.Internal_GetIdentifier: String;
begin
  Result := 'JupiterTools.Modules.Tasks';
end;

procedure TJupiterTasks.GetTasks(var prTreeMenu: TTreeView);
var
  vrNode : TTreeNode;
  vrSubNode : TTreeNode;
  vrSubCurrent : TTreeNode;
begin
  inherited GetTasks(prTreeMenu);

  vrNode               := prTreeMenu.Items.Add(nil, 'Tarefas');
  vrNode.ImageIndex    := ICON_CURRENTTASK;
  vrNode.SelectedIndex := ICON_CURRENTTASK;

  vrNode.Data := TJupiterListem.Create(Self.ID, '/', EmptyStr, 0, 'Tarefas já cadastradas. Dê um duplo clique para selecionar a tarefa atual.');

  if Self.JupiterApp.Config.Exists(Self.ID + '.CurrentNumber') then
  begin
    vrSubNode := prTreeMenu.Items.AddChild(vrNode, 'Atual: ' + Self.JupiterApp.Config.GetByID(Self.ID + '.CurrentNumber').Value);

    vrSubNode.ImageIndex    := ICON_CURRENTTASK;
    vrSubNode.SelectedIndex := ICON_CURRENTTASK;

    vrSubNode.Data := TJupiterListem.Create(Self.ID, '/current', EmptyStr, 90);

    if Self.JupiterApp.Config.Exists(Self.ID + '.Current') then
    begin
      vrSubCurrent := prTreeMenu.Items.AddChild(vrSubNode, 'Checklists');
      vrSubCurrent.ImageIndex    := ICON_CHECKLIST;
      vrSubCurrent.SelectedIndex := ICON_CHECKLIST;
      vrSubCurrent.Data := TJupiterListem.Create(Self.ID, '/current/checklists', EmptyStr, 0, 'Checklists da tarefa atual.');

      vrSubCurrent := prTreeMenu.Items.AddChild(vrSubNode, 'Scripts');
      vrSubCurrent.ImageIndex    := ICON_SCRIPTS;
      vrSubCurrent.SelectedIndex := ICON_SCRIPTS;
      vrSubCurrent.Data := TJupiterListem.Create(Self.ID, '/current/scripts', EmptyStr, 0, 'Scripts da tarefa atual.');
    end;
  end;

  vrNode.Expand(True);
end;

procedure TJupiterTasks.ListItems(var prParams: TJupiterListem; var prList: TList);
var
  vrClientList : TStrings;
  vrTasks      : TStrings;

  vrVez : Integer;
  vrVez2 : Integer;

  vrObj : TJupiterListableItem;

  vrClient : String;
  vrTask : String;
begin
  inherited ListItems(prParams, prList);

  prList.Clear;

  vrClientList := TStringList.Create;
  vrTasks      := TStringList.Create;
  try
    vrClientList.Clear;
    vrTasks.Clear;

    if prParams.Task = '/' then
    begin
      Self.JupiterApp.Config.AddVariable(Self.JupiterApp.AppName + '.Variables.CurrentPath', Self.JupiterApp.Config.GetByID(Self.ID + '.Path').Value, 'Diretório atual');
      Self.JupiterApp.Config.AddVariable(Self.JupiterApp.AppName + '.Variables.CurrentFile', EmptyStr, 'Arquivo atual');

      ListDirectories(Self.JupiterApp.Config.GetByID(Self.ID + '.Path').Value, vrClientList);

      for vrVez := 0 to vrClientList.Count - 1 do
      begin
        vrTasks.Clear;

        vrClient := StringReplace(vrClientList[vrVez], Self.JupiterApp.Config.GetByID(Self.ID + '.Path').Value, EmptyStr, [rfIgnoreCase, rfReplaceAll]);
        vrClient := StringReplace(vrClient, GetDirectorySeparator, EmptyStr, [rfIgnoreCase, rfReplaceAll]);

        ListDirectories(vrClientList[vrVez], vrTasks);

        for vrVez2 := 0 to vrTasks.Count - 1 do
        begin
          vrTask := StringReplace(vrTasks[vrVez2], vrClientList[vrVez], EmptyStr, [rfIgnoreCase, rfReplaceAll]);
          vrTask := StringReplace(vrTask, GetDirectorySeparator, EmptyStr, [rfIgnoreCase, rfReplaceAll]);

          vrObj             := TJupiterListableItem.Create();
          vrObj.Item        := vrTask;
          vrObj.Descricao   := Format('Cliente/Projeto: %0:s, Tarefa: %1:s', [vrClient, vrTask]);
          vrObj.Param       := vrTasks[vrVez2];
          vrObj.Selecionado := False;
          vrObj.Tag         := 0;

          if Self.JupiterApp.Config.Exists(Self.ID + '.Current') then
            vrObj.Selecionado := Self.JupiterApp.Config.GetByID(Self.ID + '.Current').Value = vrTasks[vrVez2];

          prList.Add(vrObj);
        end;
      end;
    end;

    if prParams.Task = '/current/checklists' then
    begin
      Self.JupiterApp.Config.AddVariable(Self.JupiterApp.AppName + '.Variables.CurrentPath', TratarCaminho(Self.JupiterApp.Config.GetByID(Self.ID + '.Current').Value + '\Arquivos\'), 'Diretório atual');
      Self.JupiterApp.Config.AddVariable(Self.JupiterApp.AppName + '.Variables.CurrentFile', EmptyStr, 'Arquivo atual');

      ListFiles(TratarCaminho(Self.JupiterApp.Config.GetByID(Self.ID + '.Current').Value + '\Arquivos\'), vrClientList);

      for vrVez := 0 to vrClientList.Count - 1 do
      begin
        if AnsiUpperCase(ExtractFileExt(vrClientList[vrVez])) <> '.CKL' then
          Continue;

        vrObj             := TJupiterListableItem.Create();
        vrObj.Item        := ExtractFileName(vrClientList[vrVez]);
        vrObj.Descricao   := vrClientList[vrVez];
        vrObj.Param       := vrClientList[vrVez];
        vrObj.ImageIndex  := ICON_CHECKLIST;
        vrObj.Tag         := 1;

        prList.Add(vrObj);
      end;
    end;

    if prParams.Task = '/current/scripts' then
    begin
      Self.JupiterApp.Config.AddVariable(Self.FAppName + '.Variables.CurrentPath', TratarCaminho(Self.JupiterApp.Config.GetByID(Self.ID + '.Current').Value + '\Arquivos\'), 'Diretório atual');
      Self.JupiterApp.Config.AddVariable(Self.FAppName + '.Variables.CurrentFile', EmptyStr, 'Arquivo atual');

      ListFiles(TratarCaminho(Self.JupiterApp.Config.GetByID(Self.ID + '.Current').Value + '\Arquivos\'), vrClientList);

      for vrVez := 0 to vrClientList.Count - 1 do
      begin
        if ((AnsiUpperCase(ExtractFileExt(vrClientList[vrVez])) <> '.SQL') and (AnsiUpperCase(ExtractFileExt(vrClientList[vrVez])) <> '.BAT')) then
          Continue;

        vrObj             := TJupiterListableItem.Create();
        vrObj.Item        := ExtractFileName(vrClientList[vrVez]);
        vrObj.Descricao   := vrClientList[vrVez];
        vrObj.Param       := vrClientList[vrVez];

        if AnsiUpperCase(ExtractFileExt(vrClientList[vrVez])) = '.SQL' then
          vrObj.ImageIndex  := ICON_SQL
        else
          vrObj.ImageIndex  := ICON_SCRIPTS;

        vrObj.Tag         := 2;

        prList.Add(vrObj);
      end;
    end;
  finally
    vrClientList.Clear;
    FreeAndNil(vrClientList);

    vrTasks.Clear;
    FreeAndNil(vrTasks);
  end;
end;

procedure TJupiterTasks.ListActions(prParams: TJupiterListem; var prList: TList);
begin
  if prParams.Task = '/' then
    prList.Add(TJupiterActionTasksNewTask.Create);

  if prParams.Task = '/current/checklists' then
    prList.Add(TJupiterActionTasksNewTaskCheckList.Create);

  if prParams.Task = '/current/scripts' then
  begin
    prList.Add(TJupiterActionTasksNewTaskScriptBAT.Create);

    prList.Add(TJupiterActionTasksNewTaskScriptSQL.Create);
  end;

  inherited ListActions(prParams, prList);
end;

procedure TJupiterTasks.RunListable(var prParamsItem: TJupiterListem; var prParams: TJupiterListableItem);
var
  vrExplorer : TFExplorer;
begin
  inherited RunListable(prParamsItem, prParams);

  if prParams.Tag = 0 then
  begin
    Self.JupiterApp.Config.AddConfig(Self.ID + '.Current', prParams.Param, 'Tarefa atual');
    Self.JupiterApp.Config.AddConfig(Self.ID + '.CurrentNumber', prParams.Item, 'Número Tarefa atual');
  end;

  if prParams.Tag = 1 then
  begin
    Application.CreateForm(TFExplorer, vrExplorer);
    try
      vrExplorer.Params := TJupiterListem.Create('JupiterTools.Modules.Checklist', '/checklistFile', prParams.Param, 0, 'Dê um duplo clique para marcar/desmarcar a opção selecionada.');

      if Self.JupiterApp.Config.GetByID('JupiterTools.UI.Display.WindowsState').Value = 'Maximized' then
        vrExplorer.WindowState := wsMaximized;

      vrExplorer.Caption := ExtractFileName(prParams.Param) + ' - Checklist de Tarefa';
      vrExplorer.ShowModal;
    finally
      vrExplorer.Release;
      FreeAndNil(vrExplorer);
    end;
  end;

  if prParams.Tag = 2 then
  begin
    vrJupiterApp.Log.AddLog(Now, Self.ClassName, 'Abrindo arquivo: ' + prParams.Param);

    Self.CreateTaskDetail.ExecuteFile(prParams.Param);
  end;
end;

function TJupiterTasks.CreateTaskDetail: TJupiterTaskDetails;
begin
  if Self.JupiterApp.Config.Exists(Self.ID + '.Current') then
    Result := TJupiterTaskDetails.Create(Self.JupiterApp.Config.GetByID(Self.ID + '.Current').Value)
  else
    Result := TJupiterTaskDetails.Create(EmptyStr);
end;

end.

