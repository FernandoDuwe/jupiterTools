unit JupiterToolsModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterModule, JupiterRoute, JupiterObject, JupiterAction,
  JupiterConsts, JupiterApp, JupiterEnviroment, JupiterCSVDataProvider,
  JupiterDirectoryDataProvider, JupiterFileDataProvider, SysUtils;

type

  { TJupiterToolsModule }

  TJupiterToolsModule = class(TJupiterModule)
  protected
    procedure Internal_Prepare; override;
    function Internal_GetModuleID : String; override;
    function Internal_GetModuleTitle : String; override;

    procedure Internal_OnChangeCurrentTask(prID, prNewValue : String);
    procedure Internal_ListLibrary(prDir : String; var prList : TJupiterObjectList);
    procedure Internal_ListChecklists(var prList : TJupiterObjectList);
  public
    function GetActions(prRoute : TJupiterRoute) : TJupiterObjectList; override;

    function  CreateTask(prClient, prTaskName : String; prAsCurrentTask : Boolean) : String;
    procedure CreatePathScruture(prPath : String);
    procedure CopyFileFromTemplate(prPath: String; prFile: String);
    procedure SetStartTime;
    procedure SetEndTime;
    procedure ClearTime;
    function  StartedTime : Boolean;
    procedure SetCurrentTask(prClient, prTaskName, prCompletePath : String);
  end;

implementation

uses uExplorer, StrUtils;

{ TJupiterToolsModule }

procedure TJupiterToolsModule.Internal_Prepare;
var
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Internal_Prepare;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.CreatePath('modules/tools');
    vrEnviroment.CreatePath('modules/tools/checklists/');
    vrEnviroment.CreatePath('modules/tools/favorites/');
    vrEnviroment.CreatePath('modules/tools/library/');
    vrEnviroment.CreatePath('modules/tools/templates/');

    if not Self.Params.Exists(Self.DefineParamName('Tasks.Path')) then
       Self.Params.AddConfig(Self.DefineParamName('Tasks.Path'), vrEnviroment.CreatePath('Tarefas/'), 'Diretório de tarefas');

    if not Self.Params.Exists(Self.DefineParamName('Tasks.Current.Client')) then
      Self.Params.AddConfig(Self.DefineParamName('Tasks.Current.Client'), EmptyStr, 'Cliente da tarefa atual');

    if not Self.Params.Exists(Self.DefineParamName('Tasks.Current.Number')) then
      Self.Params.AddConfig(Self.DefineParamName('Tasks.Current.Number'), '0', 'Número da tarefa atual');

    if not Self.Params.Exists(Self.DefineParamName('Tasks.Current.Path')) then
      Self.Params.AddConfig(Self.DefineParamName('Tasks.Current.Path'), EmptyStr, 'Diretório da tarefa atual');

    Self.Params.VariableById(Self.DefineParamName('Tasks.Current.Path')).OnChangeValue := @Internal_OnChangeCurrentTask;

    vrEnviroment.CreateFile('modules/tools/favorites/applications.csv', 'Descrição;Caminho;');
    vrEnviroment.CreateFile('modules/tools/favorites/folders.csv', 'Descrição;Caminho;');
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

function TJupiterToolsModule.Internal_GetModuleID: String;
begin
  Result := 'Jupiter.Tools';
end;

function TJupiterToolsModule.Internal_GetModuleTitle: String;
begin
  Result := 'Tools';
end;

procedure TJupiterToolsModule.Internal_OnChangeCurrentTask(prID, prNewValue: String);
var
  vrValue : String;
begin
  vrValue := StringReplace(prNewValue, Self.Params.VariableById(Self.DefineParamName('Tasks.Path')).Value, EmptyStr, [rfIgnoreCase, rfReplaceAll]);
  vrValue := StringReplace(vrValue, GetDirectorySeparator, ';', [rfReplaceAll, rfIgnoreCase]);

  Self.SetCurrentTask(GetCSVColumn(vrValue, 0), GetCSVColumn(vrValue, 1), prNewValue);
end;

procedure TJupiterToolsModule.Internal_ListLibrary(prDir : String; var prList: TJupiterObjectList);
var
  vrPath       : TJupiterDirectoryDataProvider;
  vrFile       : TJupiterFileDataProvider;
  vrEnviroment : TJupiterEnviroment;
  vrVez        : Integer;
begin
  vrPath       := TJupiterDirectoryDataProvider.Create;
  vrFile       := TJupiterFileDataProvider.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrPath.Path := vrEnviroment.FullPath('modules/tools/library/' + IfThen(prDir <> EmptyStr, prDir, EmptyStr));
    vrFile.Path := vrEnviroment.FullPath('modules/tools/library/' + IfThen(prDir <> EmptyStr, prDir, EmptyStr));

    vrFile.ProvideData;
    vrPath.ProvideData;

    for vrVez := 0 to vrPath.Size - 1 do
      with vrPath.GetRowByIndex(vrVez) do
      begin
        prList.Add(TJupiterAction.Create(Fields.VariableById('Folder').Value, TJupiterRoute.Create('/tools/library/' + IfThen(prDir <> EmptyStr, prDir, EmptyStr) + Fields.VariableById('Folder').Value + '/'), TJupiterRoute.Create('/tools/library/' + IfThen(prDir <> EmptyStr, prDir, EmptyStr))));

        with TJupiterAction(prList.GetLastObject) do
        begin
          Icon := ICON_OPEN;

          Route.Params.AddVariable('title', 'Favoritos: ' + Fields.VariableById('Folder').Value, 'Título');
          Route.Params.AddVariable('type', DATAPROVIDER_TYPE_LIST_FILES, 'Tipo');
          Route.Params.AddVariable('path', vrEnviroment.FullPath('modules/tools/library/' + IfThen(prDir <> EmptyStr, prDir, EmptyStr) + Fields.VariableById('Folder').Value + '/'), 'Diretório');

          Route.Params.AddVariable('runnableField', 'File', 'Campo a ser executado');
          Route.Params.AddVariable('destinyPath', EXPLORER_FORM_PATH, 'Destino');
        end;

        Self.Internal_ListLibrary(prDir + Fields.VariableById('Folder').Value + '/', prList);
      end;

    for vrVez := 0 to vrFile.Size - 1 do
      with vrFile.GetRowByIndex(vrVez) do
      begin
        prList.Add(TJupiterAction.Create(Fields.VariableById('FieldName').Value, TJupiterRoute.Create('/tools/library/' + IfThen(prDir <> EmptyStr, prDir, EmptyStr) + Fields.VariableById('FieldName').Value + '/'), TJupiterRoute.Create('/tools/library/' + IfThen(prDir <> EmptyStr, prDir, EmptyStr))));

        with TJupiterAction(prList.GetLastObject) do
        begin
          Icon := vrEnviroment.IconOfFile(Fields.VariableById('File').Value);

          Route.Params.AddVariable('title', 'Arquivo: ' + Fields.VariableById('FieldName').Value, 'Título');
          Route.Params.AddVariable('filename', Fields.VariableById('File').Value, 'Arquivo');
          Route.Params.AddVariable('destinyPath', EDITOR_FORM_PATH, 'Destino');
        end;
      end;
  finally
    FreeAndNil(vrPath);
    FreeAndNil(vrFile);
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterToolsModule.Internal_ListChecklists(var prList: TJupiterObjectList);
var
  vrFile       : TJupiterFileDataProvider;
  vrEnviroment : TJupiterEnviroment;
  vrVez        : Integer;
begin
  vrFile       := TJupiterFileDataProvider.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrFile.Path := vrEnviroment.FullPath('modules/tools/checklists/');
    vrFile.ProvideData;

    for vrVez := 0 to vrFile.Size - 1 do
      with vrFile.GetRowByIndex(vrVez) do
      begin
        if not vrEnviroment.IsOfExtension(Fields.VariableById('File').Value, '.ckl') then
          Continue;

        prList.Add(TJupiterAction.Create(Fields.VariableById('FieldName').Value, TJupiterRoute.Create('/tools/checklists/' + Fields.VariableById('FieldName').Value + '/'), TJupiterRoute.Create('/tools/checklists/')));

        with TJupiterAction(prList.GetLastObject) do
        begin
          Icon := vrEnviroment.IconOfFile(Fields.VariableById('File').Value);

          Route.Params.AddVariable('title', 'Checklist: ' + Fields.VariableById('FieldName').Value, 'Título');
          Route.Params.AddVariable('type', DATAPROVIDER_TYPE_LIST_CSV, 'Tipo');
          Route.Params.AddVariable('path', Fields.VariableById('File').Value, 'Arquivo');
          Route.Params.AddVariable('checklistField', 'Checked', 'Campo de checklist');
          Route.Params.AddVariable('hideColumns', 'Checked, Line', 'Campos a esconder');

          Route.Params.AddVariable('filename', Fields.VariableById('File').Value, 'Nome do arquivo');
          Route.Params.AddVariable('destinyPath', EXPLORER_FORM_PATH, 'Destino');
        end;
      end;
  finally
    FreeAndNil(vrFile);
    FreeAndNil(vrEnviroment);
  end;
end;

function TJupiterToolsModule.GetActions(prRoute: TJupiterRoute): TJupiterObjectList;
var
  vrEnviroment : TJupiterEnviroment;
begin
  Result := inherited GetActions(prRoute);

  vrEnviroment := TJupiterEnviroment.Create();
  try
    Result.Add(TJupiterAction.Create('Tarefas', TJupiterRoute.Create('/tasks/'), TJupiterRoute.Create('/')));

    with TJupiterAction(Result.GetLastObject) do
    begin
      Icon := ICON_TASKS;
      Route.Params.AddVariable('title', 'Minhas tarefas', 'Título');
      Route.Params.AddVariable('type', DATAPROVIDER_TYPE_TASKS, 'Tipo');
      Route.Params.AddVariable('checkableField',    'Path');
      Route.Params.AddVariable('checkableVariable', Self.DefineParamName('Tasks.Current.Path'));
      Route.Params.AddVariable('destinyPath', EXPLORER_FORM_PATH, 'Destino');
      Route.Params.AddVariable('hint', 'Dê um duplo clique para alterar a tarefa atual', 'Dica');
    end;

    if ((Self.Params.Exists(Self.DefineParamName('Tasks.Current.Path'))) and
        (Self.Params.VariableById(Self.DefineParamName('Tasks.Current.Path')).Value <> EmptyStr)) then
    begin
      Result.Add(TJupiterAction.Create('Atual', TJupiterRoute.Create(TASK_FORM_PATH), TJupiterRoute.Create('/tasks/')));

      with TJupiterAction(Result.GetLastObject) do
        Icon := ICON_CURTASK;
    end;

    Result.Add(TJupiterAction.Create('Nova Tarefa', TJupiterRoute.Create(NEWTASK_FORM_PATH), TJupiterRoute.Create('/records/')));

    with TJupiterAction(Result.GetLastObject) do
      Icon := ICON_ADD;

    Result.Add(TJupiterAction.Create('Biblioteca', TJupiterRoute.Create('/tools/library/'), TJupiterRoute.Create('/tools/')));

    with TJupiterAction(Result.GetLastObject) do
    begin
      Icon := ICON_LIBRARY;

      Route.Params.AddVariable('title', 'Biblioteca', 'Título');
      Route.Params.AddVariable('type', DATAPROVIDER_TYPE_LIST_FILES, 'Tipo');
      Route.Params.AddVariable('path', vrEnviroment.FullPath('modules/tools/library/'), 'Diretório');

      Route.Params.AddVariable('runnableField', 'File', 'Campo a ser executado');
      Route.Params.AddVariable('destinyPath', EXPLORER_FORM_PATH, 'Destino');
    end;

    Self.Internal_ListLibrary(EmptyStr, Result);

    Result.Add(TJupiterAction.Create('Checklists', TJupiterRoute.Create('/tools/checklists/'), TJupiterRoute.Create('/tools/')));

    with TJupiterAction(Result.GetLastObject) do
    begin
      Icon := ICON_CHECK;

      Route.Params.AddVariable('title', 'Checklist', 'Título');
      Route.Params.AddVariable('type', DATAPROVIDER_TYPE_LIST_FILES, 'Tipo');
      Route.Params.AddVariable('path', vrEnviroment.FullPath('modules/tools/checklists/'), 'Diretório');

      Route.Params.AddVariable('runnableField', 'File', 'Campo a ser executado');
      Route.Params.AddVariable('destinyPath', EXPLORER_FORM_PATH, 'Destino');
    end;

    Self.Internal_ListChecklists(Result);

    Result.Add(TJupiterAction.Create('Favoritos', TJupiterRoute.Create('/tools/favorites/'), TJupiterRoute.Create('/tools/')));

    with TJupiterAction(Result.GetLastObject) do
    begin
      Icon := ICON_FAVORITE;

      Route.Params.AddVariable('title', 'Meus favoritos', 'Título');
      Route.Params.AddVariable('type', DATAPROVIDER_TYPE_LIST_FILES, 'Tipo');
      Route.Params.AddVariable('path', vrEnviroment.FullPath('modules/tools/favorites/'), 'Diretório');

      Route.Params.AddVariable('itemIcon', IntToStr(ICON_CONFIG), 'Ícone');
      Route.Params.AddVariable('runnableField', 'File', 'Campo a ser executado');
      Route.Params.AddVariable('destinyPath', EXPLORER_FORM_PATH, 'Destino');
      Route.Params.AddVariable('hint', 'Arquivos de configuração de favoritos. Dê um duplo clique para abrir', 'Dica');
    end;

    Result.Add(TJupiterAction.Create('Arquivos', TJupiterRoute.Create('/tools/favorites/apps/'), TJupiterRoute.Create('/tools/favorites/')));

    with TJupiterAction(Result.GetLastObject) do
    begin
      Icon := ICON_APPLICATION;

      Route.Params.AddVariable('type', DATAPROVIDER_TYPE_LIST_CSV, 'Tipo');
      Route.Params.AddVariable('path', vrEnviroment.FullPath('modules/tools/favorites/applications.csv'), 'Arquivo');

      Route.Params.AddVariable('itemIcon', IntToStr(ICON_APPLICATION), 'Ícone');
      Route.Params.AddVariable('runnableField', 'Caminho', 'Campo a ser executado');
      Route.Params.AddVariable('destinyPath', EXPLORER_FORM_PATH, 'Destino');
      Route.Params.AddVariable('hint', 'Meus aplicativos favoritos, para alterar, clique em Favoritos, no menu principal', 'Dica');
      Route.Params.AddVariable('hideColumns', 'Checked, Line', 'Campos a esconder');
      Route.Params.AddVariable(FIELD_ID_GENERADOR, 'FavoriteAppsForm', 'ID do formulário');
    end;

    Result.Add(TJupiterAction.Create('Diretórios', TJupiterRoute.Create('/tools/favorites/folders/'), TJupiterRoute.Create('/tools/favorites/')));

    with TJupiterAction(Result.GetLastObject) do
    begin
      Icon := ICON_OPEN;

      Route.Params.AddVariable('type', DATAPROVIDER_TYPE_LIST_CSV, 'Tipo');
      Route.Params.AddVariable('path', vrEnviroment.FullPath('modules/tools/favorites/folders.csv'), 'Arquivo');

      Route.Params.AddVariable('itemIcon', IntToStr(ICON_OPEN), 'Ícone');
      Route.Params.AddVariable('runnableField', 'Caminho', 'Campo a ser executado');
      Route.Params.AddVariable('destinyPath', EXPLORER_FORM_PATH, 'Destino');
      Route.Params.AddVariable('hint', 'Minhas pastas favoritas, para alterar, clique em Favoritos, no menu principal', 'Dica');
      Route.Params.AddVariable('hideColumns', 'Checked, Line', 'Campos a esconder');
      Route.Params.AddVariable(FIELD_ID_GENERADOR, 'FavoritePathsForm', 'ID do formulário');
    end;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

function TJupiterToolsModule.CreateTask(prClient, prTaskName: String; prAsCurrentTask : Boolean): String;
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create();
  try
    vrEnviroment.BasePath := Self.Params.VariableById(Self.DefineParamName('Tasks.Path')).Value;

    vrEnviroment.BasePath := vrEnviroment.CreatePath(prClient);
    Result := vrEnviroment.CreatePath(prTaskName);

    if prAsCurrentTask then
       Self.Params.AddConfig(Self.DefineParamName('Tasks.Current.Path'), Result, 'Diretório da tarefa atual');
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterToolsModule.CreatePathScruture(prPath: String);
var
  vrPathProvider : TJupiterDirectoryDataProvider;
  vrEnviroment   : TJupiterEnviroment;
  vrVez          : Integer;
begin
  vrPathProvider := TJupiterDirectoryDataProvider.Create;
  vrEnviroment   := TJupiterEnviroment.Create;
  try
    vrPathProvider.Path := vrEnviroment.FullPath('modules/tools/templates/');
    vrPathProvider.ProvideData;

    vrEnviroment.BasePath := prPath;

    for vrVez := 0 to vrPathProvider.Size - 1 do
      with vrPathProvider.GetRowByIndex(vrVez) do
        vrEnviroment.CreatePath(Fields.VariableById('Folder').Value);
  finally
    FreeAndNil(vrPathProvider);
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterToolsModule.CopyFileFromTemplate(prPath: String; prFile: String);
var
  vrEnviroment : TJupiterEnviroment;
  vrDestiny    : String;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.BasePath := vrEnviroment.FullPath('modules/tools/templates/');
    vrDestiny             := prPath + DirectorySeparator + vrJupiterApp.Params.ResolveString(prFile);

    if not FileExists(vrDestiny) then
      vrEnviroment.CopyFileTo(vrEnviroment.FullPath(prFile), vrDestiny);

    if FileExists(vrDestiny) then
      vrJupiterApp.Params.ResolveFile(vrDestiny);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterToolsModule.SetStartTime;
var
  vrStr        : TStrings;
  vrEnviroment : TJupiterEnviroment;
begin
  vrStr        := TStringList.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.BasePath :=  Self.Params.VariableById(Self.DefineParamName('Tasks.Current.Path')).Value;

    if FileExists(vrEnviroment.FullPath('Tempos.txt')) then
      vrStr.LoadFromFile(vrEnviroment.FullPath('Tempos.txt'));

    if vrStr.Count > 0 then
      vrStr.Add(EmptyStr);

    vrStr.Add(Format('I;%0:s;%1:s;', [FormatDateTime('dd/mm/yyyy', Now), FormatDateTime('hh:nn', Now)]));

    vrStr.SaveToFile(vrEnviroment.FullPath('Tempos.txt'));
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);

    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterToolsModule.SetEndTime;
var
  vrStr        : TStrings;
  vrEnviroment : TJupiterEnviroment;
begin
  vrStr        := TStringList.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.BasePath :=  Self.Params.VariableById(Self.DefineParamName('Tasks.Current.Path')).Value;

    if FileExists(vrEnviroment.FullPath('Tempos.txt')) then
      vrStr.LoadFromFile(vrEnviroment.FullPath('Tempos.txt'));

    vrStr.Add(Format('F;%0:s;%1:s;', [FormatDateTime('dd/mm/yyyy', Now), FormatDateTime('hh:nn', Now)]));

    vrStr.SaveToFile(vrEnviroment.FullPath('Tempos.txt'));
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);

    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterToolsModule.ClearTime;
var
  vrStr        : TStrings;
  vrEnviroment : TJupiterEnviroment;
  vrVez        : Integer;
begin
  vrStr        := TStringList.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.BasePath := Self.Params.VariableById(Self.DefineParamName('Tasks.Current.Path')).Value;

    if FileExists(vrEnviroment.FullPath('Tempos.txt')) then
      vrStr.LoadFromFile(vrEnviroment.FullPath('Tempos.txt'))
    else
      Exit;

    vrStr.Clear;
    vrStr.SaveToFile(vrEnviroment.FullPath('Tempos.txt'));
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);

    FreeAndNil(vrEnviroment);
  end;
end;

function TJupiterToolsModule.StartedTime: Boolean;
var
  vrStr        : TStrings;
  vrEnviroment : TJupiterEnviroment;
  vrVez        : Integer;
begin
  Result := False;

  vrStr        := TStringList.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.BasePath :=  Self.Params.VariableById(Self.DefineParamName('Tasks.Current.Path')).Value;

    if FileExists(vrEnviroment.FullPath('Tempos.txt')) then
      vrStr.LoadFromFile(vrEnviroment.FullPath('Tempos.txt'))
    else
      Exit;

    for vrVez := vrStr.Count - 1 downto 0 do
    begin
      if Trim(vrStr[vrVez]) = EmptyStr then
        Continue;

      Result := Copy(Trim(vrStr[vrVez]), 1, 1) = 'I';
      Exit;
    end;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);

    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterToolsModule.SetCurrentTask(prClient, prTaskName, prCompletePath : String);
begin
  Self.Params.AddConfig(Self.DefineParamName('Tasks.Current.Client'), prClient, 'Cliente da tarefa atual');
  Self.Params.AddConfig(Self.DefineParamName('Tasks.Current.Number'), prTaskName, 'Número da tarefa atual');
end;

end.

