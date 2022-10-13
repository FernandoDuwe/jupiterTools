unit JupiterToolsModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterModule, JupiterRoute, JupiterObject, JupiterAction,
  JupiterConsts, JupiterApp, JupiterEnviroment, JupiterCSVDataProvider,
  SysUtils;

type

  { TJupiterToolsModule }

  TJupiterToolsModule = class(TJupiterModule)
  protected
    procedure Internal_Prepare; override;
    function Internal_GetModuleID : String; override;
    function Internal_GetModuleTitle : String; override;

    procedure Internal_OnChangeCurrentTask(prID, prNewValue : String);
  public
    function GetActions(prRoute : TJupiterRoute) : TJupiterObjectList; override;

    function CreateTask(prClient, prTaskName : String; prAsCurrentTask : Boolean) : String;
    procedure CopyFile(prPath : String; prOldFile : String);
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
    vrEnviroment.CreatePath('modules/tools/templates/');

    if not Self.Params.Exists(Self.DefineParamName('Tasks.Path')) then
       Self.Params.AddConfig(Self.DefineParamName('Tasks.Path'), vrEnviroment.CreatePath('Tarefas/'), 'Diretório de tarefas');

    Self.Params.VariableById(Self.DefineParamName('Tasks.Current.Path')).OnChangeValue := @Internal_OnChangeCurrentTask;
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

function TJupiterToolsModule.GetActions(prRoute: TJupiterRoute): TJupiterObjectList;
begin
  Result := inherited GetActions(prRoute);

  Result.Add(TJupiterAction.Create('Tarefas', TJupiterRoute.Create(EXPLORER_FORM_PATH), TJupiterRoute.Create('/')));

  with TJupiterAction(Result.GetLastObject) do
  begin
    Icon := ICON_TASKS;
    Route.Params.AddVariable('title', 'Minhas tarefas', 'Título');
    Route.Params.AddVariable('type', DATAPROVIDER_TYPE_TASKS, 'Tipo');
    Route.Params.AddVariable('checkableField',    'Path');
    Route.Params.AddVariable('checkableVariable', Self.DefineParamName('Tasks.Current.Path'));
  end;

  if Self.Params.Exists(Self.DefineParamName('Tasks.Current.Path')) then
  begin
    Result.Add(TJupiterAction.Create('Atual', TJupiterRoute.Create(TASK_FORM_PATH), TJupiterRoute.Create(EXPLORER_FORM_PATH)));

    with TJupiterAction(Result.GetLastObject) do
      Icon := ICON_CURTASK;
  end;

  Result.Add(TJupiterAction.Create('Nova Tarefa', TJupiterRoute.Create(NEWTASK_FORM_PATH), TJupiterRoute.Create('/records/')));

  with TJupiterAction(Result.GetLastObject) do
    Icon := ICON_ADD;

  Result.Add(TJupiterAction.Create('Biblioteca', TJupiterRoute.Create('/tools/checklists/'), TJupiterRoute.Create('/tools/')));

  with TJupiterAction(Result.GetLastObject) do
    Icon := ICON_CHECK;

  Result.Add(TJupiterAction.Create('Checklists', TJupiterRoute.Create('/tools/checklists/'), TJupiterRoute.Create('/tools/')));

  with TJupiterAction(Result.GetLastObject) do
    Icon := ICON_CHECK;

  Result.Add(TJupiterAction.Create('Favoritos', TJupiterRoute.Create('/tools/favorites/'), TJupiterRoute.Create('/tools/')));

  with TJupiterAction(Result.GetLastObject) do
    Icon := ICON_FAVORITE;

  Result.Add(TJupiterAction.Create('Arquivos', TJupiterRoute.Create('/tools/favorites/apps/'), TJupiterRoute.Create('/tools/favorites/')));

  with TJupiterAction(Result.GetLastObject) do
    Icon := ICON_APPLICATION;

  Result.Add(TJupiterAction.Create('Diretórios', TJupiterRoute.Create('/tools/favorites/folders/'), TJupiterRoute.Create('/tools/favorites/')));

  with TJupiterAction(Result.GetLastObject) do
    Icon := ICON_OPEN;
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

procedure TJupiterToolsModule.CopyFile(prPath: String; prOldFile: String);
begin
  //
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
    vrEnviroment.BasePath :=  Self.Params.VariableById(Self.DefineParamName('Tasks.Current.Path')).Value;

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

