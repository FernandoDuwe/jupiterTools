unit jupiterScriptFunctions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterApp, JupiterRunnable, JupiterDialogForm,
  JupiterRoute, JupiterTasksDataProvider, jupiterclicommand, JupiterModule,
  JupiterFileDataProvider, JupiterDirectoryDataProvider, JupiterToolsModule,
  Forms;

  // I/O Functions
  procedure JupiterWriteLn(prMessage : String);
  function JupiterReadLn : String;
  function JupiterInputText(prMessage : String) : String;
  function JupiterParamByName(prParamName : String) : String;

  // Messages & Popup
  procedure JupiterAddLogMessage(prTitle, prDescription : String);
  procedure JupiterShowPopup(prTitle, prDescription : String);
  procedure JupiterShowInfoMessage(prMessage : String);
  procedure JupiterShowErrorMessage(prMessage : String);

  // Enviromental functions
  function JupiterRunCommandLine(prCommandLine : String) : String;
  function JupiterRunCommandLineNoMessage(prCommandLine : String) : String;
  procedure  JupiterRunCommandLineNoWait(prCommandLine : String);
  procedure JupiterRunnable(prCommandLine : String);
  procedure JupiterRunnableInThread(prTitle, prCommandLine : String);
  function JupiterLoadFromFile(prFileName : String) : String;
  procedure JupiterSaveToFile(prFileName, prData : String);
  function JupiterScriptHasRoutePath(prRoutePath : String) : Boolean;
  function JupiterScriptGoToRoutePath(prRoutePath : String) : Boolean;
  procedure JupiterScriptCreateTaskDataCache;
  function JupiterScriptIsConsole : Boolean;
  function JupiterScriptFileExists(prFile : String) : Boolean;
  function JupiterScriptDirectoryExists(prFile : String) : Boolean;
  function JupiterScriptExtractFileDir(prFile : String) : String;
  function JupiterScriptExtractFileExt(prFile : String) : String;
  function JupiterScriptExtractFileName(prFile : String) : String;
  function JupiterScriptReadDirectory(prDirectory : String; prSubFolders : Boolean) : TStrings;
  function JupiterScriptReadFile(prDirectory : String; prSubFolders : Boolean) : TStrings;

  // Variable funcions
  procedure JupiterAddConfiguration(prID, prValue, prTitle : String);
  procedure JupiterAddVariable(prID, prValue, prTitle : String);
  function  JupiterVariableExists(prVariableID : String) : Boolean;
  function  JupiterVariableValueByID(prVariableID : String) : String;
  function  JupiterVariableResolve(prMessage : String) : String;

  function  JupiterVariableValueRouteByID(prVariableID : String) : String;

  // String functions
  function JupiterReplace(prStr, prOldString, prNewString : String) : String;

  // Route functions
  function JupiterGetCurrentRoute : String;
  function JupiterGetCurrentRouteParams : TStringList;
  procedure JupiterScriptGoToRoute(prRoutePath : String; prCSVParams : TStringList; prModal : Boolean);

  // Task Functions
  procedure JupiterToolsRegisterStartTime;
  procedure JupiterToolsRegisterEndTime;
  function  JupiterToolsStartedTime : Boolean;

implementation

uses jupiterScript, LCLType;

procedure JupiterWriteLn(prMessage : String);
begin
  {$IFNDEF JUPITERCLI}
  if Assigned(vrJupiterScript) then
    vrJupiterScript.RunMessages.Add(prMessage);
  {$ENDIF}

  {$IFDEF JUPITERCLI}
     WriteLn(prMessage);
  {$ENDIF}
end;

function JupiterReadLn: String;
var
  vrDialog : TJupiterDialogForm;
begin
  Result := EmptyStr;

  {$IFNDEF JUPITERCLI}
  vrDialog := TJupiterDialogForm.Create;
  try
    vrDialog.Title := 'Inserir informação';

    vrDialog.Fields.AddField('VALUE', 'Insira um texto', '');

    if vrDialog.Show then
      Result := vrDialog.Fields.VariableFormById('VALUE').Value;
  finally
    FreeAndNil(vrDialog);
  end;
  {$ENDIF}

  {$IFDEF JUPITERCLI}

  {$ENDIF}
end;

function JupiterInputText(prMessage: String): String;
var
  vrDialog : TJupiterDialogForm;
begin
  Result := EmptyStr;

  vrDialog := TJupiterDialogForm.Create;
  try
    vrDialog.Title := 'Inserir informação';

    vrDialog.Fields.AddField('VALUE', prMessage, '');

    if vrDialog.Show then
      Result := vrDialog.Fields.VariableFormById('VALUE').Value;
  finally
    FreeAndNil(vrDialog);
  end;
end;

procedure JupiterShowPopup(prTitle, prDescription: String);
var
  vrStr : TStrings;
begin
  vrStr := TStringList.Create;
  vrStr.Clear;
  vrStr.Add(prDescription);

  vrJupiterApp.Popup(prTitle, vrStr);
end;

procedure JupiterScriptGoToRoute(prRoutePath : String; prCSVParams : TStringList; prModal : Boolean);
var
  vrRoute : TJupiterRoute;
begin
  vrRoute := TJupiterRoute.Create(prRoutePath);
  vrRoute.Params.FromStringList(prCSVParams);

  vrJupiterApp.NavigateTo(vrRoute, prModal);
end;

procedure JupiterToolsRegisterStartTime;
begin
  if not TJupiterToolsModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools')).StartedTime then
    TJupiterToolsModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools')).SetStartTime;
end;

procedure JupiterToolsRegisterEndTime;
begin
  if TJupiterToolsModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools')).StartedTime then
    TJupiterToolsModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools')).SetEndTime;
end;

function JupiterToolsStartedTime: Boolean;
begin
  Result := TJupiterToolsModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Tools')).StartedTime;
end;

function JupiterParamByName(prParamName: String): String;
var
  vrVez : Integer;
  vrCommand : TJupiterCLICommand;
begin
  Result := EmptyStr;

  if not Assigned(vrJupiterApp.CurrentCLICommand) then
    Exit;

  vrCommand := TJupiterCLICommand(vrJupiterApp.CurrentCLICommand);

  for vrVez := 0 to vrCommand.ParamList.Count - 1 do
    if TJupiterCLICommandParam(vrCommand.ParamList.GetAtIndex(vrVez)).ParamName = prParamName then
      Result := TJupiterCLICommandParam(vrCommand.ParamList.GetAtIndex(vrVez)).Value;
end;

procedure JupiterAddLogMessage(prTitle, prDescription: String);
begin
  vrJupiterApp.AddMessage(prTitle, 'JupiterScript').Details.Add(prDescription);
end;

procedure JupiterShowInfoMessage(prMessage: String);
begin
  Application.MessageBox(PAnsiChar(prMessage), 'Aviso', MB_ICONINFORMATION + MB_OK);
end;

procedure JupiterShowErrorMessage(prMessage: String);
begin
  Application.MessageBox(PAnsiChar(prMessage), 'Aviso', MB_ICONERROR + MB_OK);
end;

function JupiterRunCommandLine(prCommandLine: String) : String;
var
  vrRunnable : TJupiterRunnable;
begin
  vrRunnable := TJupiterRunnable.Create(prCommandLine, False);
  try
    vrRunnable.RunCommandLine(Result);
  finally
    FreeAndNil(vrRunnable);
  end;
end;

function JupiterRunCommandLineNoMessage(prCommandLine: String): String;
var
  vrRunnable : TJupiterRunnable;
begin
  vrRunnable := TJupiterRunnable.Create(prCommandLine, False);
  try
    vrRunnable.LogMessage := False;

    vrRunnable.RunCommandLine(Result);
  finally
    FreeAndNil(vrRunnable);
  end;
end;

procedure JupiterRunCommandLineNoWait(prCommandLine: String);
var
  vrRunnable : TJupiterRunnable;
  vrOutPut   : String;
begin
  vrRunnable := TJupiterRunnable.Create(prCommandLine, False);
  try
    vrRunnable.RunCommandLine(vrOutPut, False);
  finally
    FreeAndNil(vrRunnable);
  end;
end;

procedure JupiterRunnable(prCommandLine: String);
begin
  TJupiterRunnable.Create(prCommandLine, True);
end;

procedure JupiterRunnableInThread(prTitle, prCommandLine: String);
begin
  vrJupiterApp.Threads.NewThread(prTitle, TJupiterRunnable.Create(prCommandLine));
end;

function JupiterLoadFromFile(prFileName: String): String;
var
  vrStr : TStrings;
begin
  Result := EmptyStr;

  if not FileExists(prFileName) then
    Exit;

  vrStr := TStringList.Create;
  try
    vrStr.LoadFromFile(prFileName);

    Result := vrStr.Text;
  finally
    FreeAndNil(vrStr);
  end;
end;

procedure JupiterSaveToFile(prFileName, prData : String);
var
  vrStr : TStrings;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add(prData);

    vrStr.SaveToFile(prFileName);
  finally
    FreeAndNil(vrStr);
  end;
end;

function JupiterScriptHasRoutePath(prRoutePath: String): Boolean;
begin
  Result := vrJupiterApp.HasRoute(prRoutePath);
end;

function JupiterScriptGoToRoutePath(prRoutePath: String): Boolean;
begin
  Result := vrJupiterApp.GoToRoute(prRoutePath);
end;

procedure JupiterScriptCreateTaskDataCache;
var
  vrTask : TJupiterTasksDataProvider;
begin
  vrTask := TJupiterTasksDataProvider.Create;
  try
    vrTask.CreateCacheData;
  finally
    FreeAndNil(vrTask);
  end;
end;

function JupiterScriptIsConsole: Boolean;
begin
  Result := vrJupiterApp.ConsoleMode;
end;

function JupiterScriptFileExists(prFile: String): Boolean;
begin
  Result := FileExists(prFile);
end;

function JupiterScriptDirectoryExists(prFile: String): Boolean;
begin
  Result := DirectoryExists(prFile);
end;

function JupiterScriptExtractFileDir(prFile: String): String;
begin
  Result := ExtractFileDir(prFile);
end;

function JupiterScriptExtractFileExt(prFile: String): String;
begin
  Result := ExtractFileExt(prFile);
end;

function JupiterScriptExtractFileName(prFile: String): String;
begin
  Result := ExtractFileName(prFile);
end;

function JupiterScriptReadDirectory(prDirectory: String; prSubFolders : Boolean): TStrings;
var
  vrDirectory : TJupiterDirectoryDataProvider;
  vrVez : Integer;
begin
  vrDirectory := TJupiterDirectoryDataProvider.Create;
  Result      := TStringList.Create;
  try
    Result.Clear;

    vrDirectory.Path       := prDirectory;
    vrDirectory.SubFolders := prSubFolders;
    vrDirectory.ProvideData;

    for vrVez := 0 to vrDirectory.Count - 1 do
      with vrDirectory.GetRowByIndex(vrVez) do
        Result.Add(Fields.VariableById('Path').Value);
  finally
    FreeAndNil(vrDirectory);
  end;
end;

function JupiterScriptReadFile(prDirectory: String; prSubFolders: Boolean): TStrings;
var
  vrFile : TJupiterFileDataProvider;
  vrVez : Integer;
begin
  vrFile := TJupiterFileDataProvider.Create;
  Result := TStringList.Create;
  try
    Result.Clear;

    vrFile.Path       := prDirectory;
    vrFile.SubFolders := prSubFolders;
    vrFile.ProvideData;

    for vrVez := 0 to vrFile.Count - 1 do
      with vrFile.GetRowByIndex(vrVez) do
        Result.Add(Fields.VariableById('File').Value);
  finally
    FreeAndNil(vrFile);
  end;
end;

procedure JupiterAddConfiguration(prID, prValue, prTitle: String);
begin
  if vrJupiterApp.Params.Exists(prID) then
    vrJupiterApp.Params.VariableById(prID).Value := prValue
  else
    vrJupiterApp.UserParams.AddConfig(prID, prValue, prTitle);
end;

procedure JupiterAddVariable(prID, prValue, prTitle: String);
begin
  if vrJupiterApp.Params.Exists(prID) then
    vrJupiterApp.Params.VariableById(prID).Value := prValue
  else
    vrJupiterApp.UserParams.AddVariable(prID, prValue, prTitle);
end;

function JupiterVariableExists(prVariableID : String) : Boolean;
begin
  Result := vrJupiterApp.Params.Exists(prVariableID);
end;

function JupiterVariableValueByID(prVariableID: String): String;
begin
  Result := vrJupiterApp.Params.VariableById(prVariableID).Value;
end;

function JupiterVariableResolve(prMessage: String): String;
begin
  Result := vrJupiterApp.Params.ResolveString(prMessage);
end;

function JupiterVariableValueRouteByID(prVariableID: String): String;
begin
  Result := vrJupiterApp.CurrentRoute.Params.VariableById(prVariableID).Value;
end;

function JupiterReplace(prStr, prOldString, prNewString: String): String;
begin
  Result := StringReplace(prStr, prOldString, prNewString, [rfIgnoreCase, rfReplaceAll]);
end;

function JupiterGetCurrentRoute: String;
begin
  Result := vrJupiterApp.CurrentRoute.Path;
end;

function JupiterGetCurrentRouteParams: TStringList;
begin
  Result := vrJupiterApp.CurrentRoute.Params.ToStringList;
end;

end.

