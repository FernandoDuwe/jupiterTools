unit jupiterScriptFunctions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterApp, JupiterRunnable, JupiterDialogForm,
  JupiterRoute, Forms;

  // I/O Functions
  procedure JupiterWriteLn(prMessage : String);
  function JupiterReadLn : String;
  function JupiterInputText(prMessage : String) : String;

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

implementation

uses jupiterScript, LCLType;

procedure JupiterWriteLn(prMessage : String);
begin
  if Assigned(vrJupiterScript) then
    vrJupiterScript.RunMessages.Add(prMessage);
end;

function JupiterReadLn: String;
var
  vrDialog : TJupiterDialogForm;
begin
  Result := EmptyStr;

  vrDialog := TJupiterDialogForm.Create;
  try
    vrDialog.Title := 'Inserir informação';

    vrDialog.Fields.AddField('VALUE', 'Insira um texto', '');

    if vrDialog.Show then
      Result := vrDialog.Fields.VariableFormById('VALUE').Value;
  finally
    FreeAndNil(vrDialog);
  end;
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

procedure JupiterAddLogMessage(prTitle, prDescription: String);
begin
  vrJupiterApp.AddMessage(prTitle, 'JupiterScript').Details.Add(prDescription);
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

procedure JupiterAddConfiguration(prID, prValue, prTitle: String);
begin
  vrJupiterApp.UserParams.AddConfig(prID, prValue, prTitle);
end;

procedure JupiterAddVariable(prID, prValue, prTitle: String);
begin
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

procedure JupiterScriptGoToRoute(prRoutePath : String; prCSVParams : TStringList; prModal : Boolean);
var
  vrRoute : TJupiterRoute;
begin
  vrRoute := TJupiterRoute.Create(prRoutePath);
  vrRoute.Params.FromStringList(prCSVParams);

  vrJupiterApp.NavigateTo(vrRoute, prModal);
end;

end.
