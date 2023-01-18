unit jupiterScriptFunctions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterApp, JupiterRunnable, JupiterDialogForm;

  // I/O Functions
  procedure JupiterWriteLn(prMessage : String);
  function JupiterReadLn : String;
  function JupiterInputText(prMessage : String) : String;

  // Messages & Popup
  procedure JupiterAddLogMessage(prTitle, prDescription : String);
  procedure JupiterShowPopup(prTitle, prDescription : String);

  // Enviromental functions
  procedure JupiterRunCommandLine(prCommandLine : String);

  // VariableFuncions
  procedure JupiterAddConfiguration(prID, prValue, prTitle : String);
  procedure JupiterAddVariable(prID, prValue, prTitle : String);
  function  JupiterVariableExists(prVariableID : String) : Boolean;
  function  JupiterVariableValueByID(prVariableID : String) : String;

implementation

uses jupiterScript;

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

procedure JupiterRunCommandLine(prCommandLine: String);
begin
  TJupiterRunnable.Create(prCommandLine, True);
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

end.

