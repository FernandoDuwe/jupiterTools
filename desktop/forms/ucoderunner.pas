unit uCodeRunner;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, uJupiterForm, jupiterScript, JupiterApp, JupiterConsts,
  jupiterStringUtils, uJupiterAction, SQLDB;

type

  { TFCodeRunner }

  TFCodeRunner = class(TFJupiterForm)
    lbOutput: TListBox;
    tmrExecute: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrExecuteTimer(Sender: TObject);
  private
    FMacro : String;
    FScript : TJupiterScript;
    FEnableExecute : Boolean;

    procedure Internal_AddMessage(prMessage : String);
    procedure Internal_SetCaption(prCaption: String);
    procedure Internal_OnExecute(Sender: TObject);
    procedure Internal_UpdateComponents; override;

    procedure Internal_PrepareForm; override;
  public
    procedure FromScriptID(prScriptID : String);
  end;

var
  FCodeRunner: TFCodeRunner;

implementation

{$R *.lfm}

{ TFCodeRunner }

procedure TFCodeRunner.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FScript := vrJupiterApp.NewScript;
  Self.FScript.OnAddMessage := @Internal_AddMessage;

  Self.FEnableExecute := False;
end;

procedure TFCodeRunner.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Self.FScript);

  inherited;
end;

procedure TFCodeRunner.tmrExecuteTimer(Sender: TObject);
begin
  Self.FEnableExecute := False;

  tmrExecute.Enabled := False;

  Self.Hint := 'Executando...';
  Self.Internal_SetCaption('Executando...');
  Self.UpdateForm();

  Self.FScript.Execute;

  if Self.FScript.Compiled then
  begin
    Self.Hint := 'Executado com sucesso';
    Self.Internal_SetCaption('Executado com sucesso');
  end
  else
  begin
    Self.Hint := 'Executado com erros...';
    Self.Internal_SetCaption('Executado com erros...');
  end;

  Self.FEnableExecute := True;

  Self.UpdateForm();
end;

procedure TFCodeRunner.Internal_AddMessage(prMessage: String);
begin
  Self.lbOutput.Items.Add(prMessage);

  Application.ProcessMessages;
end;

procedure TFCodeRunner.Internal_SetCaption(prCaption: String);
begin
  Self.Caption := Self.FMacro + ': ' + prCaption;
end;

procedure TFCodeRunner.Internal_OnExecute(Sender: TObject);
begin
  lbOutput.Items.Clear;

  tmrExecute.Enabled := True;
end;

procedure TFCodeRunner.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if Self.ActionGroup.Count > 0 then
    if Self.FEnableExecute then
      Self.ActionGroup.GetActionAtIndex(0).Enable
    else
      Self.ActionGroup.GetActionAtIndex(0).Disable;
end;

procedure TFCodeRunner.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Executar', 'Clique aqui para executar a macro', ICON_PLAY, @Internal_OnExecute));
end;

procedure TFCodeRunner.FromScriptID(prScriptID: String);
var
vrQry    : TSQLQuery;
begin
  Self.FMacro := prScriptID;

  Self.Hint := 'Macro: ' + prScriptID;

  vrQry := vrJupiterApp.NewWizard.NewQuery;
  try
    vrQry.SQL.Add(' SELECT ID, MACRO FROM MACROS WHERE MACROID = :PRID ');
    vrQry.ParamByName('PRID').AsString := prScriptID;
    vrQry.Open;

    Self.FScript.Script.AddStrings(JupiterStringUtilsStringToStringList(vrQry.FieldByName('MACRO').AsString));

    tmrExecute.Enabled := True;
  finally
    FreeAndNil(vrQry);
  end;
end;

end.

