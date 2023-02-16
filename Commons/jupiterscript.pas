unit jupiterScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, JupiterConsts,
  PascalScript, uPSComponent, Forms, uPSCompiler,
  uPSRuntime, Variants;

type

  { TJupiterScript }

  TJupiterScript = class(TJupiterObject)
  private
    FScript      : TStrings;
    FMessages    : TStrings;
    FRunMessages : TStrings;
    FCompiled    : Boolean;
    FRunned      : Boolean;

    procedure Internal_OutputMessages(prPSScript : TPSScript);
  protected
    procedure Internal_ClassesPlugin1CompImport(Sender: TObject; x: TPSPascalCompiler);
    procedure Internal_ClassesPlugin1ExecImport(Sender: TObject; Exec: TPSExec; x: TPSRuntimeClassImporter);
    procedure Internal_ScriptCompile(Sender: TPSScript);
    procedure Internal_ScriptExecute(Sender: TPSScript);
  published
    property Compiled : Boolean read FCompiled;
    property Runned   : Boolean read FRunned;

    property Messages    : TStrings read FMessages write FMessages;
    property RunMessages : TStrings read FRunMessages write FRunMessages;
    property Script      : TStrings read FScript;
  public
    function GetDateTimeMark : String;

    procedure LoadFromFile(prFileName : String);

    function Execute : Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure CreateNewFile(prName, prSaveAt : String);
  end;

var
  vrJupiterScript : TJupiterScript;

implementation

uses uPSR_std, uPSC_std, uPSR_stdctrls, uPSC_stdctrls, uPSR_forms, uPSC_forms,
     uPSC_graphics, uPSC_controls, uPSC_classes, uPSR_graphics, uPSR_controls,
     uPSR_classes, uPSC_comobj, uPSR_comobj, jupiterScriptFunctions;

{ TJupiterScript }

procedure TJupiterScript.Internal_OutputMessages(prPSScript: TPSScript);
var
  vrVez : Integer;
begin
  for vrVez := 0 to prPSScript.CompilerMessageCount - 1 do
    Self.Messages.Add(Self.GetDateTimeMark + ': Compilador: ' + prPSScript.CompilerErrorToStr(vrVez));
end;

procedure TJupiterScript.Internal_ClassesPlugin1CompImport(Sender: TObject; x : TPSPascalCompiler);
begin
  SIRegister_Std(x);
  SIRegister_Classes(x, True);
  SIRegister_Graphics(x, True);
  SIRegister_Controls(x);
  SIRegister_stdctrls(x);
  SIRegister_Forms(x);
  SIRegister_ComObj(x);
end;

procedure TJupiterScript.Internal_ClassesPlugin1ExecImport(Sender: TObject; Exec: TPSExec; x: TPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x, True);
  RIRegister_Graphics(x, True);
  RIRegister_Controls(x);
  RIRegister_stdctrls(x);
  RIRegister_Forms(x);
  RIRegister_ComObj(exec);
end;

procedure TJupiterScript.Internal_ScriptCompile(Sender: TPSScript);
begin
  Sender.AddFunction(@JupiterWriteLn, 'procedure Writeln(prMessage : String);');
  Sender.AddFunction(@JupiterReadLn, 'function Readln: String;');
  Sender.AddFunction(@JupiterInputText, 'function InputText(prMessage : String) : String;');

  Sender.AddFunction(@JupiterAddLogMessage, 'procedure AddLogMessage(prTitle, prDescription : String);');
  Sender.AddFunction(@JupiterShowPopup, 'procedure ShowPopup(prTitle, prDescription : String);');

  Sender.AddFunction(@JupiterRunCommandLine, 'function RunCommandLine(prCommandLine: String) : String;');
  Sender.AddFunction(@JupiterRunnable, 'procedure Runnable(prCommandLine: String);');
  Sender.AddFunction(@JupiterLoadFromFile, 'function LoadFromFile(prFileName : String) : String;');
  Sender.AddFunction(@JupiterSaveToFile, 'procedure SaveToFile(prFileName, prData : String);');

  Sender.AddFunction(@JupiterAddConfiguration, 'procedure AddConfiguration(prID, prValue, prTitle : String);');
  Sender.AddFunction(@JupiterAddVariable, 'procedure AddVariable(prID, prValue, prTitle : String);');
  Sender.AddFunction(@JupiterVariableExists, 'function VariableExists(prVariableID : String) : Boolean;');
  Sender.AddFunction(@JupiterVariableValueByID, 'function VariableValueByID(prVariableID : String) : String;');

  Sender.AddRegisteredVariable('vars', 'Variant');
  Sender.AddRegisteredVariable('Application', 'TApplication');
end;

procedure TJupiterScript.Internal_ScriptExecute(Sender: TPSScript);
begin
  Sender.SetVarToInstance('APPLICATION', Application);

  PPSVariantVariant(Sender.GetVariable('VARS'))^.Data := VarArrayCreate([0, 1], varShortInt)
end;

function TJupiterScript.GetDateTimeMark: String;
begin
  Result := FormatDateTime('dd/mm/yyyy hh:nn:ss', Now);

  Result := '[' + Result + ']';
end;

procedure TJupiterScript.LoadFromFile(prFileName: String);
begin
  Self.FScript.Clear;
  Self.FScript.LoadFromFile(prFileName);
end;

function TJupiterScript.Execute: Boolean;
var
  vrPSScript : TPSScript;
begin
  Self.FMessages.Clear;

  Self.FCompiled := False;
  Self.FRunned   := False;

  vrPSScript := TPSScript.Create(Application.MainForm);
  try
    vrPSScript.OnCompile    := @Self.Internal_ScriptCompile;
    vrPSScript.OnExecute    := @Self.Internal_ScriptExecute;
    vrPSScript.OnCompImport := @Self.Internal_ClassesPlugin1CompImport;
    vrPSScript.OnExecImport := @Self.Internal_ClassesPlugin1ExecImport;

    vrPSScript.Script.Clear;
    vrPSScript.Script.AddStrings(Self.Script);

    if vrPSScript.Compile then
    begin
      Self.FCompiled := True;

      Self.Messages.Add(Self.GetDateTimeMark + ': Compilação completa');

      Self.RunMessages.Add(Self.GetDateTimeMark + ': Iniciando execução');
      Self.RunMessages.Add(EmptyStr);

      if vrPSScript.Execute then
      begin
        Self.RunMessages.Add(EmptyStr);
        Self.RunMessages.Add(Self.GetDateTimeMark + ': Execução finalizada');

        Self.Messages.Add(Self.GetDateTimeMark + ': Execução completa');

        Self.FRunned := True;
      end
      else
        Self.Messages.Add(Self.GetDateTimeMark + ': ' + vrPSScript.ExecErrorToString + ' em ' + IntToStr(vrPSScript.ExecErrorProcNo) + '.' + IntToStr(vrPSScript.ExecErrorByteCodePosition));
    end
    else
    begin
      Self.FCompiled := False;

      Self.Internal_OutputMessages(vrPSScript);
      Self.Messages.Add(EmptyStr);
      Self.Messages.Add(Self.GetDateTimeMark + ': Compilação falhou');
    end;
  finally
    FreeAndNil(vrPSScript);
  end;
end;

constructor TJupiterScript.Create;
begin
  Self.FScript := TStringList.Create;
  Self.FScript.Clear;

  Self.FMessages := TStringList.Create;
  Self.FMessages.Clear;

  Self.FRunMessages := TStringList.Create;
  Self.FRunMessages.Clear;

  Self.FCompiled := False;
  Self.FRunned   := False;

  vrJupiterScript := Self;
end;

destructor TJupiterScript.Destroy;
begin
  Self.FScript.Clear;
  FreeAndNil(Self.FScript);

  Self.FRunMessages.Clear;
  FreeAndNil(Self.FRunMessages);

  vrJupiterScript := nil;

  inherited Destroy;
end;

procedure TJupiterScript.CreateNewFile(prName, prSaveAt : String);
var
  vrFile : TStrings;
begin
  vrFile := TStringList.Create;
  try
    vrFile.Clear;
    vrFile.Add('program ' + StringReplace(prName, ' ', EmptyStr, [rfIgnoreCase, rfReplaceAll]) + ';');
    vrFile.Add('begin');
    vrFile.Add('  // Seu código aqui');
    vrFile.Add('end.');

    vrFile.SaveToFile(prSaveAt + DirectorySeparator + prName + '.jpas');
  finally
    FreeAndNil(vrFile);
  end;
end;

end.

