unit jupiterScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, JupiterConsts, JupiterEnviroment,
  JupiterVariable, jupiterStringUtils, PascalScript, uPSComponent, Forms,
  uPSCompiler, uPSRuntime, Variants;

type

  { TJupiterScript }

  TJupiterScriptFlags = Record
    GenerateFullFile : Boolean;
  end;

  TJupiterScriptAnalyserType = (jsaVariable, jsaProcedure, jsaFunction, jsaCompilerFlags);

  { TJupiterScriptAnalyserItem }

  TJupiterScriptAnalyserItem = class(TJupiterObject)
  private
    FID : Integer;
    FContext : Integer;
    FType : TJupiterScriptAnalyserType;
    FText : String;
    FDocText : String;
  published
    property ID         : Integer read FID;
    property Context    : Integer read FContext;
    property ScriptType : TJupiterScriptAnalyserType read FType;
    property Text       : String read FText;
    property DocText    : String read FDocText;
  public
    constructor Create(prID, prContext : Integer; prType : TJupiterScriptAnalyserType; prDocText : String; prText : String = '');
  end;

  { TJupiterScriptAnalyserList }

  TJupiterScriptAnalyserList = class(TJupiterObjectList)
  public
    procedure AddItem(prItem : TJupiterScriptAnalyserItem);
    function ItemByIndex(prIndex : Integer) : TJupiterScriptAnalyserItem;
  end;

  { TJupiterScriptLibrary }

  TJupiterScriptLibrary = class(TJupiterObject)
  protected
    FOwner : TJupiterObject;

    function Internal_GetName : String; virtual;
  published
    property Name : String read Internal_GetName;

    property Owner : TJupiterObject read FOwner write FOwner;
  public
    procedure DoCompile(prSender: TPSScript); virtual;
    function AnalyseCode: TJupiterScriptAnalyserList; virtual;
  end;

  TJupiterScript = class(TJupiterObject)
  private
    FScriptID    : String;
    FScript      : TStrings;
    FMessages    : TStrings;
    FRunMessages : TStrings;
    FCompiled    : Boolean;
    FRunned      : Boolean;
    FUserCommand : String;
    FLibraryList : TJupiterObjectList;
    FParamList   : TJupiterVariableList;

    procedure Internal_OutputMessages(prPSScript : TPSScript);

    procedure Internal_AnalyseUserCode(var prAnalyserObj : TJupiterScriptAnalyserList);

    function Internal_HasTag(prTag, prLine : String) : Boolean;
  protected
    procedure Internal_ClassesPlugin1CompImport(Sender: TObject; x: TPSPascalCompiler);
    procedure Internal_ClassesPlugin1ExecImport(Sender: TObject; Exec: TPSExec; x: TPSRuntimeClassImporter);
    procedure Internal_ScriptCompile(Sender: TPSScript);
    procedure Internal_ScriptExecute(Sender: TPSScript);
    function  Internal_GetFullScript : TStrings;
    procedure Internal_IncludeScript(prFileName : String; var prStrings : TStrings);
  published
    property Compiled : Boolean read FCompiled;
    property Runned   : Boolean read FRunned;
    property Messages    : TStrings             read FMessages    write FMessages;
    property RunMessages : TStrings             read FRunMessages write FRunMessages;
    property Script      : TStrings             read FScript;
    property Params      : TJupiterVariableList read FParamList   write FParamList;

    property LibraryList : TJupiterObjectList read FLibraryList write FLibraryList;
    property UserCommand : String read FUserCommand write FUserCommand;

    property ScriptID : String read FScriptID;
  public
    Flags : TJupiterScriptFlags;

    function GetDateTimeMark : String;

    procedure LoadFromFile(prFileName : String);

    function AnalyseCode : TJupiterScriptAnalyserList;
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
     uPSR_classes, uPSC_comobj, uPSR_comobj, JupiterApp;

{ TJupiterScriptAnalyserList }

procedure TJupiterScriptAnalyserList.AddItem(prItem: TJupiterScriptAnalyserItem
  );
begin
  Self.Add(prItem);
end;

function TJupiterScriptAnalyserList.ItemByIndex(prIndex: Integer
  ): TJupiterScriptAnalyserItem;
begin
  Result := TJupiterScriptAnalyserItem(Self.GetAtIndex(prIndex));
end;

{ TJupiterScriptLibrary }

function TJupiterScriptLibrary.Internal_GetName: String;
begin
  Result := EmptyStr;
end;

procedure TJupiterScriptLibrary.DoCompile(prSender: TPSScript);
begin
  //
end;

function TJupiterScriptLibrary.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result := TJupiterScriptAnalyserList.Create;
end;

{ TJupiterScriptAnalyserItem }

constructor TJupiterScriptAnalyserItem.Create(prID, prContext: Integer;
  prType: TJupiterScriptAnalyserType; prDocText : String; prText : String = '');
begin
  Self.FID      := prID;
  Self.FContext := prContext;
  Self.FType    := prType;
  Self.FDocText := prDocText;

  if Trim(prText) = EmptyStr then
  begin
    Self.FText := prDocText;

    if prType = jsaProcedure then
      Self.FText := StringReplace(Self.FText, 'procedure ', EmptyStr, [rfIgnoreCase, rfReplaceAll]);

    if prType = jsaFunction then
      Self.FText := StringReplace(Self.FText, 'function ', EmptyStr, [rfIgnoreCase, rfReplaceAll]);

    Self.FText := StringReplace(Self.FText, ');', ')', [rfIgnoreCase, rfReplaceAll]);
    Self.FText := StringReplace(Self.FText, ';', ',', [rfIgnoreCase, rfReplaceAll]);
    Self.FText := StringReplace(Self.FText, ' ', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
    Self.FText := StringReplace(Self.FText, ':String', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
    Self.FText := StringReplace(Self.FText, ':Integer', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
    Self.FText := StringReplace(Self.FText, ':Boolean', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
    Self.FText := StringReplace(Self.FText, ':Float', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
    Self.FText := StringReplace(Self.FText, ':TDate', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
    Self.FText := StringReplace(Self.FText, ':TDateTime', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
    Self.FText := StringReplace(Self.FText, ':TTime', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
    Self.FText := StringReplace(Self.FText, ':TStrings', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
    Self.FText := StringReplace(Self.FText, ':TStringList', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
    Self.FText := StringReplace(Self.FText, ',', ', ', [rfIgnoreCase, rfReplaceAll]);
    Self.FText := TrimRight(Self.FText);

    if Copy(Self.FText, Length(Self.FText), 1) = ',' then
      Self.FText := Copy(Self.FText, 1, Length(Self.FText) - 1);
  end
  else
    Self.FText := prText;
end;

{ TJupiterScript }

procedure TJupiterScript.Internal_OutputMessages(prPSScript: TPSScript);
var
  vrVez : Integer;
begin
  for vrVez := 0 to prPSScript.CompilerMessageCount - 1 do
    Self.Messages.Add(Self.GetDateTimeMark + ': Compilador: ' + prPSScript.CompilerErrorToStr(vrVez));
end;

procedure TJupiterScript.Internal_AnalyseUserCode(var prAnalyserObj: TJupiterScriptAnalyserList);
var
  vrStrings : TStrings;
  vrVez : Integer;
begin
  vrStrings := Self.Internal_GetFullScript();

  for vrVez := 0 to vrStrings.Count - 1 do
  begin
    if Self.Internal_HasTag('procedure', vrStrings[vrVez]) then
      prAnalyserObj.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaProcedure, Trim(vrStrings[vrVez])));

    if Self.Internal_HasTag('function', vrStrings[vrVez]) then
      prAnalyserObj.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, Trim(vrStrings[vrVez])));
  end;
end;

function TJupiterScript.Internal_HasTag(prTag, prLine: String): Boolean;
begin
  prTag  := Trim(AnsiUpperCase(prTag));
  prLine := Trim(AnsiUpperCase(prLine));

  Result := Copy(prLine, 1, Length(prTag)) = prTag;
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
  SIRegisterTStringList(x);
  SIRegister_StdCtrls(x);
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
var
  vrVez : Integer;
begin
  for vrVez := 0 to Self.LibraryList.Count - 1 do
    with TJupiterScriptLibrary(Self.LibraryList.GetAtIndex(vrVez)) do
      DoCompile(Sender);

  Sender.AddRegisteredVariable('vars', 'Variant');
  Sender.AddRegisteredVariable('Application', 'TApplication');
end;

procedure TJupiterScript.Internal_ScriptExecute(Sender: TPSScript);
begin
  Sender.SetVarToInstance('APPLICATION', Application);

  PPSVariantVariant(Sender.GetVariable('VARS'))^.Data := VarArrayCreate([0, 1], varShortInt)
end;

function TJupiterScript.Internal_GetFullScript: TStrings;
var
  vrVez  : Integer;
  vrFile : String;
  vrAux  : TStrings;
begin
  Result := TStringList.Create;
  Result.Clear;

  for vrVez := 0 to Self.Script.Count - 1 do
    if Copy(AnsiUpperCase(Trim(Self.Script[vrVez])), 1, 12) = 'INCLUDEJPAS(' then
    begin
      vrFile := TrimLeft(TrimRight(Self.Script[vrVez]));
      vrFile := StringReplace(vrFile, 'IncludeJPAS(', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
      vrFile := StringReplace(vrFile, '''', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
      vrFile := StringReplace(vrFile, ');', EmptyStr, [rfIgnoreCase, rfReplaceAll]);

      Self.Internal_IncludeScript(vrFile, Result);
    end
    else
    begin
      if AnsiUpperCase(TrimRight(TrimLeft(Self.Script[vrVez]))) = AnsiUpperCase(JPAS_FLAG_GENERATEFULLFILE) then
        Self.Flags.GenerateFullFile := True
      else
      begin
        Result.Add(Self.Script[vrVez]);
      end;
    end;

  for vrVez := 0 to Result.Count - 1 do
    Result[vrVez] := StringReplace(Result[vrVez], JPAS_FLAG_USERCOMMAND, Self.UserCommand, [rfIgnoreCase, rfReplaceAll]);

  Result.Text := StringReplace(Result.Text, JPAS_FLAG_SCRIPTID, Self.ScriptID, [rfIgnoreCase, rfReplaceAll]);
end;

procedure TJupiterScript.Internal_IncludeScript(prFileName: String; var prStrings: TStrings);
var
  vrVez  : Integer;
  vrFile : String;
  vrStr  : TStringList;
begin
  if not FileExists(prFileName) then
    raise Exception.Create('Não foi possível incluir o arquivo ' + prFileName + '.');

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.LoadFromFile(prFileName);

    for vrVez := 0 to vrStr.Count - 1 do
      if Copy(AnsiUpperCase(Trim(vrStr[vrVez])), 1, 12) = 'INCLUDEJPAS(' then
      begin
        vrFile := TrimLeft(TrimRight(vrStr[vrVez]));
        vrFile := StringReplace(vrFile, 'IncludeJPAS(', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
        vrFile := StringReplace(vrFile, '''', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
        vrFile := StringReplace(vrFile, ');', EmptyStr, [rfIgnoreCase, rfReplaceAll]);

        Self.Internal_IncludeScript(vrFile, prStrings);
      end
      else
      begin
        if AnsiUpperCase(TrimRight(TrimLeft(vrStr[vrVez]))) = AnsiUpperCase(JPAS_FLAG_GENERATEFULLFILE) then
          Self.Flags.GenerateFullFile := True
        else
          prStrings.Add(vrStr[vrVez]);
      end;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
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

function TJupiterScript.AnalyseCode: TJupiterScriptAnalyserList;
var
  vrVez  : Integer;
  vrVez2 : Integer;
  vrList : TJupiterScriptAnalyserList;
begin
  Result := TJupiterScriptAnalyserList.Create;

  for vrVez := 0 to Self.LibraryList.Count - 1 do
    with TJupiterScriptLibrary(Self.LibraryList.GetAtIndex(vrVez)) do
    begin
      vrList := AnalyseCode;

      for vrVez2 := 0 to vrList.Count - 1 do
        Result.AddItem(vrList.ItemByIndex(vrVez2));
    end;

  // Code functions
  Self.Internal_AnalyseUserCode(Result);
end;

function TJupiterScript.Execute: Boolean;
var
  vrPSScript   : TPSScript;
  vrEnviroment : TJupiterEnviroment;
  vrVez        : Integer;
begin
  Self.FMessages.Clear;

  for vrVez := 0 to Self.LibraryList.Count - 1 do
    TJupiterScriptLibrary(Self.LibraryList.GetAtIndex(vrVez)).Owner := Self;

  Self.FCompiled := False;
  Self.FRunned   := False;

  vrPSScript := TPSScript.Create(Application.MainForm);
  try
    vrPSScript.OnCompile    := @Self.Internal_ScriptCompile;
    vrPSScript.OnExecute    := @Self.Internal_ScriptExecute;
    vrPSScript.OnCompImport := @Self.Internal_ClassesPlugin1CompImport;
    vrPSScript.OnExecImport := @Self.Internal_ClassesPlugin1ExecImport;

    vrPSScript.Script.Clear;
    vrPSScript.Script.AddStrings(Self.Internal_GetFullScript);

    if Self.Flags.GenerateFullFile then
    begin
      vrEnviroment := TJupiterEnviroment.Create;
      try
        vrPSScript.Script.SaveToFile(vrEnviroment.FullPath('/temp/compiledFile.jpas'));

        Self.Script.SaveToFile(vrEnviroment.FullPath('/temp/script.jpas'));
      finally
        FreeAndNil(vrEnviroment);
      end;
    end;

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
  try
    Self.FScriptID := JupiterStringUtilsGenerateGUID;

    Self.Flags.GenerateFullFile := False;

    Self.FParamList := TJupiterVariableList.Create;

    Self.FScript := TStringList.Create;
    Self.FScript.Clear;

    Self.FMessages := TStringList.Create;
    Self.FMessages.Clear;

    Self.FRunMessages := TStringList.Create;
    Self.FRunMessages.Clear;

    Self.FCompiled := False;
    Self.FRunned   := False;

    Self.FUserCommand := EmptyStr;

    Self.LibraryList := TJupiterObjectList.Create;

    vrJupiterScript := Self;
  finally
    vrJupiterApp.Scripts.Add(Self);
  end;
end;

destructor TJupiterScript.Destroy;
begin
  Self.FScript.Clear;
  FreeAndNil(Self.FScript);

  FreeAndNil(Self.FParamList);

  Self.FRunMessages.Clear;
  FreeAndNil(Self.FRunMessages);

  vrJupiterScript := nil;

  FreeAndNil(Self.FLibraryList);

  vrJupiterApp.DeleteScriptById(Self.ScriptID);

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

