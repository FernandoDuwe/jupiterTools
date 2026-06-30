unit jupiterScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterObject, JupiterConsts, JupiterEnviroment,
  JupiterVariable, jupiterStringUtils, PascalScript, uPSComponent, {$IFNDEF JUPITERCLI} Forms,  {$ENDIF}
  uPSCompiler, uPSRuntime, Variants;

type

  { TJupiterScript }

  TJupiterScriptFlags = Record
    GenerateFullFile : Boolean;
    DisableSmartImporter : Boolean;
    UseCaches : Boolean;
  end;

  TJupiterScriptAnalyserType = (jsaVariable, jsaProcedure, jsaFunction, jsaCompilerFlags);

  TJupiterScriptOnExecute = procedure (prScript, prMessages, prRunMessages : TStrings; prExecuted : Boolean) of object;

  TJupiterScriptOnAddMessage = procedure (prMessage : String) of object;

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
    function CanIncludeSource(prSourceCode : TStrings) : Boolean;

    constructor Create(prID, prContext : Integer; prType : TJupiterScriptAnalyserType; prDocText : String; prText : String = '');

    function GetIdentifier : String;
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

    function CanIncludeSource(prSourceCode : TStrings) : Boolean;
  end;

  TJupiterScript = class(TJupiterObject)
  private
    FScriptID     : String;
    FScriptName   : String;
    FScript       : TStrings;
    FMessages     : TStrings;
    FRunMessages  : TStrings;
    FCompiled     : Boolean;
    FRunned       : Boolean;
    FUseDebugInfo : Boolean;
    FUserCommand  : String;
    FLibraryList  : TJupiterObjectList;
    FParamList    : TJupiterVariableList;
    FOnExecute    : TJupiterScriptOnExecute;
    FOnAddMessage : TJupiterScriptOnAddMessage;

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

    procedure Internal_WriteLn(prMessage : String);
  published
    property Compiled     : Boolean read FCompiled;
    property Runned       : Boolean read FRunned;
    property UseDebugInfo : Boolean read FUseDebugInfo write FUseDebugInfo;

    property Messages    : TStrings             read FMessages    write FMessages;
    property RunMessages : TStrings             read FRunMessages write FRunMessages;
    property Script      : TStrings             read FScript;
    property Params      : TJupiterVariableList read FParamList   write FParamList;

    property LibraryList : TJupiterObjectList read FLibraryList write FLibraryList;
    property UserCommand : String read FUserCommand write FUserCommand;

    property OnAddMessage : TJupiterScriptOnAddMessage read FOnAddMessage write FOnAddMessage;
    property OnExecute : TJupiterScriptOnExecute read FOnExecute write FOnExecute;

    property ScriptID   : String read FScriptID;
    property ScriptName : String read FScriptName write FScriptName;
  public
    Flags : TJupiterScriptFlags;

    procedure Optimize(prSourceCode : TStrings);

    function GetDateTimeMark : String;

    procedure LoadFromFile(prFileName : String);

    function AnalyseCode : TJupiterScriptAnalyserList;
    function Execute : Boolean;

    procedure AddMessage(prMessage : String);

    constructor Create;
    destructor Destroy; override;

    procedure CreateNewFile(prName, prSaveAt : String);
  end;

var
  vrJupiterScript : TJupiterScript;

implementation

uses uPSR_std, uPSC_std, uPSR_stdctrls, uPSC_stdctrls, uPSR_forms, uPSC_forms,
     uPSC_graphics, uPSC_controls, uPSC_classes, uPSR_graphics, uPSR_controls,
     uPSR_classes, uPSC_comobj, uPSR_comobj, JupiterApp, uJupiterDesktopAppScript;

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

function TJupiterScriptLibrary.CanIncludeSource(prSourceCode: TStrings): Boolean;
var
  vrList : TJupiterScriptAnalyserList;
  vrVez  : Integer;
begin
  Result := False;

  vrList := Self.AnalyseCode;

  for vrVez := 0 to vrList.Count - 1 do
    if vrList.ItemByIndex(vrVez).CanIncludeSource(prSourceCode) then
    begin
      Result := True;
      Exit;
    end;
end;

{ TJupiterScriptAnalyserItem }

function TJupiterScriptAnalyserItem.CanIncludeSource(prSourceCode: TStrings): Boolean;
var
  vrID : String;
begin
  Result := True;

  vrID := Self.GetIdentifier;

  if vrID = EmptyStr then
    Exit;

  if Pos(AnsiUpperCase(vrID), AnsiUpperCase(prSourceCode.Text)) = 0 then
  begin
    Result := False;
    Exit;
  end;
end;

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

function TJupiterScriptAnalyserItem.GetIdentifier: String;
begin
  Result := Self.DocText;
  Result := StringReplace(Result, 'function ', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(Result, 'procedure ', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
  Result := StringReplace(Result, ' ', EmptyStr, [rfIgnoreCase, rfReplaceAll]);

  Result := Copy(AnsiUpperCase(Result), 1, Pos('(', Result) - 1);
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
  {$IFNDEF JUPITERCLI}
  Sender.SetVarToInstance('APPLICATION', Application);
  {$ENDIF}

  PPSVariantVariant(Sender.GetVariable('VARS'))^.Data := VarArrayCreate([0, 1], varShortInt)
end;

function TJupiterScript.Internal_GetFullScript: TStrings;
var
  vrVez      : Integer;
  vrFile     : String;
  vrLine     : String;
  vrUpperLine: String;

  function ReplacePlaceholders(const ALine: String): String;
  begin
    Result := StringReplace(ALine, JPAS_FLAG_USERCOMMAND, Self.UserCommand, [rfIgnoreCase, rfReplaceAll]);
    Result := StringReplace(Result, JPAS_FLAG_SCRIPTID, Self.ScriptID, [rfIgnoreCase, rfReplaceAll]);
    Result := StringReplace(Result, JPAS_FLAG_USECACHES, EmptyStr, [rfIgnoreCase, rfReplaceAll]);
  end;

  function ExtractIncludeFileName(const ALine: String): String;
  begin
    Result := StringReplace(ALine, 'IncludeJPAS(', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
    Result := StringReplace(Result, '''', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
    Result := StringReplace(Result, ');', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
  end;
begin
  Result := TStringList.Create;

  for vrVez := 0 to Self.Script.Count - 1 do
  begin
    vrLine := Trim(Self.Script[vrVez]);

    vrUpperLine := AnsiUpperCase(vrLine);

    if Copy(vrUpperLine, 1, 12) = 'INCLUDEJPAS(' then
    begin
      vrFile := ExtractIncludeFileName(vrLine);
      Self.Internal_IncludeScript(vrFile, Result);
    end
    else if Pos(AnsiUpperCase(JPAS_FLAG_GENERATEFULLFILE), vrUpperLine) > 0 then
      Self.Flags.GenerateFullFile := True
    else if Pos(AnsiUpperCase(JPAS_FLAG_DISABLESMARTIMPORTER), vrUpperLine) > 0 then
      Self.Flags.DisableSmartImporter := True
    else if Pos(AnsiUpperCase(JPAS_FLAG_USECACHES), vrUpperLine) > 0 then
    begin
      Self.Flags.UseCaches := True;

      Result.Add(ReplacePlaceholders(Self.Script[vrVez]));
    end
    else
      Result.Add(ReplacePlaceholders(Self.Script[vrVez]));
  end;
end;

procedure TJupiterScript.Internal_IncludeScript(prFileName: String; var prStrings: TStrings);
var
  vrVez      : Integer;
  vrFile     : String;
  vrLine     : String;
  vrUpperLine: String;
  vrStr      : TStringList;

  function ReplacePlaceholders(const ALine: String): String;
  begin
    Result := StringReplace(ALine, JPAS_FLAG_USERCOMMAND, Self.UserCommand, [rfIgnoreCase, rfReplaceAll]);
    Result := StringReplace(Result, JPAS_FLAG_SCRIPTID, Self.ScriptID, [rfIgnoreCase, rfReplaceAll]);
  end;

  function ExtractIncludeFileName(const ALine: String): String;
  begin
    Result := StringReplace(ALine, 'IncludeJPAS(', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
    Result := StringReplace(Result, '''', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
    Result := StringReplace(Result, ');', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
  end;
begin
  if not FileExists(prFileName) then
    raise Exception.Create('Não foi possível incluir o arquivo ' + prFileName + '.');

  vrStr := TStringList.Create;
  try
    vrStr.LoadFromFile(prFileName);

    for vrVez := 0 to vrStr.Count - 1 do
    begin
      vrLine := Trim(vrStr[vrVez]);
      vrUpperLine := AnsiUpperCase(vrLine);

      if Copy(vrUpperLine, 1, 12) = 'INCLUDEJPAS(' then
      begin
        vrFile := ExtractIncludeFileName(vrLine);
        Self.Internal_IncludeScript(vrFile, prStrings);
      end
      else if vrUpperLine = AnsiUpperCase(JPAS_FLAG_GENERATEFULLFILE) then
      begin
        Self.Flags.GenerateFullFile := True;
      end
      else if vrUpperLine = AnsiUpperCase(JPAS_FLAG_USECACHES) then
      begin
        Self.Flags.UseCaches := True;
        prStrings.Add(ReplacePlaceholders(vrStr[vrVez]));
      end
      else
        prStrings.Add(ReplacePlaceholders(vrStr[vrVez]));
    end;
  finally
    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterScript.Internal_WriteLn(prMessage: String);
begin
  Self.AddMessage(prMessage);

  WriteLn(prMessage);
end;

procedure TJupiterScript.Optimize(prSourceCode: TStrings);
var
  vrVez : Integer;
begin
  if Self.Flags.DisableSmartImporter then
    Exit;

  for vrVez := Self.LibraryList.Count - 1 downto 0 do
    if not TJupiterScriptLibrary(Self.LibraryList.GetAtIndex(vrVez)).CanIncludeSource(prSourceCode) then
      Self.LibraryList.DeleteAtIndex(vrVez);

  for vrVez := Self.LibraryList.Count - 1 downto 0 do
    if Self.LibraryList.GetAtIndex(vrVez) is TJupiterDesktopAppScript then
    begin
      vrJupiterApp.Params.VariableById(FORM_CURRENTSCRIPTID).Value := Self.ScriptID;
      vrJupiterApp.Params.VariableById(FORM_CURRENTSCRIPTNAME).Value := Self.ScriptName;
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
  vrUseCaches  : Boolean;
  vrCompiled   : AnsiString;
  vrCompiledFlag : Boolean;
begin
  Self.FMessages.Clear;
  Self.FRunMessages.Clear;

  for vrVez := 0 to Self.LibraryList.Count - 1 do
    TJupiterScriptLibrary(Self.LibraryList.GetAtIndex(vrVez)).Owner := Self;

  Self.FCompiled := False;
  Self.FRunned   := False;

  vrPSScript := TPSScript.Create(Application.MainForm);
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrPSScript.UseDebugInfo := Self.UseDebugInfo;
    vrPSScript.OnCompile    := @Self.Internal_ScriptCompile;
    vrPSScript.OnExecute    := @Self.Internal_ScriptExecute;
    vrPSScript.OnCompImport := @Self.Internal_ClassesPlugin1CompImport;
    vrPSScript.OnExecImport := @Self.Internal_ClassesPlugin1ExecImport;

    vrPSScript.Script.Clear;
    vrPSScript.Script.AddStrings(Self.Internal_GetFullScript);

    Self.Optimize(vrPSScript.Script);

    if Self.Flags.GenerateFullFile then
    begin
      vrPSScript.Script.SaveToFile(vrEnviroment.FullPath('/temp/compiledFile.jpas'));

      Self.Script.SaveToFile(vrEnviroment.FullPath('/temp/script.jpas'));
    end;

    try
      Self.Messages.Add('ScriptName: ' + Self.ScriptName);

      if vrJupiterApp.ScriptCache.Exists(Self.ScriptName + '.jpascache') then
      begin
        vrCompiled     := vrJupiterApp.ScriptCache.VariableById(Self.ScriptName + '.jpascache').Value;
        vrCompiledFlag := True;

        vrPSScript.SetCompiled(vrCompiled);

        Self.Messages.Add(Self.GetDateTimeMark + ': Compilação utilizada por caches');
      end
      else
        vrCompiledFlag := vrPSScript.Compile;

      if vrCompiledFlag then
      begin
        if ((Self.Flags.UseCaches) and (not vrEnviroment.Exists('/caches/' + Self.ScriptName + '.jpascache'))) then
        begin
          vrPSScript.GetCompiled(vrCompiled);

          vrJupiterApp.ScriptCache.AddVariable(Self.ScriptName + '.jpascache', vrCompiled);

          vrEnviroment.CreateFile('/caches/' + Self.ScriptName + '.jpascache', vrCompiled);
        end;

        Self.FCompiled := True;

        Self.Messages.Add(Self.GetDateTimeMark + ': Compilação completa');

        Self.AddMessage(Self.GetDateTimeMark + ': Iniciando execução');
        Self.AddMessage(EmptyStr);

        if vrPSScript.Execute then
        begin
          Self.AddMessage(EmptyStr);
          Self.AddMessage(Self.GetDateTimeMark + ': Execução finalizada');

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
    except
      Self.Messages.Add(Self.GetDateTimeMark + ': ' + Exception(ExceptObject).Message);
    end;
  finally
    if Assigned(Self.OnExecute) then
      Self.OnExecute(vrPSScript.Script, Self.Messages, Self.RunMessages, Self.FRunned);

    FreeAndNil(vrPSScript);
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterScript.AddMessage(prMessage: String);
begin
  if Assigned(Self.OnAddMessage) then
  begin
    Self.OnAddMessage(prMessage);
    Exit;
  end;

  Self.RunMessages.Add(prMessage);
end;

constructor TJupiterScript.Create;
begin
  Self.ScriptName    := EmptyStr;
  Self.FUseDebugInfo := False;

  try
    Self.FScriptID := JupiterStringUtilsGenerateGUID;

    Self.Flags.GenerateFullFile     := False;
    Self.Flags.DisableSmartImporter := False;
    Self.Flags.UseCaches            := False;

    if vrJupiterApp.Params.Exists('core.jpas.useCompiledCache') then
      Self.Flags.UseCaches := vrJupiterApp.Params.VariableById('core.jpas.useCompiledCache').AsBool;

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

  Self.FMessages.Clear;
  FreeAndNil(Self.FMessages);

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

