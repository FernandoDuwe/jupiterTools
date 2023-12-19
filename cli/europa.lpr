program europa;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, JupiterConsts, JupiterObject, JupiterVariable,
  JupiterAction, JupiterFileDataProvider, JupiterEnviroment, JupiterRunnable,
  jupiterclicommand, JupiterApp, jupiterScriptFunctions, jupiterScript,
  jupiterclimodule, JupiterGeneratorModule, JupiterStandardModule,
  JupiterToolsModule, pascalscript, CustApp, Interfaces
  { you can add units after this };

type

  { TEuropa }

  TEuropa = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure Internal_RunScript;
    function Internal_GetCommand : String;
  published
    property Command : String read Internal_GetCommand;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TEuropa }

procedure TEuropa.DoRun;
var
  ErrorMsg : String;
  vrVez : Integer;
begin
  WriteLn('Europa - CLI for Jupiter - ' + vrJupiterApp.GetVersion);
  try
    // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');

    if ErrorMsg <> EmptyStr then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    { add your program here }

    if ParamCount < 1 then
    begin
      WriteLn('You must inform the command to be executed. For more info, use the -h command.');
      Exit;
    end;

    Self.Internal_RunScript;
  finally
    // stop program loop
    Terminate;
  end;
end;

procedure TEuropa.Internal_RunScript;
var
  vrFileProvider : TJupiterFileDataProvider;
  vrEnviroment : TJupiterEnviroment;
  vrCLI : TJupiterCLICommand;
  vrVez : Integer;
  vrVez2 : Integer;
begin
  vrFileProvider := TJupiterFileDataProvider.Create;
  vrEnviroment   := TJupiterEnviroment.Create;
  try
    vrEnviroment.BasePath := ExtractFileDir(Self.ExeName);

    vrFileProvider.Path := vrEnviroment.FullPath('/modules/CLI/');
    vrFileProvider.ProvideData;

    for vrVez := 0 to vrFileProvider.Count - 1 do
    begin
      vrCLI := TJupiterCLICommand.Create;
      vrCLI.FileName := vrFileProvider.GetRowByIndex(vrVez).Fields.VariableById('File').Value;
      vrCLI.LoadFromFile;

      if Self.Command = AnsiUpperCase(vrCLI.Command) then
      begin
        for vrVez2 := 0 to vrCLI.ParamList.Count - 1 do
        begin
          TJupiterCLICommandParam(vrCLI.ParamList.GetAtIndex(vrVez2)).Value := EmptyStr;

          if (vrVez2 + 2) <= ParamCount then
            TJupiterCLICommandParam(vrCLI.ParamList.GetAtIndex(vrVez2)).Value := ParamStr(vrVez2 + 2);

          if ((TJupiterCLICommandParam(vrCLI.ParamList.GetAtIndex(vrVez2)).Required) and (TJupiterCLICommandParam(vrCLI.ParamList.GetAtIndex(vrVez2)).Value = EmptyStr)) then
          begin
            WriteLn(Format('Param %0:s is required', [TJupiterCLICommandParam(vrCLI.ParamList.GetAtIndex(vrVez2)).ParamName]));
            Exit;
          end;
        end;

        vrJupiterApp.CurrentCLICommand := vrCLI;

        vrJupiterApp.RunScript(vrCLI.CommandText);
      end;
    end;
  finally
    FreeAndNil(vrFileProvider);
    FreeAndNil(vrEnviroment);
  end;
end;

function TEuropa.Internal_GetCommand: String;
begin
  Result := AnsiUpperCase(Params[1]);
end;

constructor TEuropa.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  StopOnException := True;
end;

destructor TEuropa.Destroy;
begin
//  vrJupiterApp.Free;

  inherited Destroy;
end;

procedure TEuropa.WriteHelp;
var
  vrFileProvider : TJupiterFileDataProvider;
  vrEnviroment : TJupiterEnviroment;
  vrCLI : TJupiterCLICommand;
  vrVez : Integer;
  vrVez2 : Integer;
begin
  { add your help code here }
  WriteLn('Usage: ', ExeName, ' -h');
  WriteLn(EmptyStr);
  WriteLn('europa COMMAND <PARAM...>');
  WriteLn('Avaliable commands:');

  vrFileProvider := TJupiterFileDataProvider.Create;
  vrEnviroment   := TJupiterEnviroment.Create;
  try
    vrEnviroment.BasePath := ExtractFileDir(Self.ExeName);

    vrFileProvider.Path := vrEnviroment.FullPath('/modules/CLI/');
    vrFileProvider.ProvideData;

    for vrVez := 0 to vrFileProvider.Count - 1 do
    begin
      vrCLI := TJupiterCLICommand.Create;
      vrCLI.FileName := vrFileProvider.GetRowByIndex(vrVez).Fields.VariableById('File').Value;
      vrCLI.LoadFromFile;

      WriteLn(Format('  * %0:s - %1:s', [vrCLI.CommandName, vrCLI.Command]));

      if vrCLI.ParamList.Count > 0 then
      begin
        WriteLn('    Params:');

        for vrVez2 := 0 to vrCLI.ParamList.Count - 1 do
        begin
          if TJupiterCLICommandParam(vrCLI.ParamList.GetAtIndex(vrVez2)).Required then
            WriteLn(Format('    - %0:s - %1:s', [TJupiterCLICommandParam(vrCLI.ParamList.GetAtIndex(vrVez2)).ParamName, 'Required']))
          else
            WriteLn(Format('    - %0:s - %1:s', [TJupiterCLICommandParam(vrCLI.ParamList.GetAtIndex(vrVez2)).ParamName, 'Optional']));
        end;
      end;

      WriteLn(EmptyStr);
    end;
  finally
    FreeAndNil(vrFileProvider);
    FreeAndNil(vrEnviroment);
  end;
end;

var
  Application: TEuropa;

{$R *.res}

begin
  vrJupiterApp := TJupiterApp.Create('jupiter', 'Europa - CLI for Jupiter');
  try
    vrJupiterApp.AddModule(TJupiterStandardModule.Create);
    vrJupiterApp.AddModule(TJupiterCLIModule.Create);
    vrJupiterApp.AddModule(TJupiterToolsModule.Create);
    vrJupiterApp.AddModule(TJupiterGeneratorModule.Create);

    Application := TEuropa.Create(nil);
    Application.Title := 'Europa';
    Application.Run;
    Application.Free;
  finally
    // vrJupiterApp.Free;
  end;
end.

