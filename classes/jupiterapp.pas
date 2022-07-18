unit JupiterApp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, JupiterConfig, SysUtils, JupiterConsts, jupiterLog;

type

  { TJupiterApp }

  TJupiterApp = class(TObject)
  protected
    FConfig      : TJupiterConfig;
    FLog         : TJupiterLog;
    FModules     : TList;
    FAppName     : String;

    procedure Internal_SetAppVariables;
    procedure Internal_SetModules;
  published
    property AppName : String         read FAppName;
    property Config  : TJupiterConfig read FConfig  write FConfig;
    property Log     : TJupiterLog    read FLog     write FLog;
  public
    function ConsoleMode : Boolean;

    function ModuleCount : Integer;
    function GetModuleByIndex(prIndex : Integer) : TObject;
    function GetModuleByID(prID : String) : TObject;
    function GetVersion : String;

    constructor Create(prAppName : String);
    destructor Destroy; override;

    procedure ListItems(var prParams : TJupiterListem; var prList : TList);
    procedure ListActions(prParams : TJupiterListem; var prList : TList);

    procedure RunListable(var prParamsItem : TJupiterListem; var prParamsListableItem : TJupiterListableItem);

    procedure RegisterRecentTask(prTask : TJupiterListem);

    function GetAboutInfo : String;
  end;

var
  vrJupiterApp : TJupiterApp;

implementation

uses FileInfo, fileUtils, JupiterTasks, jupiterRunner, JupiterModule, jupiterchecklist, jupiterGenerator;

{ TJupiterApp }

procedure TJupiterApp.Internal_SetAppVariables;
begin
  Self.Config.AddVariable(Self.FAppName + '.Variables.ExeFile', Application.ExeName, 'Executável do JupiterTools');
  Self.Config.AddVariable(Self.FAppName + '.Variables.Path', ExtractFileDir(Application.ExeName), 'Diretório do JupiterTools');

  if not Self.Config.Exists(Self.FAppName + '.UI.Display.FontSize') then
    Self.Config.AddConfig(Self.FAppName + '.UI.Display.FontSize', '9', 'Tamanho da fonte dos formulários');

  if not Self.Config.Exists(Self.FAppName + '.UI.Display.WindowsState') then
    Self.Config.AddConfig(Self.FAppName + '.UI.Display.WindowsState', 'Normal', 'Estado da Janela (Normal / Maximized)');

  Self.Config.AddVariable(Self.FAppName + '.Variables.OS.DirectotySeparator', GetDirectorySeparator, 'Caracter separador de diretório');

  {$IFDEF WINDOWS}
    Self.Config.AddVariable(Self.FAppName + '.Variables.OS', 'Windows', 'Sistema Operacional');
  {$ELSE}
    Self.Config.AddVariable(Self.FAppName + '.Variables.OS', 'Linux', 'Sistema Operacional');
  {$ENDIF}

  Self.Config.AddVariable(Self.FAppName + '.Variables.ComputerName', GetEnvironmentVariable('COMPUTERNAME'), 'Nome do computador');

  Self.Config.AddVariable(Self.FAppName + '.Variables.CurrentPath', EmptyStr, 'Diretório atual');
  Self.Config.AddVariable(Self.FAppName + '.Variables.CurrentFile', EmptyStr, 'Arquivo atual');
end;

procedure TJupiterApp.Internal_SetModules;
begin
  if not DirectoryExists(ExtractFileDir(Application.ExeName) + '/temp/') then
     CreateDir(ExtractFileDir(Application.ExeName) + '/temp/');

  if not DirectoryExists(ExtractFileDir(Application.ExeName) + '/modules/') then
     CreateDir(ExtractFileDir(Application.ExeName) + '/modules/');

  Self.FModules.Add(TJupiterTasks.Create(Self));

  Self.FModules.Add(TJupiterRunner.Create(Self));

  Self.FModules.Add(TJupiterChecklist.Create(Self));

  Self.FModules.Add(TJupiterGenerator.Create(Self));
end;

function TJupiterApp.ConsoleMode: Boolean;
begin
  Result := False;
end;

function TJupiterApp.ModuleCount: Integer;
begin
  Result := Self.FModules.Count;
end;

function TJupiterApp.GetModuleByIndex(prIndex: Integer): TObject;
begin
  Result := TJupiterModule(Self.FModules[prIndex]);
end;

function TJupiterApp.GetModuleByID(prID: String): TObject;
var
  vrVez : Integer;
begin
  Result := nil;

  for vrVez := 0 to Self.ModuleCount -1 do
      if TJupiterModule(Self.GetModuleByIndex(vrVez)).ID = prID then
      begin
        Result := TJupiterModule(Self.GetModuleByIndex(vrVez));
        Exit;
      end;
end;

function TJupiterApp.GetVersion: String;
var
  vrVersionInfo : TVersionInfo;
begin
  Result := EmptyStr;

  vrVersionInfo:=TVersionInfo.Create;
  try
    vrVersionInfo.Load(HINSTANCE);

    Result := Format('%0:d.%1:d.%2:d.%3:d', [vrVersionInfo.FixedInfo.FileVersion[0], vrVersionInfo.FixedInfo.FileVersion[1], vrVersionInfo.FixedInfo.FileVersion[2], vrVersionInfo.FixedInfo.FileVersion[3]]);
  finally
    if Assigned(vrVersionInfo) then
      vrVersionInfo.Free;
  end;
end;

constructor TJupiterApp.Create(prAppName : String);
begin
  try
    Self.FAppName := prAppName;

    Self.FConfig := TJupiterConfig.Create;

    Self.FLog := TJupiterLog.Create;

    Self.FModules := TList.Create;

    Self.Internal_SetAppVariables;
    Self.Internal_SetModules;

  finally
    Self.Log.AddLog(Now, Self.FAppName, 'Iniciando');
  end;
end;

destructor TJupiterApp.Destroy;
begin
  while Self.FModules.Count > 0 do
  begin
    TJupiterModule(Self.FModules[0]).Free;
    Self.FModules.Delete(0);
  end;

  FreeAndNil(Self.FModules);

  FreeAndNil(Self.FLog);

  FreeAndNil(Self.FConfig);

  inherited Destroy;
end;

procedure TJupiterApp.ListItems(var prParams: TJupiterListem; var prList: TList);
var
  vrModule : TJupiterModule;
begin
  vrModule := TJupiterModule(Self.GetModuleByID(prParams.Module));

  vrModule.ListItems(prParams, prList);
end;

procedure TJupiterApp.ListActions(prParams: TJupiterListem; var prList: TList);
var
  vrModule : TJupiterModule;
begin
  vrModule := TJupiterModule(Self.GetModuleByID(prParams.Module));

  vrModule.ListActions(prParams, prList);
end;

procedure TJupiterApp.RunListable(var prParamsItem: TJupiterListem; var prParamsListableItem: TJupiterListableItem);
var
  vrModule : TJupiterModule;
begin
  vrModule := TJupiterModule(Self.GetModuleByID(prParamsItem.Module));

  vrModule.RunListable(prParamsItem, prParamsListableItem);
end;

procedure TJupiterApp.RegisterRecentTask(prTask: TJupiterListem);
var
  vrStr : TStrings;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;

    if FileExists(TratarCaminho(Self.Config.GetByID(Self.FAppName + '.Variables.Path').Value + '\datasets\recents.csv')) then
      vrStr.LoadFromFile(TratarCaminho(Self.Config.GetByID(Self.FAppName + '.Variables.Path').Value + '\datasets\recents.csv'))
    else
    begin
      vrStr.Add('OBJECTSTR');
      vrStr.Add(EmptyStr);
    end;

    if vrStr.IndexOf(prTask.ObjectToStr) <> - 1 then
      vrStr.Delete(vrStr.IndexOf(prTask.ObjectToStr));

    vrStr.Insert(1, prTask.ObjectToStr);

    while vrStr.Count > 11 do
      vrStr.Delete(vrStr.Count - 1);

    vrStr.SaveToFile(TratarCaminho(Self.Config.GetByID(Self.FAppName + '.Variables.Path').Value + '\datasets\recents.csv'));
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

function TJupiterApp.GetAboutInfo: String;
var
  vrPlataform : String;
  vrVez       : Integer;
begin
  vrPlataform := EmptyStr;

  {$IFDEF WINDOWS}
    vrPlataform := 'Windows';
  {$ELSE}
    vrPlataform := 'Linux';
  {$ENDIF}

  Result := Self.FAppName + #13#10 + #13#10 + 'Versão: ' + Self.GetVersion + #13#10 + 'Plataforma atual: ' + vrPlataform+ #13#10 + 'Plataformas suportadas: Windows, Linux';

  Result := Result + #13#10 + #13#10 + 'Módulos instalados: ';

  for vrVez := 0 to Self.ModuleCount - 1 do
     Result := Result + #13#10 + '  - ' + TJupiterModule(Self.GetModuleByIndex(vrVez)).ID + ';';
;
end;

end.

