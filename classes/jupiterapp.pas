unit JupiterApp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, JupiterConfig, SysUtils, JupiterConsts;

type

  { TJupiterApp }

  TJupiterApp = class(TObject)
  protected
    FConfig  : TJupiterConfig;
    FModules : TList;

    procedure Internal_SetAppVariables;
    procedure Internal_SetModules;
  published
    property Config : TJupiterConfig read FConfig write FConfig;
  public
    function ModuleCount : Integer;
    function GetModuleByIndex(prIndex : Integer) : TObject;
    function GetModuleByID(prID : String) : TObject;

    constructor Create();
    destructor Destroy; override;

    procedure ListItems(prParams : TJupiterListem; var prList : TList);

    procedure RunListable(prParamsItem : TJupiterListem; prParamsListableItem : TJupiterListableItem);
  end;

var
  vrJupiterApp : TJupiterApp;

implementation

uses JupiterTasks, jupiterRunner, JupiterModule;

{ TJupiterApp }

procedure TJupiterApp.Internal_SetAppVariables;
begin
  Self.Config.AddVariable('JupiterTools.Variables.ExeFile', Application.ExeName, 'Executável do JupiterTools');
  Self.Config.AddVariable('JupiterTools.Variables.Path', ExtractFileDir(Application.ExeName), 'Diretório do JupiterTools');
end;

procedure TJupiterApp.Internal_SetModules;
begin
  if not DirectoryExists(ExtractFileDir(Application.ExeName) + '/modules/') then
     CreateDir(ExtractFileDir(Application.ExeName) + '/modules/');

  Self.FModules.Add(TJupiterTasks.Create(Self));

  Self.FModules.Add(TJupiterRunner.Create(Self));
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

constructor TJupiterApp.Create();
begin
  Self.FConfig := TJupiterConfig.Create;

  Self.FModules := TList.Create;

  Self.Internal_SetAppVariables;
  Self.Internal_SetModules;
end;

destructor TJupiterApp.Destroy;
begin
  while Self.FModules.Count > 0 do
  begin
    TJupiterModule(Self.FModules[0]).Free;
    Self.FModules.Delete(0);
  end;

  FreeAndNil(Self.FModules);

  FreeAndNil(Self.FConfig);

  inherited Destroy;
end;

procedure TJupiterApp.ListItems(prParams: TJupiterListem; var prList: TList);
var
  vrModule : TJupiterModule;
begin
  vrModule := TJupiterModule(Self.GetModuleByID(prParams.Module));

  vrModule.ListItems(prParams, prList);
end;

procedure TJupiterApp.RunListable(prParamsItem: TJupiterListem; prParamsListableItem: TJupiterListableItem);
var
  vrModule : TJupiterModule;
begin
  vrModule := TJupiterModule(Self.GetModuleByID(prParamsItem.Module));

  vrModule.RunListable(prParamsListableItem);
end;

end.

