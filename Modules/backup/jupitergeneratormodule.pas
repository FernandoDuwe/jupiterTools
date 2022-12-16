unit JupiterGeneratorModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterModule, JupiterRoute, JupiterObject, JupiterAction,
  JupiterConsts, JupiterEnviroment, JupiterFileDataProvider,
  JupiterGeneratorMenuItem, SysUtils;

type
  { TJupiterGeneratorModule }

  TJupiterGeneratorModule = class(TJupiterModule)
  protected
    procedure Internal_Prepare; override;
    function Internal_GetModuleID : String; override;
    function Internal_GetModuleTitle : String; override;
  public
    function GetActions(prRoute : TJupiterRoute) : TJupiterObjectList; override;
  end;

implementation

{ TJupiterGeneratorModule }

procedure TJupiterGeneratorModule.Internal_Prepare;
var
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Internal_Prepare;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.CreatePath('modules/generator');
    vrEnviroment.CreatePath('modules/generator/menus');
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

function TJupiterGeneratorModule.Internal_GetModuleID: String;
begin
  Result := 'Jupiter.Generator';
end;

function TJupiterGeneratorModule.Internal_GetModuleTitle: String;
begin
  Result := 'Generator';
end;

function TJupiterGeneratorModule.GetActions(prRoute: TJupiterRoute): TJupiterObjectList;
var
  vrEnviroment : TJupiterEnviroment;
  vrFile       : TJupiterFileDataProvider;
  vrVez        : Integer;
  vrMenu       : TJupiterGeneratorMenuItem;
begin
  Result := inherited GetActions(prRoute);

  vrEnviroment := TJupiterEnviroment.Create;
  vrFile       := TJupiterFileDataProvider.Create;
  try
    vrFile.Path := vrEnviroment.FullPath('modules/generator/menus');
    vrFile.ProvideData;

    for vrVez := 0 to vrFile.Size - 1 do
    begin
      vrMenu := TJupiterGeneratorMenuItem.Create;
      vrMenu.FileName := vrFile.GetRowByIndex(vrVez).Fields.VariableById('File').Value;

      Result.Add(vrMenu.CreateAction);
    end;
  finally
    FreeAndNil(vrFile);
    FreeAndNil(vrEnviroment);
  end;
end;

end.

