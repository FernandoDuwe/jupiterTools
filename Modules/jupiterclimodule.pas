unit jupiterclimodule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, JupiterModule, JupiterEnviroment, jupiterclicommand, SysUtils;

type

  { TJupiterCLIModule }

  TJupiterCLIModule = class(TJupiterModule)
  private
    procedure Internal_CreateCommands;
  protected
    procedure Internal_Prepare; override;
    function Internal_GetModuleID : String; override;
    function Internal_GetModuleTitle : String; override;
  end;

implementation

{ TJupiterCLIModule }

procedure TJupiterCLIModule.Internal_CreateCommands;
var
  vrCommand : TJupiterCLICommand;
begin
  vrCommand := TJupiterCLICommand.Create;
  try
    vrCommand.CommandName := 'Write config value';
    vrCommand.Command := 'Write';
    vrCommand.AddParam('PARAMNAME', True);
    vrCommand.AddParam('PARAMVALUE', True);
    vrCommand.CommandText.Add('// Gravação de uma nova configuração');
    vrCommand.CommandText.Add('AddConfiguration(ParamByName(''PARAMNAME''), ParamByName(''PARAMVALUE''), ParamByName(''PARAMNAME''));');

    vrCommand.SaveToFile;
  finally
    FreeAndNil(vrCommand);
  end;

  Sleep(1000);

  vrCommand := TJupiterCLICommand.Create;
  try
    vrCommand.CommandName := 'Read config value';
    vrCommand.Command := 'Read';
    vrCommand.AddParam('PARAMNAME', True);
    vrCommand.CommandText.Add('// Gravação de uma nova configuração');
    vrCommand.CommandText.Add('Writeln(VariableValueByID(ParamByName(''PARAMNAME'')));');

    vrCommand.SaveToFile;
  finally
    FreeAndNil(vrCommand);
  end;
end;

procedure TJupiterCLIModule.Internal_Prepare;
var
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Internal_Prepare;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    if not DirectoryExists(vrEnviroment.FullPath('modules/CLI')) then
    begin
      vrEnviroment.CreatePath('modules/CLI');

      Self.Internal_CreateCommands;
    end;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

function TJupiterCLIModule.Internal_GetModuleID: String;
begin
  Result := 'Jupiter.CLI';
end;

function TJupiterCLIModule.Internal_GetModuleTitle: String;
begin
  Result := 'CLI';
end;

end.

