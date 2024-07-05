unit uJupiterUserExperience;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterModule, JupiterApp, jupiterDatabaseWizard,
  JupiterConsts;

type

  { TJupiterUserExperience }

  TJupiterUserExperience = class(TJupiterModule)
  protected
    function Internal_GetModuleID : String; override;
    function Internal_GetModuleTitle : String; override;
    procedure Internal_Prepare; override;
  end;

implementation

{ TJupiterUserExperience }

function TJupiterUserExperience.Internal_GetModuleID: String;
begin
  Result := 'Jupiter.UserExperience';
end;

function TJupiterUserExperience.Internal_GetModuleTitle: String;
begin
  Result := 'UserExperience: Pacote de experiência do usuário';
end;

procedure TJupiterUserExperience.Internal_Prepare;
begin
  inherited Internal_Prepare;

  Self.Internal_CreateRouteIfDontExists('Preferências', '/menu/show/userPref/', NULL_KEY, ICON_APPLICATION, 100);
end;

end.

