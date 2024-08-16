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
var
  vrWizard : TJupiterDatabaseWizard;
begin
  inherited Internal_Prepare;

  vrWizard := vrJupiterApp.NewWizard;
  try
    if Self.Internal_CreateMacroIfDontExists('menu.show.incFont.click', 'Clique do botão Incrementar fonte', CreateStringList('program macro;' + #13#10 + 'begin' + #13#10 + '  IncFont();' + #13#10 + 'end.')) then
      Self.Internal_CreateRouteIfDontExists('Aumentar fonte', '/menu/show/incFont/', vrWizard.GetLastID('MACROS'), NULL_KEY, 70);

    if Self.Internal_CreateMacroIfDontExists('menu.show.decFont.click', 'Clique do botão Decrementar fonte', CreateStringList('program macro;' + #13#10 + 'begin' + #13#10 + '  DecFont();' + #13#10 + 'end.')) then
      Self.Internal_CreateRouteIfDontExists('Diminuir fonte', '/menu/show/decFont/', vrWizard.GetLastID('MACROS'), NULL_KEY, 70);

    Self.Internal_CreateRouteIfDontExists('Preferências', '/menu/show/userPref/', NULL_KEY, ICON_APPLICATION, 100);

    Self.Internal_CreateVariablIfDontExists(FIELD_FONT_SIZE, 'Tamanho da fonte', '9');
  finally
    FreeAndNil(vrWizard);
  end;
end;

end.

