unit uJupiterGenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterModule, JupiterApp, jupiterDatabaseWizard,
  JupiterConsts;

type

  { TJupiterGenerator }

  TJupiterGenerator = class(TJupiterModule)
  protected
    function Internal_GetModuleID : String; override;
    function Internal_GetModuleTitle : String; override;
    procedure Internal_Prepare; override;
  end;

implementation

{ TJupiterGenerator }

function TJupiterGenerator.Internal_GetModuleID: String;
begin
  Result := 'Jupiter.Generator';
end;

function TJupiterGenerator.Internal_GetModuleTitle: String;
begin
  Result := 'UserExperience: Pacote de experiência do usuário';
end;

procedure TJupiterGenerator.Internal_Prepare;
var
  vrWizard : TJupiterDatabaseWizard;
begin
  vrWizard := vrJupiterApp.NewWizard;
  try
    if Self.Internal_CreateMacroIfDontExists('menu.tools.generator.click', 'Clique do botão Generator', CreateStringList('program macro;' + #13#10 + 'begin' + #13#10 + '  OpenForm(''/forms/generator'');' + #13#10 + 'end.')) then
      Self.Internal_CreateRouteIfDontExists('Generator', '/menu/tools/generator/', vrWizard.GetLastID('MACROS'), ICON_WIZARD, 100);
  finally
    FreeAndNil(vrWizard);
  end;
end;

end.

