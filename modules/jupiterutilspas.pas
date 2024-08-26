unit jupiterutilspas;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterModule, JupiterApp, jupiterDatabaseWizard,
  JupiterConsts;

type

  { TJupiterUtils }

  TJupiterUtils = class(TJupiterModule)
  protected
    function Internal_GetModuleID : String; override;
    function Internal_GetModuleTitle : String; override;
    procedure Internal_Prepare; override;
  end;

implementation

{ TJupiterUtils }

function TJupiterUtils.Internal_GetModuleID: String;
begin
  Result := 'Jupiter.Utils';
end;

function TJupiterUtils.Internal_GetModuleTitle: String;
begin
  Result := 'Utils: Pacote de utilitários';
end;

procedure TJupiterUtils.Internal_Prepare;
var
  vrWizard : TJupiterDatabaseWizard;
begin
  inherited Internal_Prepare;

  vrWizard := vrJupiterApp.NewWizard;
  try
    if not vrWizard.TableExists('FAVORITOS') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE FAVORITOS ( ID INTEGER PRIMARY KEY, ENDERECO VARCHAR (200), DESCRICAO VARCHAR(100))'));

    if not vrWizard.TableExists('TEMPOS') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE TEMPOS ( ID INTEGER PRIMARY KEY, DIA DATE, INICIO TIME, FIM TIME)'));

    if Self.Internal_CreateMacroIfDontExists('main.tools.favorites.click', 'Clique do item de menu Favoritos', CreateStringListToMacro('OpenGridFromTable(''FAVORITOS'');')) then
      Self.Internal_CreateRouteIfDontExists('Favoritos', '/main/tools/favorites/', vrWizard.GetLastID('MACROS'), ICON_FAVORITE, 200);

    if Self.Internal_CreateMacroIfDontExists('main.tools.times.click', 'Clique do item de menu Meus tempos', CreateStringListToMacro('OpenGridFromTable(''TEMPOS'');')) then
      Self.Internal_CreateRouteIfDontExists('Meus tempos', '/main/tools/times/', vrWizard.GetLastID('MACROS'), ICON_STARTTIME, 200);
  finally
    FreeAndNil(vrWizard);
  end;
end;

end.

