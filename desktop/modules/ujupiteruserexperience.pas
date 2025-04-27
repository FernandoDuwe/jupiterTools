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

    if Self.Internal_CreateMacroIfDontExists('menu.show.userPref.click', 'Clique do botão Preferências do usuário', CreateStringList('program macro;' + #13#10 + 'begin' + #13#10 + '  OpenForm(''/forms/userPreference'');' + #13#10 + 'end.')) then
      Self.Internal_CreateRouteIfDontExists('Preferências', '/menu/show/userPref/', vrWizard.GetLastID('MACROS'), ICON_APPLICATION, 100);

    Self.Internal_CreateVariablIfDontExists(FIELD_FONT_SIZE, 'Tamanho da fonte', '12');

    Self.Internal_CreateVariablIfDontExists(FIELD_FONT_NAME, 'Nome da fonte', 'Calibri');

    Self.Internal_CreateVariablIfDontExists(FIELD_EDITABLES_FONT_NAME, 'Nome da fonte: Para componentes editáveis', 'Calibri');

    Self.Internal_CreateVariablIfDontExists(FIELD_MEMO_FONT_NAME, 'Nome da fonte: Para componentes Memo', 'Calibri');

    Self.Internal_CreateVariablIfDontExists(FIELD_SYNEDIT_FONT_NAME, 'Nome da fonte: Para componentes SynEdit', 'DejaVu Sans Mono');

    Self.Internal_CreateVariablIfDontExists(FORM_EDITOR_RIGHTEDGE, 'Linha de limite do editor de scripts', '120');

    Self.Internal_CreateVariablIfDontExists(FORM_UPDATE_TIME, 'Tempo de atualização dos formulários; (em milisegundos)', '15000');

    Self.Internal_CreateVariablIfDontExists(FORM_ALWAYS_MODAL, 'Formulário ao abrir sempre modal', BOOL_TRUE_STR);

    Self.Internal_CreateVariablIfDontExists(CONTEXTMENU_TABLE_SHOW, 'Sempre exibir as tabelas no menu de contexto', BOOL_TRUE_STR);

    Self.Internal_CreateVariablIfDontExists(FORM_GRID_LIMIT, 'Quantidade de registros por página', '50');

    Self.Internal_CreateVariablIfDontExists(DEBUG_MODE, 'Debug', BOOL_FALSE_STR);

    Self.Internal_CreateVariablIfDontExists(FORM_DESCRIPTION_MAXSIZE, 'Tamanho máximo das descrições de registro', '50');

    Self.Internal_CreateVariablIfDontExists('Interface.Grid.ShowMiniForm', 'Exibir sempre o resumo dos dados em formulários de grid', BOOL_FALSE_STR);
  finally
    FreeAndNil(vrWizard);
  end;
end;

end.

