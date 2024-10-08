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
  vrStr : TStrings;
begin
  inherited Internal_Prepare;

  vrWizard := vrJupiterApp.NewWizard;
  vrStr := TStringList.Create;
  try
    if not vrWizard.TableExists('FAVORITOS') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE FAVORITOS ( ID INTEGER PRIMARY KEY, DESCRICAO VARCHAR(100), ENDERECO VARCHAR (200))'));

    vrStr.Clear;
    vrStr.Add('program macro;');
    vrStr.Add('const');
    vrStr.Add('  SCRIPTID = ''@FLAG_SCRIPTID'';');
    vrStr.Add('var');
    vrStr.Add('  vrOutput : String;');
    vrStr.Add('begin');
    vrStr.Add('  if FileOrFolderExists(GetParam(SCRIPTID, ''ENDERECO'')) then');
    vrStr.Add('  begin');
    vrStr.Add('    if FileExists(GetParam(SCRIPTID, ''ENDERECO'')) then');
    vrStr.Add('      OpenDocument(GetParam(SCRIPTID, ''ENDERECO''))');
    vrStr.Add('    else');
    vrStr.Add('      OpenFolder(GetParam(SCRIPTID, ''ENDERECO''))');
    vrStr.Add('  end');
    vrStr.Add('  else');
    vrStr.Add('    CreateProcess(GetParam(SCRIPTID, ''ENDERECO''), '''', vrOutput, False, True);');
    vrStr.Add('end.');

    Self.Internal_CreateRouteIfDontExists('Checklists', '/main/tools/checklists/', NULL_KEY, ICON_CHECK, 100);

    Self.Internal_CreateMacroIfDontExists('FAVORITOS.Executar.OnClick', 'Executar', vrStr);

    Self.Internal_CreateActionIfDontExists('FAVORITOS.Executar', 'Executar', 'FAVORITOS', ICON_PLAY, 1, Self.Internal_GetMacroById('FAVORITOS.Executar.OnClick'), Self.Internal_GetMacroById(EVENT_RECORD_ONENABLE), Self.Internal_GetMacroById(EVENT_RECORD_ONVISIBLE));

    if not vrWizard.TableExists('TEMPOS') then
      vrWizard.ExecuteScript(CreateStringList('CREATE TABLE TEMPOS ( ID INTEGER PRIMARY KEY, DIA DATE, INICIO TIME, FIM TIME, TAREFA INT, FOREIGN KEY (TAREFA) REFERENCES TAREFAS(ID))'));

    Self.Internal_CreateMacroIfDontExists('TEMPOS.MarcarHoraInicial.OnClick', 'Marcar hora inicial', CreateStringListToMacro(' DBRunScript('' INSERT INTO TEMPOS (DIA, INICIO) VALUES (DATE("now"), TIME("now")) ''); '));

    Self.Internal_CreateMacroIfDontExists('TEMPOS.MarcarHoraFinal.OnClick', 'Marcar hora final', CreateStringListToMacro(' DBRunScript('' UPDATE TEMPOS SET FIM = TIME("now") WHERE FIM IS NULL ''); '));

    Self.Internal_CreateMacroIfDontExists('TEMPOS.MarcarHoraInicial.Enabled', 'Marcar hora inicial: Habilidado', CreateStringListToMacro('  if not DBExists(''TEMPOS'', ''FIM IS NULL'') then ' + STRING_NEWLINE + '  SetParam(SCRIPTID, ''Result'', ''Y'')'));

    Self.Internal_CreateMacroIfDontExists('TEMPOS.MarcarHoraFinal.Enabled', 'Marcar hora final: Habilidado', CreateStringListToMacro('  if DBExists(''TEMPOS'', ''FIM IS NULL'') then ' + STRING_NEWLINE + '  SetParam(SCRIPTID, ''Result'', ''Y'')'));

    Self.Internal_CreateActionIfDontExists('TEMPOS.MarcarHoraInicial', 'Marcar hora inicial', 'TEMPOS', ICON_STARTTIME, 1, Self.Internal_GetMacroById('TEMPOS.MarcarHoraInicial.OnClick'), Self.Internal_GetMacroById('TEMPOS.MarcarHoraInicial.Enabled'), Self.Internal_GetMacroById(EVENT_TABLE_ONVISIBLE));
    Self.Internal_CreateActionIfDontExists('TEMPOS.MarcarHoraFinal', 'Marcar hora final', 'TEMPOS', ICON_ENDTIME, 1, Self.Internal_GetMacroById('TEMPOS.MarcarHoraFinal.OnClick'), Self.Internal_GetMacroById('TEMPOS.MarcarHoraFinal.Enabled'), Self.Internal_GetMacroById(EVENT_TABLE_ONVISIBLE));

    if Self.Internal_CreateMacroIfDontExists('main.tools.favorites.click', 'Clique do item de menu Favoritos', CreateStringListToMacro('OpenGridFromTable(''FAVORITOS'');')) then
      Self.Internal_CreateRouteIfDontExists('Favoritos', '/main/tools/favorites/', vrWizard.GetLastID('MACROS'), ICON_FAVORITE, 200);

    if Self.Internal_CreateMacroIfDontExists('main.tools.times.click', 'Clique do item de menu Meus tempos', CreateStringListToMacro('OpenGridFromTable(''TEMPOS'');')) then
      Self.Internal_CreateRouteIfDontExists('Meus tempos', '/main/tools/times/', vrWizard.GetLastID('MACROS'), ICON_STARTTIME, 300);
  finally
    FreeAndNil(vrStr);
    FreeAndNil(vrWizard);
  end;
end;

end.

