program jupiter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, JupiterConsts, JupiterObject, JupiterRoute, JupiterVariable,
  JupiterRunnable, JupiterAction, JupiterForm, JupiterModule, JupiterApp,
  pascalscript, JupiterToolsModule, uHome, uExplorer, JupiterGeneratorModule,
  JupiterStandardModule, uNewTask, JupiterDataProvider, JupiterFileDataProvider,
  JupiterDirectoryDataProvider, JupiterCSVDataProvider, uConfig,
  JupiterEnviroment, JupiterFormGenerator, JupiterFormField,
  JupiterVariableForm, JupiterDialogForm, uCurrentTask,
  JupiterTasksDataProvider, JupiterTaskTimesDataProvider, uEditor, uMessage,
  JupiterSystemMessage, JupiterGeneratorForm, JupiterXMLDataProvider,
  uNewAction, uNewField, JupiterGeneratorMenuItem, jupiterformutils, uNewParam,
  JupiterVariableDataProvider, uNewDataSet, uMain, uGenerator, jupiterScript,
  uScriptEditor, uCustomJupiterForm, jupiterScriptFunctions, uDynamicRecord,
  jupiterThread, uProcessMonitor, jupiterStream, uPrompt, ulayoutbuilder,
  jupiterclimodule, ulayoutreader, uclimanager, jupiterclicommand,
  unewcommandcli, utimecontrol, jupiterTimeControlDataProvider, 
uuserpreferences, jupiterformaction, udm, jupiterdatabase, usqlEditor,
umenuselector, uwebexplorer, jupiterexternaldatasets;

{$R *.res}

  function RunBeforeStart(var prOutPut : String) : String;
  var
    vrParams : TJupiterVariableList;
    vrRunnable : TJupiterRunnable;
  begin
    Result   := '';
    prOutPut := '';

    vrParams := TJupiterVariableList.Create;
    try
      vrParams.FileName := 'datasets/JupiterStandard.csv';

      if vrParams.HasValue('Jupiter.Standard.Triggers.OnBeforeStart') then
      begin
        Result := vrParams.VariableById('Jupiter.Standard.Triggers.OnBeforeStart').Value;

        vrRunnable := TJupiterRunnable.Create(Result, True);
        prOutPut := vrRunnable.OutPut;
        vrRunnable.Free;
      end;
    finally
      vrParams.Free;
    end;
  end;

var
  vrVez     : Integer;
  vrBefore  : String;
  vrMessage : TJupiterSystemMessage;
  vrOutPut  : String;
begin
  vrBefore := RunBeforeStart(vrOutPut);

  vrJupiterApp := TJupiterApp.Create('jupiter', 'Jupiter');

  for vrVez := 0 to ParamCount do
    vrJupiterApp.AddParam(ParamStr(vrVez));

  if not vrJupiterApp.ConsoleMode then
  begin
    RequireDerivedFormResource := True;
  Application.Title:='Jupiter';
  Application.Scaled:=True;
    Application.Initialize;
  end
  else
  begin
    {$apptype console}

    {$IFDEF WINDOWS}
    ShowWindow(GetConsoleWindow, SW_Show);
    {$ENDIF}

    Application.Initialize;
  end;

  if not vrJupiterApp.ConsoleMode then
  begin
    vrJupiterApp.AddMessage('Iniciando', Application.Title);

    if vrBefore <> '' then
    begin
      vrMessage := vrJupiterApp.AddMessage('Evento executado antes da inicialização', Application.Title);
      vrMessage.Details.Add('Comando: ' + vrBefore);
      vrMessage.Details.Add('');
      vrMessage.Details.Add('Execução:');
      vrMessage.Details.Add(vrOutPut);
    end;
  end;

  vrJupiterApp.AddModule(TJupiterStandardModule.Create);
  vrJupiterApp.AddModule(TJupiterCLIModule.Create);
  vrJupiterApp.AddModule(TJupiterToolsModule.Create);
  vrJupiterApp.AddModule(TJupiterGeneratorModule.Create);
  vrJupiterApp.AddModule(TJupiterDatabaseModule.Create);

  // Forms
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(ROOT_FORM_PATH, TFHome));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(CONFIG_PATH, TFConfig));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(EXPLORER_FORM_PATH, TFExplorer));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(CUSTOM_FORM_PATH, TFCustomJupiterForm));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(NEWTASK_FORM_PATH, TFNewTask));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(TASK_FORM_PATH, TFCurrentTask));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(EDITOR_FORM_PATH, TFEditor));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(SCRIPTEDITOR_FORM_PATH, TFScriptEditor));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(SQLEDITOR_FORM_PATH, TFSQLEditor));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(GENERATOR_FORM_PATH, TFGenerator));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(MENU_SELECT_FORM_PATH, TFMenuSelector));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(MESSAGES_PATH, TFMessage));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(DYNAMIC_RECORD_FORM_PATH, TFDynamicRecord));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(PROCESS_MONITOR_PATH, TFProcessMonitor));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(PROMPT_FORM_PATH, TFPrompt));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(LAYOUT_BUILDER_PATH, TFLayoutBuilder));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(LAYOUT_READER_PATH, TFLayoutReader));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(CLI_MANAGER_PATH, TFCLIManager));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(CLI_NEWCOMMAND_PATH, TFNewCommandCli));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(TIME_CONTROL_PATH, TFTimeControl));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(USER_PREF_PATH, TFUserPreferences));
  vrJupiterApp.FormRoutes.Add(TJupiterFormRoute.Create(WEB_EXPLORER_FORM_PATH, TFWebExplorer));

  Application.CreateForm(TFMain, FMain);
  Application.CreateForm(TDMMain, DMMain);
  Application.CreateForm(TFMenuSelector, FMenuSelector);
  Application.CreateForm(TFWebExplorer, FWebExplorer);
  Application.Run;
end.

