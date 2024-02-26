unit JupiterConsts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
   ROOT_PATH                : String = '/';
   ROOT_FORM_PATH           : String = '/forms/';
   CONFIG_PATH              : String = '/forms/config/';
   EXPLORER_FORM_PATH       : String = '/forms/explorer/';
   WEB_EXPLORER_FORM_PATH   : String = '/forms/explorer/web/';
   EDITOR_FORM_PATH         : String = '/forms/editor/';
   SCRIPTEDITOR_FORM_PATH   : String = '/forms/editor/jpas/';
   SQLEDITOR_FORM_PATH      : String = '/forms/editor/SQL/';
   GENERATOR_FORM_PATH      : String = '/forms/generator/';
   MESSAGES_PATH            : String = '/forms/messages/';
   CUSTOM_FORM_PATH         : String = '/forms/custom/';
   NEWTASK_FORM_PATH        : String = '/forms/custom/newTask/';
   TASK_FORM_PATH           : String = '/forms/custom/currentTask/';
   DYNAMIC_RECORD_FORM_PATH : String = '/forms/custom/record/dynamic/';
   MENU_SELECT_FORM_PATH    : String = '/forms/menu/selector/';
   PROCESS_MONITOR_PATH     : String = '/forms/process/monitor/';
   PROMPT_FORM_PATH         : String = '/forms/prompt/';
   LAYOUT_BUILDER_PATH      : String = '/forms/layout/builder/';
   LAYOUT_READER_PATH       : String = '/forms/layout/reader/';
   CLI_MANAGER_PATH         : String = '/forms/cli/manager/';
   CLI_NEWCOMMAND_PATH      : String = '/forms/cli/newCommand/';
   TIME_CONTROL_PATH        : String = '/forms/timeControl/';
   USER_PREF_PATH           : String = '/forms/config/preferences';

   MENU_FILE_PATH       : String = '/menu/file/';
   MENU_FILE_NEW_PATH   : String = '/menu/file/new/';
   MENU_FILE_OPEN_PATH  : String = '/menu/file/open/';
   MENU_EDIT_PATH       : String = '/menu/edit/';
   MENU_TOOLS_PATH      : String = '/menu/tools/';
   MENU_FOLDERS_PATH    : String = '/menu/folders/';
   MENU_HELP_PATH       : String = '/menu/help/';

   ICON_ADD       : SmallInt = 0;
   ICON_NEW       : SmallInt = 1;
   ICON_OPEN      : SmallInt = 2;
   ICON_SAVE      : SmallInt = 3;
   ICON_EDIT      : SmallInt = 4;
   ICON_DELETE    : SmallInt = 5;
   ICON_CHECK     : SmallInt = 6;
   ICON_CANCEL    : SmallInt = 7;
   ICON_MARK      : SmallInt = 8;
   ICON_HOME      : SmallInt = 9;
   ICON_UP        : SmallInt = 10;
   ICON_DOWN      : SmallInt = 11;
   ICON_LEFT      : SmallInt = 12;
   ICON_RIGHT     : SmallInt = 13;
   ICON_CONFIG    : SmallInt = 14;
   ICON_REFRESH   : SmallInt = 15;
   ICON_HELP      : SmallInt = 16;
   ICON_TASKS     : SmallInt = 17;
   ICON_CURTASK   : SmallInt = 18;
   ICON_TOOLS     : SmallInt = 19;
   ICON_RECORDS   : SmallInt = 20;
   ICON_STARTTIME : SmallInt = 21;
   ICON_ENDTIME   : SmallInt = 22;
   ICON_PLAY      : SmallInt = 23;
   ICON_IMPORTANT_MESSAGE : SmallInt = 24;
   ICON_FAVORITE    : SmallInt = 25;
   ICON_APPLICATION : SmallInt = 26;
   ICON_LIBRARY     : SmallInt = 27;
   ICON_GRAPHICFILE : SmallInt = 28;
   ICON_DOCFILE     : SmallInt = 29;
   ICON_TECHFILE    : SmallInt = 30;
   ICON_TXTFILE     : SmallInt = 31;
   ICON_SHEETFILE   : SmallInt = 32;
   ICON_TIMEFILE    : SmallInt = 33;
   ICON_PICTUREFILE : SmallInt = 34;
   ICON_COPY        : SmallInt = 35;
   ICON_EXIT        : SmallInt = 36;
   ICON_WIZARD      : SmallInt = 37;

   NULL_KEY : SmallInt = -1;

   EMPTY_SPACE_SEPARATOR  : String = '/JUPITERTOOLS\|/JUPITERTOOLS\';
   COLUMN_SPACE_SEPARATOR : String = '      ';
   FIELD_ID_GENERADOR     : String = 'Generator.FormId';
   FIELD_TREE_COLAPSE     : String = 'MenuTree_Colapse';
   FIELD_FONT_SIZE        : String = 'Interface.Font.Size';

   CSV_SEPARATOR_REPLACER : String = '[SEMICOLON]';

   DATAPROVIDER_TYPE_LIST_FILES : String = 'TJupiterFileDataProvider';
   DATAPROVIDER_TYPE_LIST_PATHS : String = 'TJupiterDirectoryDataProvider';
   DATAPROVIDER_TYPE_LIST_CSV   : String = 'TJupiterCSVDataProvider';
   DATAPROVIDER_TYPE_TASKS      : String = 'TJupiterTasksDataProvider';
   DATAPROVIDER_TYPE_XML        : String = 'TJupiterXMLDataProvider';

   // Providers
   PROVIDER_LIST : Array[0..4] of String = ('TJupiterFileDataProvider',
                                            'TJupiterDirectoryDataProvider',
                                            'TJupiterCSVDataProvider',
                                            'TJupiterTasksDataProvider',
                                            'TJupiterXMLDataProvider');

   // Forms
   FORM_MARGIN_TOP           : Integer = 8;
   FORM_MARGIN_LEFT          : Integer = 8;
   FORM_MARGIN_RIGHT         : Integer = 16;
   FORM_MARGIN_BOTTOM        : Integer = 8;

   {$IFDEF WINDOWS}
     FORM_MARGIN_BOTTOM_TONEXT : Integer = 10; // Bottom to the next field
   {$ELSE}
     FORM_MARGIN_BOTTOM_TONEXT : Integer = 20; // Bottom to the next field
   {$ENDIF}

   FORM_ACTION_MINWIDTH         : Integer = 120;
   FORM_ACTION_MINWIDTH_COMPACT : Integer = 80;

   // Generator
   GENERATOR_SYSLAYER : SmallInt = 1000;

   FIELD_TYPE_EDIT  : String = 'Edit';
   FIELD_TYPE_COMBO : String = 'Combo';

   // JupiterScripts (JPAS)
   JPAS_INCLUDE : String = '';
   JPAS_FLAG_GENERATEFULLFILE : String = '@FLAG_SAVE_COMPILED_FILE';
   JPAS_FLAG_USERCOMMAND : String = '@FLAG_USER_COMMAND';

   function GetCurrentOS : String;
   function GetDirectorySeparator : String;
   function GetRootDirectory : String;
   function GetCommandLineTool : String;
   function SearchIsPartOf(prData, prQuery : String) : Boolean;

type
  // JupiterThreads
  TJupiterThreadsStatus = (jtsNotStarted, jtsRunning, jtsFinished);
  TJupiterSearchMode = (jsmForm, jsmActions);

implementation

function GetCurrentOS: String;
begin
  {$IFDEF WINDOWS}
     Result := 'Windows';
  {$ELSE}
     Result := 'Linux';
  {$ENDIF}
end;

function GetDirectorySeparator: String;
begin
  {$IFDEF WINDOWS}
     Result := '\';
  {$ELSE}
     Result := '/';
  {$ENDIF}
end;

function GetRootDirectory: String;
begin
  {$IFDEF WINDOWS}
     Result := 'C:\';
  {$ELSE}
     Result := '/home/';
  {$ENDIF}
end;

function GetCommandLineTool : String;
begin
  {$IFDEF WINDOWS}
     Result := 'cmd.exe';
  {$ELSE}
     Result := 'terminal';
  {$ENDIF}
end;

function SearchIsPartOf(prData, prQuery: String): Boolean;
begin
  prData  := AnsiUpperCase(prData);
  prQuery := AnsiUpperCase(prQuery);

  Result := Pos(prQuery, prData) <> 0;
end;

end.

