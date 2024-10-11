unit JupiterConsts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
   ROOT_PATH : String = '/';
   NEWTASKMENU_PATH : String = '/forms/newTask';
   CONFIG_PATH : String = '/forms/config';
   GENERATOR_PATH : String = '/forms/generator';
   USERPREFERENCE_PATH : String = '/forms/userPreference';
   CUSTOMDATABASE_PATH : String = '/forms/custom/database';
   CUSTOMGRIDDATABASE_PATH : String = '/forms/custom/databaseGrid';
   SCRIPTFORM_PATH : String = '/forms/script';
   SYSTEM_PATH : String = '/forms/system';

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

   BOOL_TRUE_STR : String = 'Y';
   BOOL_FALSE_STR : String = 'N';

   STRING_NEWLINE : String = #13#10;

   EMPTY_SPACE_SEPARATOR  : String = '/JUPITERTOOLS\|/JUPITERTOOLS\';
   COLUMN_SPACE_SEPARATOR : String = '      ';
   FIELD_ID_GENERADOR     : String = 'Generator.FormId';
   FIELD_TREE_COLAPSE     : String = 'MenuTree_Colapse';

   FORM_ALWAYS_MODAL      : String = 'Interface.Form.AlwaysModal';
   FIELD_FONT_SIZE        : String = 'Interface.Font.Size';

   CSV_SEPARATOR_REPLACER : String = '[SEMICOLON]';

   DATAPROVIDER_TYPE_LIST_FILES : String = 'TJupiterFileDataProvider';
   DATAPROVIDER_TYPE_LIST_PATHS : String = 'TJupiterDirectoryDataProvider';
   DATAPROVIDER_TYPE_LIST_CSV   : String = 'TJupiterCSVDataProvider';
   DATAPROVIDER_TYPE_TASKS      : String = 'TJupiterTasksDataProvider';
   DATAPROVIDER_TYPE_XML        : String = 'TJupiterXMLDataProvider';

   // Providers
   PROVIDER_LIST : Array[0..1] of String = ('TJupiterCSVDataProvider',
                                            'TJupiterTasksDataProvider');

   // Forms
   FORM_MARGIN_TOP           : Integer = 10;
   FORM_MARGIN_LEFT          : Integer = 10;
   FORM_MARGIN_RIGHT         : Integer = 20;
   FORM_MARGIN_BOTTOM        : Integer = 10;

   {$IFDEF WINDOWS}
     FORM_MARGIN_BOTTOM_TONEXT : Integer = 20; // Bottom to the next field
   {$ELSE}
     FORM_MARGIN_BOTTOM_TONEXT : Integer = 30; // Bottom to the next field
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
   JPAS_FLAG_SCRIPTID : String = '@FLAG_SCRIPTID';

   // Triggers
   TRIGGER_ONSTART   : String = 'triggers.onStart';
   TRIGGER_ONPROMPT  : String = 'triggers.onPrompt';

   // Events
   EVENT_RECORD_ONENABLE : String = 'events.record.onEnable';
   EVENT_TABLE_ONENABLE  : String = 'events.table.onEnable';
   EVENT_RECORD_ONVISIBLE : String = 'events.record.onVisible';
   EVENT_TABLE_ONVISIBLE  : String = 'events.table.onVisible';

   function GetCurrentOS : String;
   function GetDirectorySeparator : String;
   function GetRootDirectory : String;
   function GetCommandLineTool : String;
   function SearchIsPartOf(prData, prQuery : String) : Boolean;
   function CreateStringList(prContent : String) : TStrings;
   function CreateStringListToMacro(prCommand : String) : TStrings;

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

function CreateStringList(prContent: String): TStrings;
begin
  Result := TStringList.Create;
  Result.Text := prContent;
end;

function CreateStringListToMacro(prCommand: String): TStrings;
begin
  Result := TStringList.Create;
  Result.Clear;
  Result.Add('program macro;');
  Result.Add('const');
  Result.Add('  SCRIPTID = ''' + JPAS_FLAG_SCRIPTID + ''';');
  Result.Add(EmptyStr);
  Result.Add('  // Include libraries');
  Result.Add(EmptyStr);
  Result.Add('begin');
  Result.Add('  ' + prCommand);
  Result.Add('end.');
end;

end.

