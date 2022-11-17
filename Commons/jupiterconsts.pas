unit JupiterConsts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
   ROOT_PATH           : String = '/';
   ROOT_FORM_PATH      : String = '/forms/';
   CONFIG_PATH         : String = '/forms/config/';
   EXPLORER_FORM_PATH  : String = '/forms/explorer/';
   EDITOR_FORM_PATH    : String = '/forms/editor/';
   GENERATOR_FORM_PATH : String = '/forms/generator/';
   MESSAGES_PATH       : String = '/forms/messages/';
   CUSTOM_FORM_PATH    : String = '/forms/custom/';
   NEWTASK_FORM_PATH   : String = '/forms/custom/newTask/';
   TASK_FORM_PATH      : String = '/forms/custom/currentTask/';

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

   NULL_KEY : SmallInt = -1;

   EMPTY_SPACE_SEPARATOR  : String = '/JUPITERTOOLS\|/JUPITERTOOLS\';
   COLUMN_SPACE_SEPARATOR : String = '      ';
   FIELD_ID_GENERADOR     : String = 'Generator.FormId';

   DATAPROVIDER_TYPE_LIST_FILES : String = 'TJupiterFileDataProvider';
   DATAPROVIDER_TYPE_LIST_PATHS : String = 'TJupiterDirectoryDataProvider';
   DATAPROVIDER_TYPE_LIST_CSV   : String = 'TJupiterCSVDataProvider';
   DATAPROVIDER_TYPE_TASKS      : String = 'TJupiterTasksDataProvider';

   // Forms
   FORM_MARGIN_TOP      : Integer = 8;
   FORM_MARGIN_LEFT     : Integer = 8;
   FORM_MARGIN_RIGHT    : Integer = 16;
   FORM_MARGIN_BOTTOM   : Integer = 8;
   FORM_ACTION_MINWIDTH : Integer = 100;

   // Generator
   GENERATOR_SYSLAYER : SmallInt = 1000;

   function GetCurrentOS : String;
   function GetDirectorySeparator : String;

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

end.

