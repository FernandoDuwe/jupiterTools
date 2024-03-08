library jupiter_notepad_plugin;

{$MODE Delphi}

uses
  SysUtils,
  Classes,
  Types,
  Interfaces,
  LCLIntf,
  LCLType,
  LMessages,
  Forms,
  Messages,
  nppplugin in 'lib\nppplugin.pas',
  scisupport in 'lib\SciSupport.pas',
  NppForms in 'lib\NppForms.pas' {NppForm},
  NppDockingForms in 'lib\NppDockingForms.pas' {NppDockingForm},
  windows, jupiter_plugin, uPluginConfig, ucurrentTaskPlugin, JupiterVariable,
  JupiterObject, JupiterConsts, JupiterDialogForm, JupiterApp,
  JupiterStandardModule, uCustomJupiterForm, udm, pascalscript, pascalscriptfcl;

{$Include 'lib\NppPluginInclude.pas'}
{$R *.res}

begin
  Dll_Process_Detach_Hook:= @DLLEntryPoint;

  DLLEntryPoint(DLL_PROCESS_ATTACH);
  Application.Initialize;
end.

