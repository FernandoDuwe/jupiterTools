unit jupiter_plugin;

{$MODE Delphi}

interface

uses
  NppPlugin, SysUtils, LCLIntf, LCLType, LMessages, SciSupport,
  JupiterDialogForm, JupiterVariable, JupiterApp, uCustomJupiterForm, uPluginConfig,
  Windows, ucurrentTaskPlugin;

type
  { TJupiterPlugin }
  TJupiterPlugin = class(TNppPlugin)
  private
    FVariables : TJupiterVariableList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure OpenConfig;
    procedure OpenCurrentTask;

    procedure DoNppnToolbarModification; override;
  end;

  procedure _OpenConfig; cdecl;
  procedure _OpenCurrentTask; cdecl;

var
  Npp: TJupiterPlugin;

implementation

uses Controls, Forms;

procedure _OpenConfig; cdecl;
begin
  Npp.OpenConfig;
end;

procedure _OpenCurrentTask; cdecl;
begin
  Npp.OpenCurrentTask;
end;

{ TJupiterPlugin }

constructor TJupiterPlugin.Create;
begin
  FCurrentTaskPlugin := nil;

  Self.FVariables := TJupiterVariableList.Create;
  Self.FVariables.FileName := './plugins/jupiter_notepad_plugin/jupiter_notepad_plugin.csv';

  Self.PluginName := 'Jupiter';

  Self.AddFuncItem(AnsiToUtf8('Tarefa atual'), _OpenCurrentTask);
  Self.AddFuncItem(AnsiToUtf8('Config'), _OpenConfig);
end;

destructor TJupiterPlugin.Destroy;
begin
  FreeAndNil(Self.FVariables);

  inherited Destroy;
end;

procedure TJupiterPlugin.OpenConfig;
var
  vrConfig : TFPluginConfig;
begin
  vrConfig := TFPluginConfig.Create(Self);
  try
    if Self.FVariables.Exists('Jupiter.Home') then
      vrConfig.edPath.Text := Self.FVariables.VariableById('Jupiter.Home').Value;

    vrConfig.ShowModal;

    if vrConfig.OkClick then
      Self.FVariables.AddConfig('Jupiter.Home', vrConfig.edPath.Text, 'Jupiter home path');
  finally
    vrConfig.Free;
  end;
end;

procedure TJupiterPlugin.OpenCurrentTask;
var
  vrVariable : TJupiterVariableList;
begin
  vrVariable := TJupiterVariableList.Create;
  try
    vrVariable.FileName := Self.FVariables.VariableById('Jupiter.Home').Value + 'datasets\JupiterTools.csv';

    if (not Assigned(FCurrentTaskPlugin)) then
      FCurrentTaskPlugin := TFCurrentTaskPlugin.Create(Self, 1);

    FCurrentTaskPlugin.TaskPath := vrVariable.VariableById('Jupiter.Tools.Tasks.Current.Path').Value;

    FCurrentTaskPlugin.Show;
  finally
    vrVariable.Free;
  end;
end;

procedure TJupiterPlugin.DoNppnToolbarModification;
var
  tb: TToolbarIcons;
begin
  tb.ToolbarIcon := 0;
  tb.ToolbarBmp := LoadImage(Hinstance, 'IDB_TB_TEST', IMAGE_BITMAP, 0, 0, (LR_DEFAULTSIZE or LR_LOADMAP3DCOLORS));
  SendMessage(self.NppData.NppHandle, NPPM_ADDTOOLBARICON, WPARAM(self.CmdIdFromDlgId(1)), LPARAM(@tb));
end;

initialization
  Npp := TJupiterPlugin.Create;

end.

