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
  Self.FVariables.FileName := './jupiter_notepad_plugin.csv';

  Self.PluginName := 'Jupiter';

  Self.AddFuncItem(AnsiToUtf8('Tarefa'), _OpenCurrentTask);
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
    vrConfig.ShowModal;
  finally
    vrConfig.Free;
  end;
end;

procedure TJupiterPlugin.OpenCurrentTask;
begin
  if (not Assigned(FCurrentTaskPlugin)) then
     FCurrentTaskPlugin := TFCurrentTaskPlugin.Create(Self, 1);

  FCurrentTaskPlugin.Show;
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

