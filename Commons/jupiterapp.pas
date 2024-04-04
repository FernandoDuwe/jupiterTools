unit JupiterApp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, {$IFNDEF JUPITERCLI} ExtCtrls, JupiterFormTab, {$ENDIF}
  JupiterModule, JupiterObject, JupiterRoute, {$IFNDEF JUPITERCLI} JupiterForm, {$ENDIF}
  JupiterVariable, JupiterEnviroment, JupiterSystemMessage, {$IFNDEF JUPITERCLI} PopUpNotifier, {$ENDIF}
  JupiterVariableDataProvider, jupiterThread, JupiterRunnable, jupiterScript,
  jupiterexternaldatasets, JupiterCSVDataProvider, jupitershortcut, SysUtils,
  Controls;

type

  { TJupiterApp }

  TJupiterApp = class(TJupiterObject)
  private
    FAppID            : String;
    FAppName          : String;
    FModules          : TJupiterModuleList;
    FMessages         : TJupiterObjectList;
    FThreads          : TJupiterThreadList;
    FFormRoutes       : TJupiterObjectList;

    {$IFNDEF JUPITERCLI}
    FJupiterFormTab   : TJupiterFormTab;
    FBodyPanel        : TPanel;
    FCurrentForm      : TFJupiterForm;
    {$ENDIF}

    FParams           : TJupiterVariableList;
    FUserParams       : TJupiterVariableList;
    FRouteParams      : TJupiterVariableList;
    FDataSetParams    : TJupiterVariableDataProviderList;

    {$IFNDEF JUPITERCLI}
    FMainIcons         : TImageList;
    FPopupNotifier     : TPopupNotifier;
    {$ENDIF}

    FOnBeforeNavigate  : TNotifyEvent;
    FOnAfterNavigate   : TNotifyEvent;
    FOnShowPopup       : TNotifyEvent;
    FParamList         : TStrings;
    FCurrentRoute      : TJupiterRoute;
    FCurrentCLICommand : TJupiterObject;
    FShorcuts          : TJupiterShortcutList;

    procedure Internal_OpenFormUnique(prRoute : TJupiterFormRoute; prOriginalRoute : TJupiterRoute; prAsModal : Boolean);
    procedure Internal_OpenFormTabs(prRoute : TJupiterFormRoute; prOriginalRoute : TJupiterRoute; prAsModal : Boolean);
  protected
    procedure Internal_Prepare; virtual;

  published
    property AppID         : String         read FAppID;
    property AppName       : String         read FAppName;

    {$IFNDEF JUPITERCLI}
    property BodyPanel      : TPanel          read FBodyPanel      write FBodyPanel;
    property CurrentForm    : TFJupiterForm   read FCurrentForm    write FCurrentForm;
    property JupiterFormTab : TJupiterFormTab read FJupiterFormTab write FJupiterFormTab;
    {$ENDIF}


    property CurrentCLICommand : TJupiterObject read FCurrentCLICommand write FCurrentCLICommand;

    property CurrentRoute  : TJupiterRoute  read FCurrentRoute;

    {$IFNDEF JUPITERCLI}
    property MainIcons     : TImageList     read FMainIcons   write FMainIcons;
    property PopupNotifier : TPopupNotifier read FPopupNotifier write FPopupNotifier;
    {$ENDIF}

    property FormRoutes     : TJupiterObjectList   read FFormRoutes    write FFormRoutes;
    property ModulesList    : TJupiterModuleList   read FModules       write FModules;
    property Messages       : TJupiterObjectList   read FMessages      write FMessages;
    property Shortcuts      : TJupiterShortcutList read FShorcuts      write FShorcuts;
    property Threads        : TJupiterThreadList   read FThreads       write FThreads;
    property Params         : TJupiterVariableList read FParams        write FParams;
    property DataSetParams  : TJupiterVariableDataProviderList read FDataSetParams write FDataSetParams;
    property UserParams     : TJupiterVariableList read FUserParams    write FUserParams;

    property OnBeforeNavigate : TNotifyEvent read FOnBeforeNavigate write FOnBeforeNavigate;
    property OnAfterNavigate  : TNotifyEvent read FOnAfterNavigate  write FOnAfterNavigate;
    property OnShowPopup      : TNotifyEvent read FOnShowPopup      write FOnShowPopup;
  public
    procedure AddModule(prModule : TJupiterModule);
    function  AddMessage(prTitle, prOrigin : String) : TJupiterSystemMessage;
    procedure AddParam(prParam : String);

    function GetVersion : String;
    function ConsoleMode : Boolean;

    procedure NavigateTo(prRoute : TJupiterRoute; prAsModal : Boolean);

    procedure Popup(prTitle : String; prStrMessage : TStrings);

    function GetActions(prRoute : TJupiterRoute) : TJupiterObjectList; virtual;

    procedure ExecuteCommand(prParamList : TStrings); virtual;
    procedure ExecuteCommandFromParamList(); virtual;

    function HasRoute(prRoutePath: String): Boolean;
    function GoToRoute(prRoutePath: String): Boolean;

    procedure RunScript(prScript : TStrings);

    constructor Create(prAppID, prAppName : String);
    destructor Destroy; override;
  end;

var
  vrJupiterApp : TJupiterApp;

implementation

uses FileInfo, Forms, JupiterConsts {$IFNDEF JUPITERCLI}, JupiterFormTabSheet,  jupiterformutils {$ENDIF};

{ TJupiterApp }

procedure TJupiterApp.Internal_OpenFormUnique(prRoute: TJupiterFormRoute; prOriginalRoute : TJupiterRoute; prAsModal: Boolean);
{$IFNDEF JUPITERCLI}
var
  vrFormModal : TFJupiterForm;
  {$ENDIF}
begin
  {$IFNDEF JUPITERCLI}

  if prAsModal then
  begin
    Application.CreateForm(prRoute.FormClass, vrFormModal);
    try
      vrFormModal.IsModal := prAsModal;

      if ((Assigned(prRoute)) and (prRoute.Params.Count > 0)) then
        vrFormModal.Params.CopyValues(prRoute.Params);

      if ((Assigned(prOriginalRoute)) and (prOriginalRoute.Params.Count > 0)) then
        vrFormModal.Params.CopyValues(prOriginalRoute.Params);

      vrFormModal.ShowModal;
    finally
      vrFormModal.Release;
      FreeAndNil(vrFormModal);
    end;

    Exit;
  end
  else
  begin
    if Assigned(Self.CurrentForm) then
    begin
      Self.CurrentForm.Close;
      Self.CurrentForm.Release;
      FreeAndNil(Self.FCurrentForm);
    end;

    Self.CurrentForm := TFJupiterForm(prRoute.FormClass.Create(Self.BodyPanel));

    Self.CurrentForm.Parent      := Self.BodyPanel;
    Self.CurrentForm.WindowState := wsMaximized;
    Self.CurrentForm.BorderStyle := bsNone;
    Self.CurrentForm.Align       := alClient;
    Self.CurrentForm.IsModal     := prAsModal;

    if ((Assigned(prRoute)) and (prRoute.Params.Count > 0)) then
      Self.CurrentForm.Params.CopyValues(prRoute.Params);

    if ((Assigned(prOriginalRoute)) and (prOriginalRoute.Params.Count > 0)) then
      Self.CurrentForm.Params.CopyValues(prOriginalRoute.Params);

    Self.CurrentForm.Show;
  end;
  {$ENDIF}
end;

procedure TJupiterApp.Internal_OpenFormTabs(prRoute: TJupiterFormRoute; prOriginalRoute : TJupiterRoute; prAsModal: Boolean);
{$IFNDEF JUPITERCLI}
var
  vrFormModal : TFJupiterForm;
  vrVez : Integer;
{$ENDIF}
begin
  {$IFNDEF JUPITERCLI}

  if prAsModal then
  begin
    Application.CreateForm(prRoute.FormClass, vrFormModal);
    try
      vrFormModal.IsModal := prAsModal;

      if ((Assigned(prRoute)) and (prRoute.Params.Count > 0)) then
        vrFormModal.Params.CopyValues(prRoute.Params);

      if ((Assigned(prOriginalRoute)) and (prOriginalRoute.Params.Count > 0)) then
        vrFormModal.Params.CopyValues(prOriginalRoute.Params);

      vrFormModal.ShowModal;
    finally
      vrFormModal.Release;
      FreeAndNil(vrFormModal);
    end;

    Exit;
  end
  else
  begin
    Self.CurrentForm := TFJupiterForm(prRoute.FormClass.Create(Self.JupiterFormTab));

    Self.CurrentForm.WindowState := wsMaximized;
    Self.CurrentForm.BorderStyle := bsNone;
    Self.CurrentForm.Align       := alClient;
    Self.CurrentForm.IsModal     := prAsModal;

    if ((Assigned(prRoute)) and (prRoute.Params.Count > 0)) then
      Self.CurrentForm.Params.CopyValues(prRoute.Params);

    if ((Assigned(prOriginalRoute)) and (prOriginalRoute.Params.Count > 0)) then
      Self.CurrentForm.Params.CopyValues(prOriginalRoute.Params);

    for vrVez := 0 to Self.JupiterFormTab.PageCount - 1 do
    begin
      if not (Self.JupiterFormTab.Pages[vrVez] is TJupiterFormTabSheet) then
        Continue;

     if IsSameForm(TJupiterFormTabSheet(Self.JupiterFormTab.Pages[vrVez]).Form, Self.CurrentForm, [FIELD_ID_GENERADOR, 'Size', 'Hint.Success']) then
      begin
        Self.JupiterFormTab.ActivePageIndex := vrVez;

        if Assigned(Self.JupiterFormTab.OnChange) then
          Self.JupiterFormTab.OnChange(Self.JupiterFormTab.Page[vrVez]);

        Exit;
      end;
    end;

    Self.JupiterFormTab.AddForm(CurrentForm);
  end;

  {$ENDIF}
end;

procedure TJupiterApp.Internal_Prepare;
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.CreatePath('assets');
    vrEnviroment.CreatePath('datasets');
    vrEnviroment.CreatePath('modules');
    vrEnviroment.CreatePath('temp');

    Self.Params.FileName     := 'datasets/config.csv';
    Self.DataSetParams.FileName := 'datasets/DatasetsConfig.csv';
    Self.UserParams.FileName := 'datasets/userConfig.csv';

    if not Self.Params.Exists(Self.AppID + '.ExecutablePath') then
      Self.Params.AddVariable(Self.AppID + '.ExecutablePath', Application.ExeName, 'Caminho do Executável');

    if not Self.Params.Exists(Self.AppID + '.Path') then
      Self.Params.AddVariable(Self.AppID + '.Path', ExtractFileDir(Application.ExeName), 'Diretório do Executável');

    if not Self.Params.Exists(Self.AppID + '.Messages.Count') then
      Self.Params.AddVariable(Self.AppID + '.Messages.Count', IntToStr(Self.Messages.Size), 'Contador de Mensagens');

    if not Self.Params.Exists(Self.AppID + '.CurrentVersion') then
      Self.Params.AddVariable(Self.AppID + '.CurrentVersion', Self.GetVersion, 'Versão atual');

    if not Self.Params.Exists('Enviroment.ComputerName') then
      Self.Params.AddVariable('Enviroment.ComputerName', GetEnvironmentVariable('COMPUTERNAME'), 'Nome do computador');

    if not Self.Params.Exists('Enviroment.CurrentOS') then
      Self.Params.AddVariable('Enviroment.CurrentOS', GetCurrentOS, 'Sistema operacional atual');

    if not Self.Params.Exists('Enviroment.DirectorySeparator') then
      Self.Params.AddVariable('Enviroment.DirectorySeparator', DirectorySeparator, 'Separador de diretório');

    if not Self.Params.Exists('Enviroment.BasePath') then
      Self.Params.AddVariable('Enviroment.BasePath', GetRootDirectory, 'Diretório raiz');

    if not Self.Params.Exists('Enviroment.Run.ShellScript') then
       Self.Params.AddConfig('Enviroment.Run.ShellScript', GetCommandLineTool, 'Aplicação que executará arquivo de bat ou shell');

    if not Self.Params.Exists('Enviroment.DoNotChangeContentExtensions') then
       Self.Params.AddConfig('Enviroment.DoNotChangeContentExtensions', '.doc|.docx|.pdf', 'Extensões de arquivos que não terão seu conteúdo alterado');

    if not Self.Params.Exists('Enviroment.Run.EditorPref') then
       Self.Params.AddConfig('Enviroment.Run.EditorPref', 'notepad.exe', 'Editor preferencial (caminho ou comando)');

    if not Self.Params.Exists('Enviroment.Run.Method') then
       Self.Params.AddConfig('Enviroment.Run.Method', 'CreateProcess', 'Método de execução (RunCommand / ShellExecute / CreateProcess)');

    if not Self.Params.Exists('Enviroment.Run.OpenInEditorPrefExtensions') then
       Self.Params.AddConfig('Enviroment.Run.OpenInEditorPrefExtensions', '.txt|.sql|.md', 'Extensões de arquivos abertas em editor preferencial');

    if not Self.Params.Exists('Enviroment.Run.ScriptExtensions') then
       Self.Params.AddConfig('Enviroment.Run.ScriptExtensions', '.bat', 'Extensões de arquivos de script');

    if not Self.Params.Exists('Enviroment.WorkDir') then
      Self.Params.AddVariable('Enviroment.WorkDir', ExtractFileDir(Application.ExeName), 'Diretório de trabalho atual');

    if not Self.Params.Exists('Interface.Font.Size') then
       Self.Params.AddConfig('Interface.Font.Size', '8', 'Tamanho da fonte');

    if not Self.Params.Exists('Interface.CurrentForm.Title') then
       Self.Params.AddVariable('Interface.CurrentForm.Title', EmptyStr, 'Título do formulário atual');
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterApp.AddModule(prModule: TJupiterModule);
begin
  Self.ModulesList.Add(prModule);

  Self.Params.AddChildList(prModule.Params);
end;

function TJupiterApp.AddMessage(prTitle, prOrigin: String): TJupiterSystemMessage;
begin
  Result := TJupiterSystemMessage.Create(prTitle, prOrigin, EmptyStr);

  if Messages <> nil then
  begin
    Self.Messages.Add(Result);

    Self.Params.AddVariable(Self.AppID + '.Messages.Count', IntToStr(Self.Messages.Size), 'Contador de Mensagens');
  end;
end;

procedure TJupiterApp.AddParam(prParam: String);
begin
  Self.FParamList.Add(prParam);
end;

function TJupiterApp.GetVersion: String;
var
  vrVersionInfo : TVersionInfo;
begin
  Result := EmptyStr;

  vrVersionInfo := TVersionInfo.Create;
  try
    vrVersionInfo.Load(HINSTANCE);

    Result := Format('%0:d.%1:d.%2:d.%3:d', [vrVersionInfo.FixedInfo.FileVersion[0], vrVersionInfo.FixedInfo.FileVersion[1], vrVersionInfo.FixedInfo.FileVersion[2], vrVersionInfo.FixedInfo.FileVersion[3]]);
  finally
    if Assigned(vrVersionInfo) then
      vrVersionInfo.Free;
  end;
end;

function TJupiterApp.ConsoleMode: Boolean;
begin
  {$IFNDEF JUPITERCLI}
  Result := False;
  {$ENDIF}

  {$IFDEF JUPITERCLI}
  Result := True;
  {$ENDIF}
end;

procedure TJupiterApp.NavigateTo(prRoute: TJupiterRoute; prAsModal: Boolean);
{$IFNDEF JUPITERCLI}
var
  vrVez : Integer;
  vrVezDebug : Integer;
  vrFormRoute : TJupiterFormRoute;
{$ENDIF}
begin
  {$IFNDEF JUPITERCLI}
  Self.FCurrentRoute := prRoute;

  if Assigned(Self.OnBeforeNavigate) then
    Self.OnBeforeNavigate(Self);

  try
    for vrVez := 0 to Self.FormRoutes.Size - 1 do
    begin
      vrFormRoute := TJupiterFormRoute(Self.FormRoutes.GetAtIndex(vrVez));

      if vrFormRoute.Path = prRoute.DestinyPath then
      begin
        if Assigned(Self.BodyPanel) then
          Self.Internal_OpenFormUnique(vrFormRoute, prRoute, prAsModal);

        if Assigned(Self.JupiterFormTab) then
          Self.Internal_OpenFormTabs(vrFormRoute, prRoute, prAsModal);

        Exit;
      end;
    end;
  finally
    if Assigned(Self.OnAfterNavigate) then
      Self.OnAfterNavigate(Self);

    if vrJupiterApp.Params.Exists('Jupiter.Standard.Triggers.OnChangeRoute') then
          if Trim(vrJupiterApp.Params.VariableById('Jupiter.Standard.Triggers.OnChangeRoute').Value) <> EmptyStr then
             vrJupiterApp.Threads.NewThread('Gatilho: Ao alterar a rota atual', TJupiterRunnable.Create(vrJupiterApp.Params.VariableById('Jupiter.Standard.Triggers.OnChangeRoute').Value));
  end;
  {$ENDIF}
end;

procedure TJupiterApp.Popup(prTitle : String; prStrMessage : TStrings);
var
  vrVez : Integer;
begin
  {$IFNDEF JUPITERCLI}
  if Self.PopupNotifier.Visible then
    Self.PopupNotifier.Hide;

  Self.PopupNotifier.Text  := EmptyStr;
  Self.PopupNotifier.Title := prTitle;

  if prStrMessage.Count > 0 then
  begin
    Self.PopupNotifier.Text := prStrMessage[0];

    for vrVez := 1 to prStrMessage.Count -1 do
       Self.PopupNotifier.Text := Self.PopupNotifier.Text + LineEnding + prStrMessage[vrVez];
  end;

  Self.PopupNotifier.ShowAtPos(Screen.Width - 10, Screen.Height - 35);

  if Assigned(Self.OnShowPopup) then
    Self.OnShowPopup(Self);

  {$ENDIF}
end;

function TJupiterApp.GetActions(prRoute: TJupiterRoute): TJupiterObjectList;
var
  vrVez : Integer;
begin
  Result := TJupiterObjectList.Create;

  for vrVez := 0 to Self.ModulesList.Size - 1 do
    Result.Merge(TJupiterModule(Self.ModulesList.GetAtIndex(vrVez)).GetActions(prRoute));
end;

procedure TJupiterApp.ExecuteCommand(prParamList: TStrings);
var
  vrVez : Integer;
begin
  //WriteLn(Self.AppID + ' - ' + Self.AppName);
  //WriteLn('Versão: ' + Self.GetVersion);
  WriteLn('Versão: ');

  for vrVez := 0 to Self.ModulesList.Count - 1 do
    Self.ModulesList.GetModuleByIndex(vrVez).ExecuteCommand(prParamList);
end;

procedure TJupiterApp.ExecuteCommandFromParamList;
begin
  Self.ExecuteCommand(Self.FParamList);
end;

function TJupiterApp.HasRoute(prRoutePath: String): Boolean;
begin
  //
end;

function TJupiterApp.GoToRoute(prRoutePath: String): Boolean;
begin
  //
end;

procedure TJupiterApp.RunScript(prScript: TStrings);
var
  vrScript     : TJupiterScript;
  vrEnviroment : TJupiterEnviroment;
  vrJupiterMessage : TJupiterSystemMessage;
  vrVez : Integer;
begin
  vrScript     := TJupiterScript.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrScript.LoadFromFile(vrEnviroment.FullPath('modules/jpas/promptCommand.jpas'));

    vrScript.UserCommand := prScript.Text;
    vrScript.Execute;

    if not vrScript.Runned then
    begin
      if Self.ConsoleMode then
      begin
        WriteLn('Exception:');

        for vrVez := 0 to vrScript.Messages.Count - 1 do
          WriteLn(vrScript.Messages[vrVez]);
      end
      else
      begin
        vrJupiterMessage := Self.AddMessage('Erro ao executar', Self.ClassName);
        vrJupiterMessage.Details.AddStrings(vrScript.Messages);

        Self.Popup('Erro ao executar script', vrScript.Messages);
      end;
    end
    else
    begin
      vrJupiterMessage := Self.AddMessage('Comando executado', Self.ClassName);
      vrJupiterMessage.Details.AddStrings(vrScript.RunMessages);

      if Trim(vrScript.RunMessages.Text) <> EmptyStr then
        Self.Popup('Script', vrScript.RunMessages);
    end;
  finally
    FreeAndNil(vrScript);
    FreeAndNil(vrEnviroment);
  end;
end;

constructor TJupiterApp.Create(prAppID, prAppName : String);
var
  vrExternalDatasets : TJupiterExternalDatasets;
  vrVez         : Integer;
  vrVariables   : TJupiterVariableList;
begin
  Self.FAppID   := prAppID;
  Self.FAppName := prAppName;

  Self.FShorcuts := TJupiterShortcutList.Create;

  vrExternalDatasets := TJupiterExternalDatasets.Create;
  try
    Self.FFormRoutes := TJupiterObjectList.Create;
    Self.FModules    := TJupiterModuleList.Create;

    Self.FMessages      := TJupiterObjectList.Create;
    Self.FThreads       := TJupiterThreadList.Create;
    Self.FParams        := TJupiterVariableList.Create;
    Self.FDataSetParams := TJupiterVariableDataProviderList.Create;
    Self.FUserParams    := TJupiterVariableList.Create;

    Self.FParamList := TStringList.Create;
    Self.FParamList.Clear;

    for vrVez := 0 to vrExternalDatasets.Count - 1 do
      with TJupiterExternalDatasetItem(vrExternalDatasets.GetAtIndex(vrVez)) do
      begin
        vrVariables := TJupiterVariableList.Create;
        vrVariables.Title    := Name;
        vrVariables.FileName := FileName;

        Self.UserParams.AddChildList(vrVariables);
      end;

    Self.Params.AddChildList(Self.DataSetParams);
    Self.Params.AddChildList(Self.UserParams);

    Self.Internal_Prepare;
  finally
    FreeAndNil(vrExternalDatasets);
  end;
end;

destructor TJupiterApp.Destroy;
begin
  Self.FParamList.Clear;

  FreeAndNil(Self.FShorcuts);
  FreeAndNil(Self.FFormRoutes);
  FreeAndNil(Self.FMessages);
  FreeAndNil(Self.FThreads);
  FreeAndNil(Self.FModules);
  FreeAndNil(Self.FParams);
  FreeAndNil(Self.FDataSetParams);
  FreeAndNil(Self.FUserParams);
  FreeAndNil(Self.FParamList);

  inherited Destroy;
end;

end.

