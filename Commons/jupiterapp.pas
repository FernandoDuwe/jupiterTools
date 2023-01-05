unit JupiterApp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ExtCtrls, JupiterModule, JupiterObject, JupiterRoute, JupiterForm,
  JupiterVariable, JupiterEnviroment, JupiterSystemMessage, PopUpNotifier,
  JupiterVariableDataProvider, SysUtils, Controls;

type

  { TJupiterApp }

  TJupiterApp = class(TJupiterObject)
  private
    FAppID            : String;
    FAppName          : String;
    FModules          : TJupiterModuleList;
    FMessages         : TJupiterObjectList;
    FFormRoutes       : TJupiterObjectList;
    FBodyPanel        : TPanel;
    FCurrentForm      : TFJupiterForm;
    FParams           : TJupiterVariableList;
    FUserParams       : TJupiterVariableList;
    FDataSetParams    : TJupiterVariableDataProviderList;
    FMainIcons        : TImageList;
    FPopupNotifier    : TPopupNotifier;
    FOnBeforeNavigate : TNotifyEvent;
    FOnAfterNavigate  : TNotifyEvent;

  protected
    procedure Internal_Prepare; virtual;

  published
    property AppID         : String         read FAppID;
    property AppName       : String         read FAppName;
    property BodyPanel     : TPanel         read FBodyPanel   write FBodyPanel;
    property CurrentForm   : TFJupiterForm  read FCurrentForm write FCurrentForm;
    property MainIcons     : TImageList     read FMainIcons   write FMainIcons;
    property PopupNotifier : TPopupNotifier read FPopupNotifier write FPopupNotifier;

    property FormRoutes     : TJupiterObjectList   read FFormRoutes    write FFormRoutes;
    property ModulesList    : TJupiterModuleList   read FModules       write FModules;
    property Messages       : TJupiterObjectList   read FMessages      write FMessages;
    property Params         : TJupiterVariableList read FParams        write FParams;
    property DataSetParams  : TJupiterVariableDataProviderList read FDataSetParams write FDataSetParams;
    property UserParams     : TJupiterVariableList read FUserParams    write FUserParams;

    property OnBeforeNavigate : TNotifyEvent read FOnBeforeNavigate write FOnBeforeNavigate;
    property OnAfterNavigate  : TNotifyEvent read FOnAfterNavigate  write FOnAfterNavigate;
  public
    procedure AddModule(prModule : TJupiterModule);
    function AddMessage(prTitle, prOrigin : String) : TJupiterSystemMessage;

    function GetVersion : String;
    function ConsoleMode : Boolean;

    procedure NavigateTo(prRoute : TJupiterRoute; prAsModal : Boolean);

    procedure Popup(prTitle : String; prStrMessage : TStrings);

    function GetActions(prRoute : TJupiterRoute) : TJupiterObjectList; virtual;

    constructor Create(prAppID, prAppName : String);
    destructor Destroy; override;
  end;

var
  vrJupiterApp : TJupiterApp;

implementation

uses FileInfo, Forms, JupiterConsts;

{ TJupiterApp }

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

    if not Self.Params.Exists('Enviroment.Run.ShellScript') then
       Self.Params.AddConfig('Enviroment.Run.ShellScript', 'cmd.exe', 'Aplicação que executará arquivo de bat ou shell');

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
  Result := False;
end;

procedure TJupiterApp.NavigateTo(prRoute: TJupiterRoute; prAsModal: Boolean);
var
  vrVez : Integer;
  vrVezDebug : Integer;
  vrFormRoute : TJupiterFormRoute;
  vrFormModal : TFJupiterForm;
begin
  if Assigned(Self.OnBeforeNavigate) then
    Self.OnBeforeNavigate(Self);

  try
    for vrVez := 0 to Self.FormRoutes.Size - 1 do
    begin
      vrFormRoute := TJupiterFormRoute(Self.FormRoutes.GetAtIndex(vrVez));

      if vrFormRoute.Path = prRoute.DestinyPath then
      begin
        if prAsModal then
        begin
          Application.CreateForm(vrFormRoute.FormClass, vrFormModal);
          try
            vrFormModal.IsModal := prAsModal;
            vrFormModal.Params.CopyValues(prRoute.Params);

            vrFormModal.ShowModal;
          finally
            vrFormModal.Release;
            FreeAndNil(vrFormModal);
          end;
        end
        else
        begin
          if Assigned(Self.CurrentForm) then
          begin
            Self.CurrentForm.Release;
            FreeAndNil(Self.FCurrentForm);
          end;

          Self.CurrentForm := TFJupiterForm(vrFormRoute.FormClass.Create(Self.BodyPanel));

          Self.CurrentForm.Parent      := Self.BodyPanel;
          Self.CurrentForm.WindowState := wsMaximized;
          Self.CurrentForm.BorderStyle := bsNone;
          Self.CurrentForm.Align       := alClient;
          Self.CurrentForm.IsModal     := prAsModal;

          Self.CurrentForm.Params.CopyValues(prRoute.Params);

          Self.CurrentForm.Show;

          Exit;
        end;
      end;
    end;
  finally
    if Assigned(Self.OnAfterNavigate) then
      Self.OnAfterNavigate(Self);
  end;
end;

procedure TJupiterApp.Popup(prTitle : String; prStrMessage : TStrings);
var
  vrVez : Integer;
begin
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
end;

function TJupiterApp.GetActions(prRoute: TJupiterRoute): TJupiterObjectList;
var
  vrVez : Integer;
begin
  Result := TJupiterObjectList.Create;

  for vrVez := 0 to Self.ModulesList.Size - 1 do
    Result.Merge(TJupiterModule(Self.ModulesList.GetAtIndex(vrVez)).GetActions(prRoute));
end;

constructor TJupiterApp.Create(prAppID, prAppName: String);
begin
  Self.FAppID   := prAppID;
  Self.FAppName := prAppName;

  Self.FFormRoutes := TJupiterObjectList.Create;
  Self.FModules    := TJupiterModuleList.Create;

  Self.FMessages      := TJupiterObjectList.Create;
  Self.FParams        := TJupiterVariableList.Create;
  Self.FDataSetParams := TJupiterVariableDataProviderList.Create;
  Self.FUserParams    := TJupiterVariableList.Create;

  Self.Params.AddChildList(Self.DataSetParams);
  Self.Params.AddChildList(Self.UserParams);

  Self.Internal_Prepare;
end;

destructor TJupiterApp.Destroy;
begin
  FreeAndNil(Self.FFormRoutes);
  FreeAndNil(Self.FMessages);
  FreeAndNil(Self.FModules);
  FreeAndNil(Self.FParams);
  FreeAndNil(Self.FDataSetParams);
  FreeAndNil(Self.FUserParams);

  inherited Destroy;
end;

end.

