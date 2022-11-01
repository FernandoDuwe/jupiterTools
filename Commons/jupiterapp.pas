unit JupiterApp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ExtCtrls, JupiterModule, JupiterObject, JupiterRoute, JupiterForm,
  JupiterVariable, JupiterEnviroment, JupiterSystemMessage, SysUtils, Controls;

type

  { TJupiterApp }

  TJupiterApp = class(TJupiterObject)
  private
    FAppID       : String;
    FAppName     : String;
    FModules     : TJupiterModuleList;
    FMessages    : TJupiterObjectList;
    FFormRoutes  : TJupiterObjectList;
    FBodyPanel   : TPanel;
    FCurrentForm : TFJupiterForm;
    FParams      : TJupiterVariableList;
    FUserParams  : TJupiterVariableList;
    FMainIcons   : TImageList;

  protected
    procedure Internal_Prepare; virtual;

  published
    property AppID       : String        read FAppID;
    property AppName     : String        read FAppName;
    property BodyPanel   : TPanel        read FBodyPanel   write FBodyPanel;
    property CurrentForm : TFJupiterForm read FCurrentForm write FCurrentForm;
    property MainIcons   : TImageList    read FMainIcons   write FMainIcons;

    property FormRoutes  : TJupiterObjectList   read FFormRoutes write FFormRoutes;
    property ModulesList : TJupiterModuleList   read FModules    write FModules;
    property Messages    : TJupiterObjectList   read FMessages   write FMessages;
    property Params      : TJupiterVariableList read FParams     write FParams;
    property UserParams  : TJupiterVariableList read FUserParams write FUserParams;
  public
    procedure AddModule(prModule : TJupiterModule);
    function AddMessage(prTitle, prOrigin : String) : TJupiterSystemMessage;

    function GetVersion : String;
    function ConsoleMode : Boolean;

    procedure NavigateTo(prRoute : TJupiterRoute; prAsModal : Boolean);

    function GetActions(prRoute : TJupiterRoute) : TJupiterObjectList; virtual;

    constructor Create(prAppID, prAppName : String);
    destructor Destroy; override;
  end;

var
  vrJupiterApp : TJupiterApp;

implementation

uses FileInfo, Forms;

{ TJupiterApp }

procedure TJupiterApp.Internal_Prepare;
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrEnviroment.CreatePath('datasets');
    vrEnviroment.CreatePath('modules');
    vrEnviroment.CreatePath('temp');

    Self.Params.FileName     := 'datasets/config.csv';
    Self.UserParams.FileName := 'datasets/userConfig.csv';

    if not Self.Params.Exists(Self.AppID + '.ExecutablePath') then
      Self.Params.AddVariable(Self.AppID + '.ExecutablePath', Application.ExeName, 'Caminho do Executável');

    if not Self.Params.Exists(Self.AppID + '.Path') then
      Self.Params.AddVariable(Self.AppID + '.Path', ExtractFileDir(Application.ExeName), 'Diretório do Executável');

    if not Self.Params.Exists(Self.AppID + '.Messages.Count') then
      Self.Params.AddVariable(Self.AppID + '.Messages.Count', IntToStr(Self.Messages.Size), 'Contador de Mensagens');

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

  Self.Messages.Add(Result);

  Self.Params.AddVariable(Self.AppID + '.Messages.Count', IntToStr(Self.Messages.Size), 'Contador de Mensagens');
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
  vrFormRoute : TJupiterFormRoute;
  vrFormModal : TFJupiterForm;
begin
  for vrVez := 0 to Self.FormRoutes.Size - 1 do
  begin
    vrFormRoute := TJupiterFormRoute(Self.FormRoutes.GetAtIndex(vrVez));

    if vrFormRoute.Path = prRoute.DestinyPath then
    begin
      if prAsModal then
      begin
        Application.CreateForm(vrFormRoute.FormClass, vrFormModal);
        try
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

        Self.CurrentForm.Params.CopyValues(prRoute.Params);

        Self.CurrentForm.Show;

        Exit;
      end;
    end;
  end;
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

  Self.FMessages   := TJupiterObjectList.Create;
  Self.FParams     := TJupiterVariableList.Create;
  Self.FUserParams := TJupiterVariableList.Create;

  Self.Internal_Prepare;
end;

destructor TJupiterApp.Destroy;
begin
  FreeAndNil(Self.FFormRoutes);
  FreeAndNil(Self.FMessages);
  FreeAndNil(Self.FModules);
  FreeAndNil(Self.FParams);
  FreeAndNil(Self.FUserParams);

  inherited Destroy;
end;

end.

