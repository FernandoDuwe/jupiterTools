unit uFileFinder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, uJupiterForm,
  jupiterStringUtils, JupiterDirectoryDataProvider, JupiterFileDataProvider,
  JupiterConsts, JupiterApp, uJupiterStringUtilsScript, uJupiterRunnableScript,
  uMain, uJupiterAction, uJupiterDesktopAppScript, LCLType, jupiterthread,
  JupiterModule, jupiterDatabaseWizard;

type

  { TJupiterFinderThread }

  { TFFileFinder }

  TFFileFinder = class(TFJupiterForm)
    tvFileTree: TTreeView;
    procedure edSearchKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure miUpdateClick(Sender: TObject);
    procedure tvFileTreeDblClick(Sender: TObject);
    procedure tvFileTreeKeyPress(Sender: TObject; var Key: char);
    procedure tvFileTreeSelectionChanged(Sender: TObject);
  private
    FSearch : String;
    FForceUpdate : Boolean;

    procedure Internal_OnOpenFolder(Sender: TObject);
    procedure Internal_OnStop(Sender: TObject);
    procedure Internal_OnDelete(Sender: TObject);

    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;

    procedure Internal_ProcessThread(prId, prThreadId : Integer; prParams : String);

    procedure Internal_ProcessedThead(prId, prThreadId : Integer; prParams : String);

    procedure Internal_PrepareForm; override;
    procedure Internal_ReadDirectory(prPath : String; prOwner : TTreeNode);
    procedure Internal_ReadWithThread(prPath : String);

    function Internal_EnableWorkMenu : Boolean; override;
    procedure Internal_AddToWorkMenu; override;

    function Internal_GetRouteName : String;
    function Internal_GetMacroName : String;
  public
    procedure ReadDirectory(prPath : String; prOwner : TTreeNode);
  end;

  TJupiterFinderThread = class (TJupiterThread)
  private
    FNode       : TTreeNode;
    FPath       : String;
    FNdode      : TTreeNode;
    FFileFinder : TFFileFinder;
  protected
    procedure Internal_Execute; override;
  published
    property FileFinder : TFFileFinder read FFileFinder write FFileFinder;
    property Path       : String       read FPath       write FPath;
    property Node       : TTreeNode    read FNode       write FNode;
  end;

var
  FFileFinder: TFFileFinder;

implementation

{$R *.lfm}

{ TJupiterFinderThread }

procedure TJupiterFinderThread.Internal_Execute;
begin
  inherited Internal_Execute;

  try
    Self.FileFinder.ReadDirectory(Self.Path, Self.Node);
  finally
    if Trim(Self.FileFinder.edSearch.Text) <> EmptyStr then
      if Self.Node.Count = 0 then
        Self.FileFinder.tvFileTree.Items.Delete(Self.Node)
      else
        Self.FileFinder.tvFileTree.FullExpand;
  end;
end;

{ TFFileFinder }

procedure TFFileFinder.edSearchKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Self.UpdateForm();

    Key := #0;
  end;
end;

procedure TFFileFinder.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FForceUpdate := False;
end;

procedure TFFileFinder.FormDestroy(Sender: TObject);
begin
  inherited;
end;

procedure TFFileFinder.MenuItem2Click(Sender: TObject);
begin
  inherited;

  try
    Self.ThreadController.StopAll;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFFileFinder.miUpdateClick(Sender: TObject);
begin
  Self.FForceUpdate := True;

  inherited;
end;

procedure TFFileFinder.tvFileTreeDblClick(Sender: TObject);
begin
  if not Assigned(tvFileTree.Selected) then
    Exit;

  if not Assigned(tvFileTree.Selected.Data) then
    Exit;

  JupiterRunnableScript_RunCommandOnJupiter(TJupiterStringReference(tvFileTree.Selected.Data).Reference);
end;

procedure TFFileFinder.tvFileTreeKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    tvFileTreeDblClick(Sender);

    Key := #0;
  end;
end;

procedure TFFileFinder.tvFileTreeSelectionChanged(Sender: TObject);
begin
  if not Assigned(tvFileTree.Selected) then
    Exit;

  if not Assigned(tvFileTree.Selected.Data) then
    Exit;

  Self.Params.VariableById('currentFile').Value := TJupiterStringReference(tvFileTree.Selected.Data).Reference;
end;

procedure TFFileFinder.Internal_OnOpenFolder(Sender: TObject);
begin
  JupiterRunnableScript_RunCommandOnJupiter(Self.Params.VariableById('path').Value);
end;

procedure TFFileFinder.Internal_OnStop(Sender: TObject);
begin
  try
    Self.ThreadController.StopAll;
  finally
    Self.UpdateForm();
  end;
end;

procedure TFFileFinder.Internal_OnDelete(Sender: TObject);
begin
  if not Assigned(tvFileTree.Selected) then
    Exit;

  if not Assigned(tvFileTree.Selected.Data) then
    Exit;

  if Application.MessageBox('Deseja realmente excluir?', 'Confirmação', MB_ICONQUESTION + MB_YESNO) = ID_YES then
  begin
    DeleteFile(TJupiterStringReference(tvFileTree.Selected.Data).Reference);

    Self.UpdateForm();
  end;
end;

procedure TFFileFinder.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  Self.Caption := 'Pesquisar: ' + jupiterStringUtilsGetLastPathName(Self.Params.VariableById('path').Value);

  Self.Hint := Self.Params.VariableById('path').Value + ' (Total de Threads: ' + IntToStr(Self.ThreadController.Count) + ')';

  edSearch.Enabled := not Self.ThreadController.Running;

  if Self.ActionGroup.Count > 1 then
  begin
    if Self.ThreadController.Running then
      Self.ActionGroup.GetActionAtIndex(1).Enable
    else
      Self.ActionGroup.GetActionAtIndex(1).Disable;
  end;

  if Self.ActionGroup.Count > 2 then
  begin
    if tvFileTree.Items.Count > 1 then
      Self.ActionGroup.GetActionAtIndex(2).Enable
    else
      Self.ActionGroup.GetActionAtIndex(2).Disable;
  end;
end;

procedure TFFileFinder.Internal_UpdateDatasets;
begin
  inherited Internal_UpdateDatasets;

  if not vrJupiterApp.Params.VariableById('Interface.Finder.AlwaysSearchEmptyQuery').AsBool then
    if Trim(edSearch.Text) = EmptyStr then
      Exit;

  if not Self.FForceUpdate then
    if Trim(edSearch.Text) <> EmptyStr then
      if edSearch.Text = Self.FSearch then
        Exit;

  Self.FForceUpdate := False;

  Self.ThreadController.StopAll;

  tvFileTree.Items.Clear;
  tvFileTree.SortType := stNone;

  JupiterAppDesktopCursorToWait;
  try
    Self.FSearch := edSearch.Text;

    Self.Internal_ReadDirectory(Self.Params.VariableById('path').Value, nil);
  finally
    tvFileTree.SortType := stText;

    if Trim(edSearch.Text) <> EmptyStr then
      tvFileTree.FullExpand;

    Self.FSearch := edSearch.Text;

    JupiterAppDesktopCursorToIdle;
  end;
end;

procedure TFFileFinder.Internal_ProcessThread(prId, prThreadId: Integer; prParams: String);
begin
  {
  try
    Self.Internal_ReadDirectory(prParams, vrNode);
  finally
    if Trim(edSearch.Text) <> EmptyStr then
      if vrNode.Count = 0 then
        tvFileTree.Items.Delete(vrNode)
      else
        tvFileTree.rFullExpand;
  end;
  }
end;

procedure TFFileFinder.Internal_ProcessedThead(prId, prThreadId: Integer; prParams: String);
begin
  Self.UpdateForm(False, True, False);
end;

procedure TFFileFinder.Internal_PrepareForm;
begin
  Self.FSearch := EmptyStr;

  inherited Internal_PrepareForm;

  Self.ShowSearchBar := True;

  Self.Hint := Self.Params.VariableById('path').Value;

  tvFileTree.Images := FMain.ilIconFamily;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Abrir pasta', 'Clique aqui para abrir a pasta atual externamente', ICON_OPEN, @Internal_OnOpenFolder));
  Self.ActionGroup.AddAction(TJupiterAction.Create('Parar', 'Clique aqui para parar a pesquisa', ICON_CANCEL, @Internal_OnStop));
  Self.ActionGroup.AddAction(TJupiterAction.Create('Excluir', 'Clique aqui para abrir a pasta atual externamente', ICON_DELETE, @Internal_OnDelete));
  Self.ActionGroup.AddAction(TJupiterAction.Create('Pesquisar', 'Clique aqui para efetuar a pesquisa', ICON_SEARCH, @Internal_OnOpenFolder));

  if not Self.Params.Exists('currentFile') then
    Self.Params.AddVariable('currentFile', EmptyStr);
end;

procedure TFFileFinder.Internal_ReadDirectory(prPath: String; prOwner: TTreeNode);
var
  vrDirectoryProvider : TJupiterDirectoryDataProvider;
  vrFileProvider : TJupiterFileDataProvider;
  vrVez : Integer;
  vrNode : TTreeNode;
  vrNodeFile : TTreeNode;
  vrThread : TJupiterFinderThread;
begin
  vrDirectoryProvider := TJupiterDirectoryDataProvider.Create;
  vrFileProvider := TJupiterFileDataProvider.Create;
  try
    vrDirectoryProvider.Path := prPath;
    vrDirectoryProvider.SubFolders := False;
    vrDirectoryProvider.ProvideData;

    for vrVez := 0 to vrDirectoryProvider.Count - 1 do
    begin
      if prOwner <> nil then
        vrNode := tvFileTree.Items.AddChild(prOwner, vrDirectoryProvider.GetRowByIndex(vrVez).Fields.VariableById('Folder').Value)
      else
        vrNode := tvFileTree.Items.Add(prOwner, vrDirectoryProvider.GetRowByIndex(vrVez).Fields.VariableById('Folder').Value);

      vrNode.ImageIndex := ICON_OPEN;
      vrNode.SelectedIndex := ICON_OPEN;
      vrNode.Data := TJupiterStringReference.Create(vrDirectoryProvider.GetRowByIndex(vrVez).Fields.VariableById('Path').Value);

      if ((vrJupiterApp.Params.VariableById(USE_THREADS_LOG_TASKS).AsBool) and (prOwner = nil)) then
      begin
        vrThread            := TJupiterFinderThread.Create(True);
        vrThread.Node       := vrNode;
        vrThread.FileFinder := Self;
        vrThread.Path       := vrDirectoryProvider.GetRowByIndex(vrVez).Fields.VariableById('Path').Value;
        vrThread.OnExecuted := @Internal_ProcessedThead;

        Self.ThreadController.AddThread(vrThread);
      end
      else
      begin
        Self.Internal_ReadDirectory(vrDirectoryProvider.GetRowByIndex(vrVez).Fields.VariableById('Path').Value, vrNode);

        if Trim(edSearch.Text) <> EmptyStr then
          if vrNode.Count = 0 then
            tvFileTree.Items.Delete(vrNode);
       end;
    end;

    vrFileProvider.Path := prPath;
    vrFileProvider.SubFolders := False;
    vrFileProvider.ProvideData;

    for vrVez := 0 to vrFileProvider.Count - 1 do
    begin
      if Trim(edSearch.Text) <> EmptyStr then
      begin
        if not jupiterStringUtilsIsValidSearch(prPath, edSearch.Text) then
          if not jupiterStringUtilsIsValidSearch(vrFileProvider.GetRowByIndex(vrVez).Fields.VariableById('FieldName').Value, edSearch.Text) then
            Continue;
      end;

      if prOwner <> nil then
        vrNodeFile := tvFileTree.Items.AddChild(prOwner, vrFileProvider.GetRowByIndex(vrVez).Fields.VariableById('FieldName').Value)
      else
        vrNodeFile := tvFileTree.Items.Add(prOwner, vrFileProvider.GetRowByIndex(vrVez).Fields.VariableById('FieldName').Value);

      vrNodeFile.Data := TJupiterStringReference.Create(vrFileProvider.GetRowByIndex(vrVez).Fields.VariableById('File').Value);

      vrNodeFile.ImageIndex := ICON_NEW;
      vrNodeFile.SelectedIndex := ICON_NEW;
    end;
  finally
    FreeAndNil(vrDirectoryProvider);
    FreeAndNil(vrFileProvider);
  end;
end;

procedure TFFileFinder.Internal_ReadWithThread(prPath: String);
begin

end;

function TFFileFinder.Internal_EnableWorkMenu: Boolean;
begin
  Result := True;
end;

procedure TFFileFinder.Internal_AddToWorkMenu;
var
  vrModule : TJupiterModule;
  vrWizard : TJupiterDatabaseWizard;
begin
  inherited Internal_AddToWorkMenu;

  vrWizard := vrJupiterApp.NewWizard;
  vrModule := TJupiterModule.Create;
  try
    if vrModule.CreateMacroIfDontExists(Self.Internal_GetMacroName, 'Clique do item de menu ' + ExtractFileName(Self.Params.VariableById('path').Value), CreateStringListToMacro(' OpenFileFinderForm(''' + Self.Params.VariableById('path').Value + '''); ')) then
      vrModule.CreateRouteIfDontExists(Self.Caption, Self.Internal_GetRouteName, vrWizard.GetLastID('MACROS'), ICON_SEARCH, 1000);
  finally
    FreeAndNil(vrModule);
    FreeAndNil(vrWizard);
  end;
end;

function TFFileFinder.Internal_GetRouteName: String;
begin
  Result := vrJupiterApp.Params.VariableById('Menus.Work.Route').Value + '/search_' + FormatDateTime('ddmmyyyy_hhnnss', Now);
end;

function TFFileFinder.Internal_GetMacroName: String;
begin
  Result := JupiterStringUtilsScript_Replace(Copy(Self.Internal_GetRouteName, 2), '/', '.');
end;

procedure TFFileFinder.ReadDirectory(prPath: String; prOwner: TTreeNode);
begin
  Self.Internal_ReadDirectory(prPath, prOwner);
end;

end.

