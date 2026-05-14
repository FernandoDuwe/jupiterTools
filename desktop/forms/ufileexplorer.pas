unit uFileExplorer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ShellCtrls, ExtCtrls,
  ActnList, FileCtrl, EditBtn, Calendar, Arrow, uJupiterForm, JupiterConsts,
  jupiterformutils, JupiterEnviroment, jupiterStringUtils, JupiterModule,
  jupiterDatabaseWizard, JupiterApp, uJupiterRunnableScript,
  uJupiterStringUtilsScript, uJupiterEnviromentScript, uJupiterAction,
  uJupiterDesktopAppScript, ComCtrls, StdCtrls;

type

  { TFFileExplorer }

  TFFileExplorer = class(TFJupiterForm)
    acCopy: TAction;
    cbView: TComboBox;
    dePath: TDirectoryEdit;
    pnBody: TPanel;
    pnLeft: TPanel;
    slvExporer: TShellListView;
    spDivider: TSplitter;
    stvFolders: TShellTreeView;
    procedure acCopyExecute(Sender: TObject);
    procedure cbViewChange(Sender: TObject);
    procedure dePathChange(Sender: TObject);
    procedure edSearchKeyPress(Sender: TObject; var Key: char);
    procedure slvExporerAddItem(Sender: TObject; const ABasePath: String; const AFileInfo: TSearchRec; var CanAdd: Boolean);
    procedure slvExporerDblClick(Sender: TObject);
    procedure slvExporerSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure spDividerMoved(Sender: TObject);
    procedure stvFoldersChange(Sender: TObject; Node: TTreeNode);
  private
    procedure Internal_UpdateComponents; override;

    procedure Internal_PrepareForm; override;

    procedure Internal_OnOpenFolder(Sender: TObject);
    procedure Internal_OnAsReport(Sender: TObject);
    procedure Internal_OnAsList(Sender: TObject);
    procedure Internal_OnAsIcons(Sender: TObject);
    procedure Internal_OnAsSmallIcons(Sender: TObject);
    procedure Internal_OnSearchInFiles(Sender: TObject);
    procedure Internal_OnSearchContentInFiles(Sender: TObject);

    function Internal_EnableWorkMenu : Boolean; override;
    procedure Internal_AddToWorkMenu; override;

    function Internal_GetRouteName : String;
    function Internal_GetMacroName : String;

    procedure Internal_SetPath(prPath : String);
  public

  end;

var
  FFileExplorer: TFFileExplorer;

implementation

uses Clipbrd;

{$R *.lfm}

{ TFFileExplorer }

procedure TFFileExplorer.slvExporerDblClick(Sender: TObject);
begin
  if not Assigned(slvExporer.Selected) then
    Exit;

  if JupiterEnviromentScript_FolderExists(slvExporer.Root + slvExporer.Selected.Caption) then
  begin
    Self.Internal_SetPath(slvExporer.Root + slvExporer.Selected.Caption);

    Exit;
  end;

  JupiterRunnableScript_RunCommandOnJupiter(slvExporer.Root + slvExporer.Selected.Caption);
end;

procedure TFFileExplorer.slvExporerSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if not Assigned(slvExporer.Selected) then
    Exit;

  if Assigned(Item) then
    Self.Params.VariableById('currentFile').Value := slvExporer.Root + slvExporer.Selected.Caption
end;

procedure TFFileExplorer.acCopyExecute(Sender: TObject);
begin
  if stvFolders.Focused then
    Clipboard.AsText := stvFolders.Path;

  if slvExporer.Focused then
    if Assigned(slvExporer.Selected) then
      Clipboard.AsText := slvExporer.Selected.Caption;
end;

procedure TFFileExplorer.cbViewChange(Sender: TObject);
begin
  if cbView.ItemIndex = -1 then
    Exit;

  if cbView.ItemIndex = 0 then
    slvExporer.ViewStyle := vsIcon;

  if cbView.ItemIndex = 1 then
    slvExporer.ViewStyle := vsList;

  if cbView.ItemIndex = 2 then
    slvExporer.ViewStyle := vsReport;

  if cbView.ItemIndex = 3 then
    slvExporer.ViewStyle := vsSmallIcon;
end;

procedure TFFileExplorer.dePathChange(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    if vrEnviroment.Exists(dePath.Text) then
      Self.Internal_SetPath(dePath.Text);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFFileExplorer.edSearchKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    slvExporer.UpdateView;

    Self.UpdateForm();

    Key := #0;
  end;
end;

procedure TFFileExplorer.slvExporerAddItem(Sender: TObject; const ABasePath: String; const AFileInfo: TSearchRec; var CanAdd: Boolean);
begin
  CanAdd := Trim(edSearch.Text) = EmptyStr;

  if not CanAdd then
    CanAdd := Pos(AnsiUpperCase(edSearch.Text), AnsiUpperCase(AFileInfo.Name)) > 0;
end;

procedure TFFileExplorer.spDividerMoved(Sender: TObject);
begin
  miLookColumn.Checked := False;
end;

procedure TFFileExplorer.stvFoldersChange(Sender: TObject; Node: TTreeNode);
begin
  Self.Params.VariableById('currentPath').Value := stvFolders.Path;
end;

procedure TFFileExplorer.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if miLookColumn.Checked then
    pnLeft.Width := PercentOfScreen(Self.Width, Self.PercentDivisor);

  Self.Caption := 'Pasta: ' + jupiterStringUtilsGetLastPathName(slvExporer.Root);

  slvExporer.Update;
  slvExporer.UpdateView;
  slvExporer.Refresh;
  slvExporer.Repaint;
end;

procedure TFFileExplorer.Internal_PrepareForm;
begin
  Self.ShowSearchBar := True;

  cbView.ItemIndex := 2;

  if not Self.Params.Exists('currentFile') then
    Self.Params.AddVariable('currentFile', EmptyStr);

  if not Self.Params.Exists('currentPath') then
    Self.Params.AddVariable('currentPath', EmptyStr);

  inherited Internal_PrepareForm;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Abrir pasta', 'Clique aqui para abrir a pasta atual externamente', ICON_OPEN, @Internal_OnOpenFolder));

//  Self.ActionGroup.AddAction(TJupiterAction.Create('Relatório', 'Exibir itens como relatório', NULL_KEY, @Internal_OnAsReport));

//  Self.ActionGroup.AddAction(TJupiterAction.Create('Lista', 'Exibir itens como lista', NULL_KEY, @Internal_OnAsList));

//  Self.ActionGroup.AddAction(TJupiterAction.Create('Ícones', 'Exibir itens como ícones', NULL_KEY, @Internal_OnAsIcons));

//  Self.ActionGroup.AddAction(TJupiterAction.Create('Ícones pequenos', 'Exibir itens como ícones pequenos', NULL_KEY, @Internal_OnAsSmallIcons));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Pesquisar arquivos', 'Pesquisar arquivos no diretório atual', ICON_SEARCH, @Internal_OnSearchInFiles));

//  Self.ActionGroup.AddAction(TJupiterAction.Create('Pesquisar em arquivos', 'Pesquisar conteúdo nos arquivos do diretório atual', ICON_TASKS, @Internal_OnSearchContentInFiles));

  if Self.Params.Exists('path') then
    Self.Internal_SetPath(Self.Params.VariableById('path').Value)
  else
    Self.Internal_SetPath(EmptyStr);
end;

procedure TFFileExplorer.Internal_OnOpenFolder(Sender: TObject);
begin
  JupiterRunnableScript_RunCommandOnJupiter(slvExporer.Root);
end;

procedure TFFileExplorer.Internal_OnAsReport(Sender: TObject);
begin
  slvExporer.ViewStyle := vsReport;
end;

procedure TFFileExplorer.Internal_OnAsList(Sender: TObject);
begin
  slvExporer.ViewStyle := vsList;
end;

procedure TFFileExplorer.Internal_OnAsIcons(Sender: TObject);
begin
  slvExporer.ViewStyle := vsIcon;
end;

procedure TFFileExplorer.Internal_OnAsSmallIcons(Sender: TObject);
begin
  slvExporer.ViewStyle := vsSmallIcon;
end;

procedure TFFileExplorer.Internal_OnSearchInFiles(Sender: TObject);
begin
  JupiterAppDesktopOpenFileFinderForm(slvExporer.Root);
end;

procedure TFFileExplorer.Internal_OnSearchContentInFiles(Sender: TObject);
begin
  JupiterAppDesktopOpenFileReaderFinderForm(slvExporer.Root);
end;

function TFFileExplorer.Internal_EnableWorkMenu: Boolean;
begin
  Result := True;
end;

procedure TFFileExplorer.Internal_AddToWorkMenu;
var
  vrModule : TJupiterModule;
  vrWizard : TJupiterDatabaseWizard;
begin
  inherited Internal_AddToWorkMenu;

  vrWizard := vrJupiterApp.NewWizard;
  vrModule := TJupiterModule.Create;
  try
    if vrModule.CreateMacroIfDontExists(Self.Internal_GetMacroName, 'Clique do item de menu ' + ExtractFileName(Self.Params.VariableById('path').Value), CreateStringListToMacro(' OpenFileExplorerForm(''' + Self.Params.VariableById('path').Value + '''); ')) then
      vrModule.CreateRouteIfDontExists(Self.Caption, Self.Internal_GetRouteName, vrWizard.GetLastID('MACROS'), ICON_OPEN, 1000);
  finally
    FreeAndNil(vrModule);
    FreeAndNil(vrWizard);
  end;
end;

function TFFileExplorer.Internal_GetRouteName: String;
begin
  Result := vrJupiterApp.Params.VariableById('Menus.Work.Route').Value + ExtractFileName(Self.Params.VariableById('path').Value) + '/' + FormatDateTime('ddmmyyyy_hhnnss', Now);
end;

function TFFileExplorer.Internal_GetMacroName: String;
begin
  Result := JupiterStringUtilsScript_Replace(Copy(Self.Internal_GetRouteName, 2), '/', '.');
end;

procedure TFFileExplorer.Internal_SetPath(prPath: String);
var
  vrEnviroment : TJupiterEnviroment;
begin
  if Trim(prPath) <> EmptyStr then
    if prPath[Length(prPath)] <> GetDirectorySeparator then
      prPath := prPath + GetDirectorySeparator;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    if not Self.Params.Exists('path') then
      Self.Params.AddVariable('path', vrEnviroment.BasePath, 'Endereço')
    else
      Self.Params.VariableById('path').Value := prPath;

    if not Self.Params.Exists('currentFile') then
      Self.Params.AddVariable('currentFile', EmptyStr);

    if not Self.Params.Exists('currentPath') then
      Self.Params.AddVariable('currentPath', Self.Params.VariableById('path').Value)
    else
      Self.Params.VariableById('currentPath').Value := Self.Params.VariableById('path').Value;

    slvExporer.Root := Self.Params.VariableById('path').Value;
    stvFolders.Root := Self.Params.VariableById('path').Value;

    dePath.RootDir := slvExporer.Root;
    dePath.Text := dePath.RootDir;
  finally
    FreeAndNil(vrEnviroment);

    Self.Hint := Self.Params.VariableById('path').Value;
  end;
end;

end.

