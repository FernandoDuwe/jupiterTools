unit uFileFinder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, uJupiterForm,
  jupiterStringUtils, JupiterDirectoryDataProvider, JupiterFileDataProvider,
  JupiterConsts, JupiterApp, uJupiterStringUtilsScript, uJupiterRunnableScript,
  uMain, uJupiterAction, uJupiterDesktopAppScript, LCLType;

type

  { TFFileFinder }

  TFFileFinder = class(TFJupiterForm)
    tvFileTree: TTreeView;
    procedure edSearchKeyPress(Sender: TObject; var Key: char);
    procedure tvFileTreeDblClick(Sender: TObject);
    procedure tvFileTreeKeyPress(Sender: TObject; var Key: char);
  private
    FSearch : String;

    procedure Internal_OnDelete(Sender: TObject);

    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;

    procedure Internal_PrepareForm; override;
    procedure Internal_ReadDirectory(prPath : String; prOwner : TTreeNode);
  public

  end;

var
  FFileFinder: TFFileFinder;

implementation

{$R *.lfm}

{ TFFileFinder }

procedure TFFileFinder.edSearchKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Self.UpdateForm();

    Key := #0;
  end;
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

  if Self.ActionGroup.Count > 0 then
    if tvFileTree.Items.Count > 0 then
      Self.ActionGroup.GetActionAtIndex(0).Enable
    else
      Self.ActionGroup.GetActionAtIndex(0).Disable;
end;

procedure TFFileFinder.Internal_UpdateDatasets;
begin
  inherited Internal_UpdateDatasets;

  tvFileTree.Items.Clear;
  tvFileTree.SortType := stNone;

  if not vrJupiterApp.Params.VariableById('Interface.Finder.AlwaysSearchEmptyQuery').AsBool then
    if Trim(edSearch.Text) = EmptyStr then
      Exit;

  if Trim(edSearch.Text) <> EmptyStr then
    if edSearch.Text = Self.FSearch then
      Exit;

  JupiterAppDesktopCursorToWait;
  try
    Self.Internal_ReadDirectory(Self.Params.VariableById('path').Value, nil);
  finally
    tvFileTree.SortType := stText;

    if Trim(edSearch.Text) <> EmptyStr then
      tvFileTree.FullExpand;

    Self.FSearch := edSearch.Text;

    JupiterAppDesktopCursorToIdle;
  end;
end;

procedure TFFileFinder.Internal_PrepareForm;
begin
  Self.FSearch := EmptyStr;

  inherited Internal_PrepareForm;

  Self.ShowSearchBar := True;

  Self.Hint := Self.Params.VariableById('path').Value;

  tvFileTree.Images := FMain.ilIconFamily;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Excluir', 'Clique aqui para abrir a pasta atual externamente', ICON_DELETE, @Internal_OnDelete));
end;

procedure TFFileFinder.Internal_ReadDirectory(prPath: String; prOwner: TTreeNode);
var
  vrDirectoryProvider : TJupiterDirectoryDataProvider;
  vrFileProvider : TJupiterFileDataProvider;
  vrVez : Integer;
  vrNode : TTreeNode;
  vrNodeFile : TTreeNode;
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

      Self.Internal_ReadDirectory(vrDirectoryProvider.GetRowByIndex(vrVez).Fields.VariableById('Path').Value, vrNode);

      if Trim(edSearch.Text) <> EmptyStr then
        if vrNode.Count = 0 then
          tvFileTree.Items.Delete(vrNode);
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

end.

