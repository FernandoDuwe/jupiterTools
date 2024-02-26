unit uGenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Buttons, uCustomJupiterForm, uNewAction, uNewField, uMain,
  uNewParam, JupiterFileDataProvider, JupiterEnviroment, JupiterConsts,
  JupiterXMLDataProvider, JupiterGeneratorForm, JupiterAction, JupiterRunnable,
  JupiterVariableForm, JupiterApp, JupiterRoute, jupiterformutils,
  JupiterObject, JupiterDialogForm, JupiterGeneratorMenuItem, JupiterVariable,
  JupiterCSVDataProvider, LCLType;

type

  { TFGenerator }

  TFGenerator = class(TFCustomJupiterForm)
    edFile: TEdit;
    edMenuItemLocation: TEdit;
    edMenuItemRoute: TEdit;
    edMenuItemTitle: TEdit;
    imgIconBig: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lbIconHint: TLabel;
    lbIcon: TLabel;
    lbFormList: TListBox;
    lbIconHint1: TLabel;
    lbIconHint2: TLabel;
    lbMenuList: TListBox;
    lbReleaseNotes: TListBox;
    lvDatasets: TListView;
    lvMainMenus: TListView;
    lvIcons: TListView;
    lvActions: TListView;
    lvFields: TListView;
    lvMenuVariables: TListView;
    mmCurrentMenuInfo: TMemo;
    mmIcon: TMemo;
    mmLines: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    pnMenuVariable: TPanel;
    pnMenuForm: TPanel;
    pnBodyMenuItem: TPanel;
    pnForm: TPanel;
    pcTabs: TPageControl;
    sbActionDelete: TSpeedButton;
    sbMenuParamAdd: TSpeedButton;
    sbFieldDelete: TSpeedButton;
    sbActionAdd: TSpeedButton;
    sbFieldAdd: TSpeedButton;
    sbMenuParamDelete: TSpeedButton;
    spForms: TSplitter;
    spForms1: TSplitter;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    tsMainMenu: TTabSheet;
    tsDatasets: TTabSheet;
    tsIcon: TTabSheet;
    tvCurrentMenu: TTreeView;
    tsForms: TTabSheet;
    tsInicio: TTabSheet;
    procedure edMenuItemLocationChange(Sender: TObject);
    procedure edMenuItemRouteChange(Sender: TObject);
    procedure edMenuItemTitleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbFormListClick(Sender: TObject);
    procedure lbMenuListClick(Sender: TObject);
    procedure lvActionsDblClick(Sender: TObject);
    procedure lvActionsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvFieldsDblClick(Sender: TObject);
    procedure lvFieldsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvIconsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvIconsClick(Sender: TObject);
    procedure lvMenuVariablesDblClick(Sender: TObject);
    procedure sbActionAddClick(Sender: TObject);
    procedure sbActionDeleteClick(Sender: TObject);
    procedure sbFieldAddClick(Sender: TObject);
    procedure sbFieldDeleteClick(Sender: TObject);
    procedure sbMenuParamAddClick(Sender: TObject);
    procedure sbMenuParamDeleteClick(Sender: TObject);
    procedure tvCurrentMenuClick(Sender: TObject);
  private
    FFormID : String;
    FMenuFile : String;

    procedure Internal_ShowForm(prFile : String);
    procedure Internal_ShowMenuItem(prFile : String);
    procedure Internal_SetMenuItem(prFile : String);
    procedure Internal_EditAction(prActionIndex : Integer; prAction : TJupiterAction);
    procedure Internal_DeleteAction(prActionIndex : Integer);
    procedure Internal_DeleteParam(prParamIndex : Integer);
    procedure Internal_EditField(prFieldIndex : Integer; prField : TJupiterVariableForm);
    procedure Internal_EditParam(prParamIndex : Integer; prParam : TJupiterVariable);
    procedure Internal_DeleteField(prFieldIndex : Integer);
    procedure Internal_PrepareForm; override;
    procedure Internal_RefreshClick(Sender: TObject);
    procedure Internal_NewMenuItemClick(Sender: TObject);

    procedure Internal_UpdateFormForm;
  protected
    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_Resize; override;
  published
    property FormID : String read FFormID write FFormID;
  public

  end;

var
  FGenerator: TFGenerator;

implementation


{$R *.lfm}

{ TFGenerator }

procedure TFGenerator.lbFormListClick(Sender: TObject);
begin
  if lbFormList.ItemIndex = NULL_KEY then
    Exit;

  Self.Internal_ShowForm(lbFormList.Items[lbFormList.ItemIndex]);
end;

procedure TFGenerator.lbMenuListClick(Sender: TObject);
begin
  if lbMenuList.ItemIndex = NULL_KEY then
    Exit;

  Self.Internal_ShowMenuItem(lbMenuList.Items[lbMenuList.ItemIndex]);
end;

procedure TFGenerator.lvActionsDblClick(Sender: TObject);
begin
  if not Assigned(lvActions.Selected) then
    Exit;

  if not Assigned(lvActions.Selected.Data) then
    Exit;

  Application.CreateForm(TFNewAction, FNewAction);
  try
    FNewAction.ActionIndex := TJupiterAction(lvActions.Selected.Data).Tag;
    FNewAction.Action := TJupiterAction(lvActions.Selected.Data);

    if FNewAction.ShowModal = mrOK then
      Self.Internal_EditAction(FNewAction.ActionIndex, FNewAction.Action);
  finally
    FreeAndNil(FNewAction);
  end;
end;

procedure TFGenerator.lvActionsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  Self.Internal_UpdateFormForm;
end;

procedure TFGenerator.lvFieldsDblClick(Sender: TObject);
begin
  if not Assigned(lvFields.Selected) then
    Exit;

  if not Assigned(lvFields.Selected.Data) then
    Exit;

  Application.CreateForm(TFNewField, FNewField);
  try
    FNewField.FieldIndex := TJupiterVariableForm(lvFields.Selected.Data).Tag;
    FNewField.Field      := TJupiterVariableForm(lvFields.Selected.Data);

    if FNewField.ShowModal = mrOK then
      Self.Internal_EditField(FNewField.FieldIndex, FNewField.Field);
  finally
    FreeAndNil(FNewField);
  end;
end;

procedure TFGenerator.lvFieldsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  Self.Internal_UpdateFormForm;
end;

procedure TFGenerator.lvIconsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin

end;

procedure TFGenerator.lvIconsClick(Sender: TObject);
var
  vrGraphic : TGraphic;
begin
  if not Assigned(lvIcons.Selected) then
    Exit;

  imgIconBig.Picture := nil;

  FMain.ilIconFamily.Draw(imgIconBig.Canvas, 0, 0, StrToInt(lvIcons.Selected.Caption));

  imgIconBig.Stretch := False;
  imgIconBig.Center := False;

  lbIcon.Caption := 'Ícone: ' + lvIcons.Selected.Caption;

  imgIconBig.Stretch := True;
  imgIconBig.Center := True;

  mmIcon.Lines.Clear;
  mmIcon.Lines.Add('ID: itemIcon');
  mmIcon.Lines.Add('Desc: Ícone');
  mmIcon.Lines.Add('Valor: ' + lvIcons.Selected.Caption);
end;

procedure TFGenerator.lvMenuVariablesDblClick(Sender: TObject);
begin
  if not Assigned(lvMenuVariables.Selected) then
    Exit;

  if not Assigned(lvMenuVariables.Selected.Data) then
    Exit;

  Application.CreateForm(TFNewParam, FNewParam);
  try
    FNewParam.ParamIndex := TJupiterVariable(lvMenuVariables.Selected.Data).Tag;
    FNewParam.Param := TJupiterVariable(lvMenuVariables.Selected.Data);

    if FNewParam.ShowModal = mrOK then
      Self.Internal_EditParam(FNewParam.ParamIndex, FNewParam.Param);
  finally
    FreeAndNil(FNewParam);
  end;
end;

procedure TFGenerator.sbActionAddClick(Sender: TObject);
begin
  Application.CreateForm(TFNewAction, FNewAction);
  try
    FNewAction.ActionIndex := NULL_KEY;

    if FNewAction.ShowModal = mrOK then
      Self.Internal_EditAction(FNewAction.ActionIndex, FNewAction.Action);
  finally
    FreeAndNil(FNewAction);
  end;
end;

procedure TFGenerator.sbActionDeleteClick(Sender: TObject);
begin
  if not Assigned(lvActions.Selected) then
    Exit;

  if not Assigned(lvActions.Selected.Data) then
    Exit;

  if Application.MessageBox('Deseja realmente excluir', 'Excluir Ação', MB_ICONQUESTION + MB_YESNO) = ID_YES then
    Self.Internal_DeleteAction(TJupiterAction(lvActions.Selected.Data).Tag);
end;

procedure TFGenerator.sbFieldAddClick(Sender: TObject);
begin
  Application.CreateForm(TFNewField, FNewField);
  try
    FNewField.FieldIndex := NULL_KEY;

    if FNewField.ShowModal = mrOK then
      Self.Internal_EditField(FNewField.FieldIndex, FNewField.Field);
  finally
    FreeAndNil(FNewField);
  end;
end;

procedure TFGenerator.sbFieldDeleteClick(Sender: TObject);
begin
  if not Assigned(lvFields.Selected) then
    Exit;

  if not Assigned(lvFields.Selected.Data) then
    Exit;

  if Application.MessageBox('Deseja realmente excluir', 'Excluir Campo', MB_ICONQUESTION + MB_YESNO) = ID_YES then
    Self.Internal_DeleteField(TJupiterVariableForm(lvFields.Selected.Data).Tag);
end;

procedure TFGenerator.sbMenuParamAddClick(Sender: TObject);
begin
  Application.CreateForm(TFNewParam, FNewParam);
  try
    FNewParam.ParamIndex := NULL_KEY;

    if FNewParam.ShowModal = mrOK then
      Self.Internal_EditParam(FNewParam.ParamIndex, FNewParam.Param);
  finally
    FreeAndNil(FNewParam);
  end;
end;

procedure TFGenerator.sbMenuParamDeleteClick(Sender: TObject);
begin
  if not Assigned(lvMenuVariables.Selected) then
    Exit;

  if not Assigned(lvMenuVariables.Selected.Data) then
    Exit;

  if Application.MessageBox('Deseja realmente excluir', 'Excluir Parâmetro', MB_ICONQUESTION + MB_YESNO) = ID_YES then
    Self.Internal_DeleteParam(TJupiterVariable(lvMenuVariables.Selected.Data).Tag);
end;

procedure TFGenerator.tvCurrentMenuClick(Sender: TObject);
var
  vrVez : Integer;
begin
  if not Assigned(tvCurrentMenu.Selected) then
    Exit;

  mmCurrentMenuInfo.Lines.Clear;
  mmCurrentMenuInfo.Lines.Add(Format('Selecionado: %0:s', [tvCurrentMenu.Selected.Text]));

  if not Assigned(tvCurrentMenu.Selected.Data) then
    Exit;

  with TJupiterAction(tvCurrentMenu.Selected.Data) do
  begin
    mmCurrentMenuInfo.Lines.Add(EmptyStr);
    mmCurrentMenuInfo.Lines.Add('Título:');
    mmCurrentMenuInfo.Lines.Add(Title);
    mmCurrentMenuInfo.Lines.Add(EmptyStr);
    mmCurrentMenuInfo.Lines.Add('Rota de Destino (Para onde o menu apontará):');
    mmCurrentMenuInfo.Lines.Add(Route.Path);
    mmCurrentMenuInfo.Lines.Add(EmptyStr);
    mmCurrentMenuInfo.Lines.Add('Rota de Origem (Onde o menu estará localizado):');
    mmCurrentMenuInfo.Lines.Add(Location.Path);
    mmCurrentMenuInfo.Lines.Add(EmptyStr);
    mmCurrentMenuInfo.Lines.Add('Parâmetros:');

    for vrVez := 0 to Route.Params.Size - 1 do
    begin
      mmCurrentMenuInfo.Lines.Add(EmptyStr);
      mmCurrentMenuInfo.Lines.Add('  - Parâmetro ' + IntToStr(vrVez + 1) + ':');
      mmCurrentMenuInfo.Lines.Add('    ID: ' + Route.Params.VariableByIndex(vrVez).ID);
      mmCurrentMenuInfo.Lines.Add('    Valor: ' + Route.Params.VariableByIndex(vrVez).Value);
      mmCurrentMenuInfo.Lines.Add('    Derscrição: ' + Route.Params.VariableByIndex(vrVez).Title);
    end;
  end;
end;

procedure TFGenerator.FormCreate(Sender: TObject);
var
  vrVez  : Integer;
  vrItem : TListItem;
begin
  inherited;

  Self.FFormID   := EmptyStr;
  Self.FMenuFile := EmptyStr;

  lvIcons.Items.Clear;
  lvIcons.LargeImages := FMain.ilIconFamily;
  lvIcons.SmallImages := FMain.ilIconFamily;
//  lvIcons.StateImages := FMain.ilIconFamily;

  for vrVez := 0 to FMain.ilIconFamily.Count - 1 do
  begin
    vrItem := lvIcons.Items.Add;
    vrItem.Caption := IntToStr(vrVez);
    vrItem.ImageIndex := vrVez;
  end;
end;

procedure TFGenerator.edMenuItemTitleChange(Sender: TObject);
begin
  if Self.FMenuFile = EmptyStr then
    Exit;

  if edMenuItemTitle.Text = EmptyStr then
  begin
    ShowMessage('O campo Título é obrigatório');

    edMenuItemTitle.SetFocus;
    Exit;
  end;

  Self.Internal_SetMenuItem(Self.FMenuFile);
end;

procedure TFGenerator.edMenuItemRouteChange(Sender: TObject);
begin
  if Self.FMenuFile = EmptyStr then
    Exit;

  if edMenuItemRoute.Text = EmptyStr then
  begin
    ShowMessage('O campo Rota de Destino é obrigatório');

    edMenuItemRoute.SetFocus;
    Exit;
  end;

  Self.Internal_SetMenuItem(Self.FMenuFile);
end;

procedure TFGenerator.edMenuItemLocationChange(Sender: TObject);
begin
  if Self.FMenuFile = EmptyStr then
    Exit;

  if edMenuItemLocation.Text = EmptyStr then
  begin
    ShowMessage('O campo Rota de Origem é obrigatório');

    edMenuItemLocation.SetFocus;
    Exit;
  end;

  Self.Internal_SetMenuItem(Self.FMenuFile);
end;

procedure TFGenerator.FormShow(Sender: TObject);
var
  vrVez : Integer;
  vrMenuList : TJupiterObjectList;
begin
  inherited;

  if Self.FormID <> EmptyStr then
  begin
    pcTabs.ActivePageIndex := 1;

    lbFormList.ItemIndex := lbFormList.Items.IndexOf(Self.FormID + '.xml');

    if lbFormList.ItemIndex <> NULL_KEY then
      lbFormListClick(Sender);
  end;

  tvCurrentMenu.Images := FMain.ilIconFamily;

  vrMenuList := vrJupiterApp.GetActions(TJupiterRoute.Create(ROOT_PATH));

  ShowRouteOnTreeView(tvCurrentMenu, TJupiterRoute.Create(ROOT_PATH), vrMenuList, nil);

  lbFormList.Width    := PercentOfScreen(Self.Width, 30);
  lbMenuList.Width    := PercentOfScreen(Self.Width, 30);
  tvCurrentMenu.Width := PercentOfScreen(Self.Width, 30);

  lvIcons.Selected := lvIcons.Items[0];
  lvIconsClick(Sender);
end;

procedure TFGenerator.Internal_ShowForm(prFile: String);
var
  vrEnviroment : TJupiterEnviroment;
  vrGenerator  : TJupiterGeneratorForm;
  vrVez        : Integer;
  vrRow        : TListItem;
begin
  lvActions.Items.Clear;
  lvFields.Items.Clear;

  vrEnviroment := TJupiterEnviroment.Create;
  vrGenerator  := TJupiterGeneratorForm.Create;
  try
    edFile.Text := vrEnviroment.FullPath('modules/generator/' + prFile);

    vrGenerator.FormID := StringReplace(prFile, '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]);

    for vrVez := 0 to vrGenerator.Actions.Size - 1 do
    begin
      vrRow := lvActions.Items.Add;
      vrRow.Caption := TJupiterAction(vrGenerator.Actions.GetAtIndex(vrVez)).Title;

      if TJupiterAction(vrGenerator.Actions.GetAtIndex(vrVez)).ActionType = 0 then
        vrRow.SubItems.Add(TJupiterAction(vrGenerator.Actions.GetAtIndex(vrVez)).Runnable.CommandLine);

      if TJupiterAction(vrGenerator.Actions.GetAtIndex(vrVez)).ActionType = 1 then
        vrRow.SubItems.Add(TJupiterAction(vrGenerator.Actions.GetAtIndex(vrVez)).OnClickScript.Text);


      vrRow.Data := TJupiterAction(vrGenerator.Actions.GetAtIndex(vrVez));
    end;

    for vrVez := 0 to vrGenerator.Fields.Size - 1 do
    begin
      vrRow := lvFields.Items.Add;
      vrRow.Caption := vrGenerator.Fields.VariableByIndex(vrVez).ID;
      vrRow.SubItems.Add(vrGenerator.Fields.VariableByIndex(vrVez).Value);
      vrRow.SubItems.Add(vrGenerator.Fields.VariableByIndex(vrVez).Title);
      vrRow.Data := vrGenerator.Fields.VariableByIndex(vrVez);
    end;

    Self.Internal_UpdateFormForm;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFGenerator.Internal_ShowMenuItem(prFile: String);
var
  vrMenuItem   : TJupiterGeneratorMenuItem;
  vrEnviroment : TJupiterEnviroment;
  vrVez        : Integer;
  vrRow        : TListItem;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  vrMenuItem   := TJupiterGeneratorMenuItem.Create;
  try
    Self.FMenuFile := EmptyStr;

    vrMenuItem.FileName := vrEnviroment.FullPath('modules/generator/menus/' + prFile);

    edMenuItemTitle.Caption    := vrMenuItem.Title;
    edMenuItemLocation.Caption := vrMenuItem.LocationPath;
    edMenuItemRoute.Caption    := vrMenuItem.RoutePath;

    Self.FMenuFile := vrMenuItem.FileName;

    lvMenuVariables.Items.Clear;

    for vrVez := 0 to vrMenuItem.Params.Size - 1 do
    begin
      vrRow         := lvMenuVariables.Items.Add;
      vrRow.Caption := vrMenuItem.Params.VariableByIndex(vrVez).ID;
      vrRow.SubItems.Add(vrMenuItem.Params.VariableByIndex(vrVez).Value);
      vrRow.SubItems.Add(vrMenuItem.Params.VariableByIndex(vrVez).Title);

      vrRow.Data := TJupiterVariable.Create;

      with TJupiterVariable(vrRow.Data) do
      begin
        ID    := vrMenuItem.Params.VariableByIndex(vrVez).ID;
        Value := vrMenuItem.Params.VariableByIndex(vrVez).Value;
        Title := vrMenuItem.Params.VariableByIndex(vrVez).Title;
        Tag   := vrMenuItem.Params.VariableByIndex(vrVez).Tag;
      end;
    end;

    Self.UpdateForm;
    Self.Internal_UpdateFormForm;
  finally
    FreeAndNil(vrMenuItem);
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFGenerator.Internal_SetMenuItem(prFile: String);
var
  vrMenuItem : TJupiterGeneratorMenuItem;
begin
  vrMenuItem := TJupiterGeneratorMenuItem.Create;
  try
    vrMenuItem.FileName := prFile;

    vrMenuItem.Title        := edMenuItemTitle.Caption;
    vrMenuItem.LocationPath := edMenuItemLocation.Caption;
    vrMenuItem.RoutePath    := edMenuItemRoute.Caption;
    vrMenuItem.SaveFile;
  finally
    FreeAndNil(vrMenuItem);
  end;
end;

procedure TFGenerator.Internal_EditAction(prActionIndex: Integer; prAction: TJupiterAction);
var
  vrGenerator : TJupiterGeneratorForm;
begin
  vrGenerator  := TJupiterGeneratorForm.Create;
  try
    vrGenerator.FormID := StringReplace(ExtractFileName(edFile.Text), '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]);

    if prActionIndex = NULL_KEY then
    begin
      vrGenerator.Actions.Add(TJupiterAction.Create(prAction.Title, TJupiterRunnable.Create(prAction.Runnable.CommandLine)));

      with TJupiterAction(vrGenerator.Actions.GetLastObject) do
      begin
        Hint := prAction.Hint;
        Icon := prAction.Icon;
        ActionType := prAction.ActionType;
        OnClickScript.AddStrings(prAction.OnClickScript);

        ConfirmBeforeExecute := prAction.ConfirmBeforeExecute;
      end;
    end
    else
    begin
      with TJupiterAction(vrGenerator.Actions.GetAtIndex(prActionIndex)) do
      begin
        Title := prAction.Title;
        Hint  := prAction.Hint;
        Icon  := prAction.Icon;
        ConfirmBeforeExecute := prAction.ConfirmBeforeExecute;

        Runnable.CommandLine := prAction.Runnable.CommandLine;

        ActionType := prAction.ActionType;
        OnClickScript.AddStrings(prAction.OnClickScript);
      end;
    end;

    vrGenerator.SaveFile;

    Self.Internal_ShowForm(StringReplace(ExtractFileName(edFile.Text), '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]));
  finally
    FreeAndNil(vrGenerator);
  end;
end;

procedure TFGenerator.Internal_DeleteAction(prActionIndex: Integer);
var
  vrGenerator : TJupiterGeneratorForm;
begin
  vrGenerator  := TJupiterGeneratorForm.Create;
  try
    vrGenerator.FormID := StringReplace(ExtractFileName(edFile.Text), '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
    vrGenerator.Actions.DeleteAtIndex(prActionIndex);
    vrGenerator.SaveFile;

    Self.Internal_ShowForm(StringReplace(ExtractFileName(edFile.Text), '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]));
  finally
    FreeAndNil(vrGenerator);
  end;
end;

procedure TFGenerator.Internal_DeleteParam(prParamIndex: Integer);
var
  vrMenuItem : TJupiterGeneratorMenuItem;
begin
  vrMenuItem := TJupiterGeneratorMenuItem.Create;
  try
    vrMenuItem.FileName := Self.FMenuFile;

    vrMenuItem.Params.DeleteAtIndex(prParamIndex);

    vrMenuItem.SaveFile;
  finally
    FreeAndNil(vrMenuItem);

    Self.Internal_ShowMenuItem(ExtractFileName(Self.FMenuFile));
  end;
end;

procedure TFGenerator.Internal_EditField(prFieldIndex: Integer; prField: TJupiterVariableForm);
var
  vrGenerator : TJupiterGeneratorForm;
begin
  vrGenerator  := TJupiterGeneratorForm.Create;
  try
    vrGenerator.FormID := StringReplace(ExtractFileName(edFile.Text), '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]);

    if prFieldIndex = NULL_KEY then
    begin
      vrGenerator.Fields.Add(TJupiterVariableForm.Create);

      with TJupiterVariableForm(vrGenerator.Fields.GetLastObject) do
      begin
        ID    := prField.ID;
        Title := prField.Title;
        Value := prField.Value;

        CleanOnShow   := prField.CleanOnShow;
        Required      := prField.Required;
        ReadOnly      := prField.ReadOnly;
        CopyButton    := prField.CopyButton;
        RunButton     := prField.RunButton;
        ComponentType := prField.ComponentType;
        ListVariable  := prField.ListVariable;
      end;
    end
    else
    begin
      with TJupiterVariableForm(vrGenerator.Fields.GetAtIndex(prFieldIndex)) do
      begin
        ID    := prField.ID;
        Title := prField.Title;
        Value := prField.Value;

        CleanOnShow   := prField.CleanOnShow;
        Required      := prField.Required;
        ReadOnly      := prField.ReadOnly;
        CopyButton    := prField.CopyButton;
        RunButton     := prField.RunButton;
        ComponentType := prField.ComponentType;
        ListVariable  := prField.ListVariable;
      end;
    end;

    vrGenerator.SaveFile;

    Self.Internal_ShowForm(StringReplace(ExtractFileName(edFile.Text), '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]));
  finally
    FreeAndNil(vrGenerator);
  end;
end;

procedure TFGenerator.Internal_EditParam(prParamIndex: Integer; prParam: TJupiterVariable);
var
  vrMenuItem : TJupiterGeneratorMenuItem;
begin
  vrMenuItem := TJupiterGeneratorMenuItem.Create;
  try
    vrMenuItem.FileName := Self.FMenuFile;

    vrMenuItem.Title        := edMenuItemTitle.Caption;
    vrMenuItem.LocationPath := edMenuItemLocation.Caption;
    vrMenuItem.RoutePath    := edMenuItemRoute.Caption;

    if prParamIndex = -1 then
      vrMenuItem.Params.AddConfig(prParam.ID, prParam.Value, prParam.Title)
    else
    begin
      with vrMenuItem.Params.VariableByIndex(prParamIndex) do
      begin
        ID    := prParam.ID;
        Value := prParam.Value;
        Title := prParam.Title;
      end;
    end;

    vrMenuItem.SaveFile;
  finally
    FreeAndNil(vrMenuItem);

    Self.Internal_ShowMenuItem(ExtractFileName(Self.FMenuFile));
  end;
end;

procedure TFGenerator.Internal_DeleteField(prFieldIndex: Integer);
var
  vrGenerator : TJupiterGeneratorForm;
begin
  vrGenerator  := TJupiterGeneratorForm.Create;
  try
    vrGenerator.FormID := StringReplace(ExtractFileName(edFile.Text), '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
    vrGenerator.Fields.DeleteAtIndex(prFieldIndex);
    vrGenerator.SaveFile;

    Self.Internal_ShowForm(StringReplace(ExtractFileName(edFile.Text), '.xml', EmptyStr, [rfIgnoreCase, rfReplaceAll]));
  finally
    FreeAndNil(vrGenerator);
  end;
end;

procedure TFGenerator.Internal_PrepareForm;
var
  vrAction : TJupiterAction;
  vrVez : Integer;
begin
  inherited Internal_PrepareForm;

  vrAction      := TJupiterAction.Create('Atualizar', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para atualizar a página';
  vrAction.Icon := ICON_REFRESH;
  vrAction.OnClick := @Internal_RefreshClick;

  Self.Actions.Add(vrAction);

  vrAction      := TJupiterAction.Create('Novo Menu', TJupiterRunnable.Create(''), nil);
  vrAction.Hint := 'Clique aqui para criar um novo item de menu';
  vrAction.Icon := ICON_NEW;
  vrAction.OnClick := @Internal_NewMenuItemClick;

  Self.Actions.Add(vrAction);

  lvActions.Enabled := False;
  lvFields.Enabled  := False;

  sbActionAdd.Enabled    := False;
  sbActionAdd.Images     := FMain.ilIconFamily;
  sbActionAdd.ImageIndex := ICON_NEW;

  sbActionDelete.Enabled    := False;
  sbActionDelete.Images     := FMain.ilIconFamily;
  sbActionDelete.ImageIndex := ICON_DELETE;

  sbFieldAdd.Enabled    := False;
  sbFieldAdd.Images     := FMain.ilIconFamily;
  sbFieldAdd.ImageIndex := ICON_NEW;

  sbFieldDelete.Enabled    := False;
  sbFieldDelete.Images     := FMain.ilIconFamily;
  sbFieldDelete.ImageIndex := ICON_DELETE;

  sbMenuParamAdd.Enabled    := False;
  sbMenuParamAdd.Images     := FMain.ilIconFamily;
  sbMenuParamAdd.ImageIndex := ICON_NEW;

  sbMenuParamDelete.Enabled    := False;
  sbMenuParamDelete.Images     := FMain.ilIconFamily;
  sbMenuParamDelete.ImageIndex := ICON_DELETE;

  lvActions.LargeImages := FMain.ilIconFamily;
  lvActions.SmallImages := FMain.ilIconFamily;
  lvActions.StateImages := FMain.ilIconFamily;

  Self.Hint := 'No módulo Generator você pode criar suas próprias funcionalidades no sistema';

  if vrJupiterApp.FormRoutes.Size > 0 then
  begin
    mmLines.Lines.Add(EmptyStr);
    mmLines.Lines.Add('Formulários registrados');

    for vrVez := 0 to vrJupiterApp.FormRoutes.Size - 1 do
      with TJupiterFormRoute(vrJupiterApp.FormRoutes.GetAtIndex(vrVez)) do
        mmLines.Lines.Add(Format('  - %1:s - %0:s;', [Path, FormClass.ClassName]));
  end;

  if Self.Params.Exists('Goto') then
    if Self.Params.VariableById('Goto').Value = 'ReleaseNotes' then
      pcTabs.ActivePage := TabSheet3;

  mmLines.Lines.Add(EmptyStr);
  mmLines.Lines.Add('Rotas do menu principal');
  mmLines.Lines.Add('  - ' + MENU_FILE_PATH);
  mmLines.Lines.Add('  - ' + MENU_FILE_NEW_PATH);
  mmLines.Lines.Add('  - ' + MENU_FILE_OPEN_PATH);
  mmLines.Lines.Add('  - ' + MENU_EDIT_PATH);
  mmLines.Lines.Add('  - ' + MENU_TOOLS_PATH);
  mmLines.Lines.Add('  - ' + MENU_FOLDERS_PATH);
  mmLines.Lines.Add('  - ' + MENU_HELP_PATH);
end;

procedure TFGenerator.Internal_RefreshClick(Sender: TObject);
begin
  Self.UpdateForm;
end;

procedure TFGenerator.Internal_NewMenuItemClick(Sender: TObject);
var
  vrDialog     : TJupiterDialogForm;
  vrMenuItem   : TJupiterGeneratorMenuItem;
  vrEnviroment : TJupiterEnviroment;
begin
  vrDialog     := TJupiterDialogForm.Create;
  vrMenuItem   := TJupiterGeneratorMenuItem.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrDialog.Title := 'Novo item do menu';
    vrDialog.Hint  := 'Crie um novo item de menu.';

    vrDialog.Fields.AddField('ID', 'ID do item de Menu', '');

    vrDialog.Fields.VariableFormById('ID').Required := True;

    if vrDialog.Show then
    begin
      vrMenuItem.FileName     := vrEnviroment.FullPath('modules/generator/menus/' + vrDialog.Fields.VariableFormById('ID').Value + '.xml');
      vrMenuItem.Title        := vrDialog.Fields.VariableFormById('ID').Value;
      vrMenuItem.LocationPath := ROOT_PATH;
      vrMenuItem.RoutePath    := EXPLORER_FORM_PATH;
      vrMenuItem.SaveFile;

      pcTabs.ActivePageIndex := 2;
    end;
  finally
    FreeAndNil(vrDialog);
    FreeAndNil(vrMenuItem);
    FreeAndNil(vrEnviroment);

    Self.UpdateForm;
  end;
end;

procedure TFGenerator.Internal_UpdateFormForm;
begin
  sbActionAdd.Enabled := Trim(edFile.Text) <> EmptyStr;
  sbFieldAdd.Enabled  := Trim(edFile.Text) <> EmptyStr;
  sbMenuParamAdd.Enabled := Trim(Self.FMenuFile) <> EmptyStr;

  lvActions.Enabled := Trim(edFile.Text) <> EmptyStr;
  lvFields.Enabled := Trim(edFile.Text) <> EmptyStr;

  sbActionDelete.Enabled := Assigned(lvActions.Selected);
  sbFieldDelete.Enabled  := Assigned(lvFields.Selected);

  sbMenuParamDelete.Enabled := Assigned(lvMenuVariables.Selected);
end;

procedure TFGenerator.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  mmLines.Font.Size           := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);
  mmCurrentMenuInfo.Font.Size := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);

  lbFormList.Font.Size := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);
  lvActions.Font.Size  := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);
  lvFields.Font.Size   := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);

  lbMenuList.Font.Size := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);
  lvMenuVariables.Font.Size := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);

  tvCurrentMenu.Font.Size := StrToInt(vrJupiterApp.Params.VariableById('Interface.Font.Size').Value);

  edMenuItemTitle.Enabled    := Self.FMenuFile <> EmptyStr;
  edMenuItemLocation.Enabled := Self.FMenuFile <> EmptyStr;
  edMenuItemRoute.Enabled    := Self.FMenuFile <> EmptyStr;
  lvMenuVariables.Enabled    := Self.FMenuFile <> EmptyStr;
end;

procedure TFGenerator.Internal_UpdateDatasets;
var
  vrFile        : TJupiterFileDataProvider;
  vrFileMenu    : TJupiterFileDataProvider;
  vrDatasets    : TJupiterCSVDataProvider;
  vrVez         : Integer;
  vrEnviroment  : TJupiterEnviroment;
  vrDataSetItem : TListItem;
begin
  inherited Internal_UpdateDatasets;

  lbFormList.Items.Clear;
  lbMenuList.Items.Clear;
  lvDatasets.Items.Clear;
  lvMainMenus.Items.Clear;

  vrFile           := TJupiterFileDataProvider.Create;
  vrEnviroment     := TJupiterEnviroment.Create;
  vrFileMenu       := TJupiterFileDataProvider.Create;
  vrDatasets       := TJupiterCSVDataProvider.Create;
  try
    vrFile.Path := vrEnviroment.FullPath('modules/generator/');
    vrFile.ProvideData;

    for vrVez := 0 to vrFile.Size - 1 do
      with vrFile.GetRowByIndex(vrVez) do
        lbFormList.Items.Add(Fields.VariableById('FieldName').Value);

    vrFileMenu.Path := vrEnviroment.FullPath('modules/generator/menus/');
    vrFileMenu.ProvideData;

    for vrVez := 0 to vrFileMenu.Size - 1 do
      with vrFileMenu.GetRowByIndex(vrVez) do
        lbMenuList.Items.Add(Fields.VariableById('FieldName').Value);

    if vrEnviroment.Exists('datasets/JupiterExternalVariables.csv') then
    begin
      vrDatasets.Filename := vrEnviroment.FullPath('datasets/JupiterExternalVariables.csv');
      vrDatasets.ProvideData;

      for vrVez := 0 to vrDatasets.Count - 1 do
        with vrDatasets.GetRowByIndex(vrVez) do
        begin
          vrDataSetItem := lvDatasets.Items.Add;
          vrDataSetItem.Caption := Fields.VariableById('NAME').Value;
          vrDataSetItem.SubItems.Add(Fields.VariableById('FILE').Value);
          vrDataSetItem.Data := Fields;
        end;
    end;

    for vrVez := 0 to FMain.MenuRouteList.Count - 1 do
    begin
      if FMain.MenuRouteList.MenuRouteAtIndex(vrVez).Params.Count = 0 then
        Continue;

      with FMain.MenuRouteList.MenuRouteAtIndex(vrVez) do
      begin
        vrDataSetItem := lvMainMenus.Items.Add;
        vrDataSetItem.Caption := Params.VariableById('title').Value;
        vrDataSetItem.SubItems.Add(Params.VariableById('path').Value);
        vrDataSetItem.SubItems.Add(Params.VariableById('command').Value);
      end;
    end;
  finally
    FreeAndNil(vrFile);
    FreeAndNil(vrFileMenu);
    FreeAndNil(vrEnviroment);
    FreeAndNil(vrDatasets);
  end;
end;

procedure TFGenerator.Internal_Resize;
begin
  inherited Internal_Resize;

  lvActions.Columns[0].Width := PercentOfScreen(lvActions.Width, 50);
  lvActions.Columns[1].Width := PercentOfScreen(lvActions.Width, 49);

  lvFields.Columns[0].Width := PercentOfScreen(lvFields.Width, 33);
  lvFields.Columns[1].Width := PercentOfScreen(lvFields.Width, 33);
  lvFields.Columns[2].Width := PercentOfScreen(lvFields.Width, 33);

  lvMenuVariables.Columns[0].Width := PercentOfScreen(lvMenuVariables.Width, 33);
  lvMenuVariables.Columns[1].Width := PercentOfScreen(lvMenuVariables.Width, 33);
  lvMenuVariables.Columns[2].Width := PercentOfScreen(lvMenuVariables.Width, 33);

  lvDatasets.Columns[0].Width := PercentOfScreen(lvDatasets.Width, 50);
  lvDatasets.Columns[1].Width := PercentOfScreen(lvDatasets.Width, 49);

  lvMainMenus.Columns[0].Width := PercentOfScreen(lvMainMenus.Width, 33);
  lvMainMenus.Columns[1].Width := PercentOfScreen(lvMainMenus.Width, 33);
  lvMainMenus.Columns[2].Width := PercentOfScreen(lvMainMenus.Width, 33);
end;

end.

