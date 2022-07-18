unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  Menus, StdCtrls, Buttons, JupiterApp, JupiterConfig, JupiterModule,
  uJupiterForm, uConfig, uExplorer, uCurrentTask, uNewTask, uScriptEditor,
  uMessage, uCustomForm, JupiterConsts, fileUtils, jupiterUtils, JupiterTasks,
  jupiterchecklist, jupiterRunner;

type

  { TFMain }

  TFMain = class(TJupiterForm)
    ApplicationProperties1: TApplicationProperties;
    edSearch: TEdit;
    ilMainIcons: TImageList;
    imgLogo: TImage;
    Label2: TLabel;
    lbVersion: TLabel;
    lvRecents: TListView;
    MenuItem25: TMenuItem;
    mmInstructions: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    sbHome: TSpeedButton;
    Separator4: TMenuItem;
    Separator3: TMenuItem;
    Separator2: TMenuItem;
    Separator1: TMenuItem;
    MenuItem3: TMenuItem;
    miCleanSearchChangeItem: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    miAutoUpdate: TMenuItem;
    N1: TMenuItem;
    pnTaskBar: TPanel;
    sbRefresh: TSpeedButton;
    mMenu: TMainMenu;
    pnBody: TPanel;
    pnLeft: TPanel;
    sbStatus: TStatusBar;
    Splitter1: TSplitter;
    tvItens: TTreeView;
    procedure ApplicationProperties1Activate(Sender: TObject);
    procedure ApplicationProperties1Restore(Sender: TObject);
    procedure ApplicationProperties1ShowHint(var HintStr: string;
      var CanShow: Boolean; var HintInfo: THintInfo);
    procedure edSearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvRecentsClick(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure MenuItem21Click(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure MenuItem23Click(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure MenuItem25Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure miAutoUpdateClick(Sender: TObject);
    procedure miCleanSearchChangeItemClick(Sender: TObject);
    procedure miModulesClick(Sender: TObject);
    procedure miConfigClick(Sender: TObject);
    procedure pnBodyClick(Sender: TObject);
    procedure pnTaskBarClick(Sender: TObject);
    procedure sbHomeClick(Sender: TObject);
    procedure sbRefreshClick(Sender: TObject);
    procedure tvItensClick(Sender: TObject);
    procedure tvItensCollapsed(Sender: TObject; Node: TTreeNode);
  private
    FCurrentForm : TJupiterForm;
    FProcessing  : Boolean;

    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_ShowForm(prItem : TJupiterListem);
    function Internal_CurrentTask : TJupiterTaskDetails;
    procedure Internal_ListRecents;
    function Internal_GetTreeViewByHash(prStrObject : String) : TTreeNode;
  public
    procedure UpdateForm; override;
  end;

var
  FMain: TFMain;

implementation

uses LCLType;

{$R *.lfm}

{ TFMain }

procedure TFMain.miConfigClick(Sender: TObject);
begin

end;

procedure TFMain.pnBodyClick(Sender: TObject);
begin

end;

procedure TFMain.pnTaskBarClick(Sender: TObject);
begin

end;

procedure TFMain.sbHomeClick(Sender: TObject);
begin
  if Assigned(Self.FCurrentForm) then
  begin
    Self.FCurrentForm.Release;
    FreeAndNil(Self.FCurrentForm);
  end;

  sbStatus.Panels[2].Text := EmptyStr;

  Self.Caption := 'Página Inicial - ' + vrJupiterApp.AppName;

  Self.UpdateForm;
end;

procedure TFMain.sbRefreshClick(Sender: TObject);
begin
  Self.UpdateForm;
end;

procedure TFMain.tvItensClick(Sender: TObject);
begin
  if not Assigned(tvItens.Selected) then
    Exit;

  if not Assigned(tvItens.Selected.Data) then
    Exit;

  sbStatus.Panels[2].Text := tvItens.Selected.Text;

  Self.Caption := tvItens.Selected.Text + ' - ' + vrJupiterApp.AppName;

  Self.FProcessing := True;
  try
    if miCleanSearchChangeItem.Checked then
      edSearch.Text := EmptyStr;

    Self.Internal_ShowForm(TJupiterListem(tvItens.Selected.Data));
  finally
    Self.FProcessing := False;
  end;
end;

procedure TFMain.tvItensCollapsed(Sender: TObject; Node: TTreeNode);
begin

end;

procedure TFMain.Internal_UpdateComponents;
var
  vrCount : Integer;
begin
  inherited Internal_UpdateComponents;

  lbVersion.Caption := 'Versão: ' + vrJupiterApp.GetVersion;

  if vrJupiterApp.Config.Exists('JupiterTools.Modules.Tasks.CurrentNumber') then
  begin
    MenuItem21.Enabled := True;
    MenuItem22.Enabled := not Self.Internal_CurrentTask.TimeNote.Started;
    MenuItem23.Enabled := Self.Internal_CurrentTask.TimeNote.Started;
  end
  else
  begin
    MenuItem21.Enabled := False;
    MenuItem22.Enabled := False;
    MenuItem23.Enabled := False;
  end;

  if ((tvItens.Items.Count = 0) or (tvItens.Focused)) then
  begin
    tvItens.SortType := stNone;
    tvItens.Items.Clear;

    for vrCount := 0 to vrJupiterApp.ModuleCount - 1 do
        TJupiterModule(vrJupiterApp.GetModuleByIndex(vrCount)).GetTasks(tvItens);

    tvItens.SortType := stText;
  end;

  lvRecents.Items.Clear;
  Self.Internal_ListRecents;

  if Assigned(Self.FCurrentForm) then
     Self.FCurrentForm.UpdateForm;

  sbRefresh.Height := edSearch.Height;
  sbRefresh.Width  := edSearch.Height;

  sbHome.Height := edSearch.Height;
  sbHome.Width  := edSearch.Height;

  pnTaskBar.Height := (edSearch.Top * 2) + edSearch.Height;

  tvItens.Font.Size := StrToInt(vrJupiterApp.Config.GetByID('JupiterTools.UI.Display.FontSize').Value);

  if vrJupiterApp.Config.Exists('JupiterTools.Modules.Tasks.CurrentNumber') then
    sbStatus.Panels[1].Text := Format('Tarefa atual: %0:s', [vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.CurrentNumber').Value])
  else
    sbStatus.Panels[1].Text := 'Nenhuma tarefa selecionada';

  sbStatus.Panels[3].Text := Format('Mensagens: %0:d', [vrJupiterApp.Log.Count]);
end;

procedure TFMain.Internal_UpdateDatasets;
begin
  inherited Internal_UpdateDatasets;
end;

procedure TFMain.Internal_ShowForm(prItem: TJupiterListem);
begin
  vrJupiterApp.RegisterRecentTask(prItem);

  if Assigned(Self.FCurrentForm) then
  begin
    Self.FCurrentForm.Release;
    FreeAndNil(Self.FCurrentForm);
  end;

  case prItem.Tag of
    0 : begin
          Self.FCurrentForm := TFExplorer.Create(pnBody);
          TFExplorer(FCurrentForm).Params.CopyFrom(prItem);
        end;
    30 : begin
           Self.FCurrentForm := TFCustomForm.Create(pnBody);

           TFCustomForm(FCurrentForm).CustomAction.FileName := prItem.Params;
         end;
    50 : begin
           Self.FCurrentForm := TFScriptEditor.Create(pnBody);

           TFScriptEditor(FCurrentForm).FileName := prItem.Params;
         end;
    90 : begin
           Self.FCurrentForm := TFCurrentTask.Create(pnBody);
         end;
   end;

  Self.FCurrentForm.Parent      := pnBody;
  Self.FCurrentForm.WindowState := wsMaximized;
  Self.FCurrentForm.BorderStyle := bsNone;
  Self.FCurrentForm.Align       := alClient;

  try
    Self.FCurrentForm.Show;
  finally
    Self.FCurrentForm.UpdateForm;

    if not miCleanSearchChangeItem.Checked then
      Self.FCurrentForm.Search(edSearch.Text);
  end;
end;

function TFMain.Internal_CurrentTask: TJupiterTaskDetails;
var
  vrList : TList;
begin
  Result := TJupiterTasks(vrJupiterApp.GetModuleByID('JupiterTools.Modules.Tasks')).CreateTaskDetail;

  vrList := TList.Create;
  try
    Result.TimeNote.GetTimes(vrList);
  finally
    vrList.Clear;
    FreeAndNil(vrList);
  end;
end;

procedure TFMain.Internal_ListRecents;
var
  vrStr  : TStrings;
  vrVez  : Integer;
  vrObj  : TJupiterListem;
  vrNode : TListItem;
  vrTree : TTreeNode;
begin
  lvRecents.Items.Clear;

  if not FileExists(TratarCaminho(vrJupiterApp.Config.GetByID('JupiterTools.Variables.Path').Value + '\datasets\recents.csv')) then
    Exit;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.LoadFromFile(TratarCaminho(vrJupiterApp.Config.GetByID('JupiterTools.Variables.Path').Value + '\datasets\recents.csv'));

    for vrVez := 1 to vrStr.Count - 1 do
    begin
      if Trim(vrStr[vrVez]) = EmptyStr then
        Continue;

      vrTree := Self.Internal_GetTreeViewByHash(vrStr[vrVez]);

      if not Assigned(vrTree) then
        Continue;

      vrObj := TJupiterListem.Create(EmptyStr,EmptyStr);
      vrObj.StrToObject(vrStr[vrVez]);

      vrNode := lvRecents.Items.Add;
      vrNode.Caption := vrTree.Text;
      vrNode.ImageIndex := vrTree.ImageIndex;
      vrNode.Data := vrObj;
    end;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;

end;

function TFMain.Internal_GetTreeViewByHash(prStrObject: String): TTreeNode;
var
  vrVez : Integer;
begin
  Result := nil;

  for vrVez := 0 to tvItens.Items.Count - 1 do
  begin
    if not Assigned(tvItens.Items[vrVez].Data) then
      Continue;

    if TJupiterListem(tvItens.Items[vrVez].Data).ObjectToStr = prStrObject then
      Result := tvItens.Items[vrVez];
  end;
end;

procedure TFMain.UpdateForm;
var
  vrTime : TDateTime;
begin
  vrTime := Now;

  try
    inherited UpdateForm;
  finally
    vrTime := Now - vrTime;

    vrJupiterApp.Log.AddLog(Now, Self.Caption, 'Atualização de tela efetuada. Tempo: ' + FormatDateTime('hh:nn:ss:zzz', vrTime));
  end;
end;

procedure TFMain.MenuItem7Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFMain.MenuItem8Click(Sender: TObject);
begin
  Self.UpdateForm;
end;

procedure TFMain.MenuItem9Click(Sender: TObject);
var
  vrValue : Integer;
begin
  try
    vrValue := StrToInt(vrJupiterApp.Config.GetByID('JupiterTools.UI.Display.FontSize').Value);

    vrJupiterApp.Config.AddConfig('JupiterTools.UI.Display.FontSize', IntToStr(vrValue + 1), 'Tamanho da fonte dos formulários');
  finally
    Self.UpdateForm;
  end;
end;

procedure TFMain.miAutoUpdateClick(Sender: TObject);
begin
  miAutoUpdate.Checked := not miAutoUpdate.Checked;
end;

procedure TFMain.miCleanSearchChangeItemClick(Sender: TObject);
begin
  miCleanSearchChangeItem.Checked := not miCleanSearchChangeItem.Checked;
end;

procedure TFMain.FormResize(Sender: TObject);
begin
  Self.UpdateForm;
end;

procedure TFMain.FormShow(Sender: TObject);
begin
  if vrJupiterApp.Config.GetByID('JupiterTools.UI.Display.WindowsState').Value = 'Maximized' then
    Self.WindowState := wsMaximized;

  Self.Caption := 'Página Inicial - ' + vrJupiterApp.AppName;
end;

procedure TFMain.lvRecentsClick(Sender: TObject);
begin
  if not Assigned(lvRecents.Selected) then
    Exit;

  if not Assigned(lvRecents.Selected.Data) then
    Exit;

  sbStatus.Panels[2].Text := lvRecents.Selected.Caption;

  Self.Caption := lvRecents.Selected.Caption + ' - ' + vrJupiterApp.AppName;

  Self.FProcessing := True;
  try
    if miCleanSearchChangeItem.Checked then
      edSearch.Text := EmptyStr;

    Self.Internal_ShowForm(TJupiterListem(lvRecents.Selected.Data));
  finally
    Self.FProcessing := False;
  end;

end;

procedure TFMain.MenuItem10Click(Sender: TObject);
var
  vrValue : Integer;
begin
  try
    vrValue := StrToInt(vrJupiterApp.Config.GetByID('JupiterTools.UI.Display.FontSize').Value);

    vrJupiterApp.Config.AddConfig('JupiterTools.UI.Display.FontSize', IntToStr(vrValue - 1), 'Tamanho da fonte dos formulários');
  finally
    Self.UpdateForm;
  end;
end;

procedure TFMain.MenuItem11Click(Sender: TObject);
begin
  Application.CreateForm(TFConfig, FConfig);
  try
    FConfig.ShowModal;
  finally
    FConfig.Release;
    FreeAndNil(FConfig);
  end;
end;

procedure TFMain.MenuItem12Click(Sender: TObject);
begin
  vrJupiterApp.Log.AddLog(Now, vrJupiterApp.AppName, 'Abrindo pasta: ' + ExtractFileDir(Application.ExeName));

  OpenFolder(ExtractFileDir(Application.ExeName));
end;

procedure TFMain.MenuItem13Click(Sender: TObject);
begin
  vrJupiterApp.Log.AddLog(Now, vrJupiterApp.AppName, 'Abrindo pasta: ' + TratarCaminho(ExtractFileDir(Application.ExeName) + '\datasets\'));

  OpenFolder(TratarCaminho(ExtractFileDir(Application.ExeName) + '\datasets\'));
end;

procedure TFMain.MenuItem14Click(Sender: TObject);
begin
  vrJupiterApp.Log.AddLog(Now, vrJupiterApp.AppName, 'Abrindo pasta: ' + TratarCaminho(ExtractFileDir(Application.ExeName) + '\modules\runner\'));

  OpenFolder(TratarCaminho(ExtractFileDir(Application.ExeName) + '\modules\runner\'));
end;

procedure TFMain.MenuItem15Click(Sender: TObject);
begin
  vrJupiterApp.Log.AddLog(Now, vrJupiterApp.AppName, 'Abrindo pasta: ' + TratarCaminho(ExtractFileDir(Application.ExeName) + '\modules\checklist\'));

  OpenFolder(TratarCaminho(ExtractFileDir(Application.ExeName) + '\modules\checklist\'));
end;

procedure TFMain.MenuItem16Click(Sender: TObject);
begin
  vrJupiterApp.Log.AddLog(Now, vrJupiterApp.AppName, 'Abrindo pasta: ' + TratarCaminho(ExtractFileDir(Application.ExeName) + '\modules\tasks\'));

  OpenFolder(TratarCaminho(ExtractFileDir(Application.ExeName) + '\modules\tasks\'));
end;

procedure TFMain.MenuItem17Click(Sender: TObject);
begin
  vrJupiterApp.Log.AddLog(Now, vrJupiterApp.AppName, 'Abrindo pasta: ' + TratarCaminho(ExtractFileDir(Application.ExeName) + '\temp\'));

  OpenFolder(TratarCaminho(ExtractFileDir(Application.ExeName) + '\temp\'));
end;

procedure TFMain.MenuItem18Click(Sender: TObject);
var
  vrObj : TJupiterActionChecklistNewFileCheckList;
begin
  vrObj := TJupiterActionChecklistNewFileCheckList.Create;
  try
    vrObj.Run(TJupiterListem.Create(EmptyStr, EmptyStr));
  finally
    FreeAndNil(vrObj);

    if tvItens.CanFocus then
      tvItens.SetFocus;

    Self.UpdateForm;
  end;
end;

procedure TFMain.MenuItem19Click(Sender: TObject);
var
  vrObj : TJupiterRunnerNewScriptBAT;
begin
  vrObj := TJupiterRunnerNewScriptBAT.Create;
  try
    vrObj.Run(TJupiterListem.Create(EmptyStr, EmptyStr));
  finally
    FreeAndNil(vrObj);

    if tvItens.CanFocus then
      tvItens.SetFocus;

    Self.UpdateForm;
  end;
end;

procedure TFMain.MenuItem20Click(Sender: TObject);
var
  vrObj : TJupiterRunnerNewScriptSQL;
begin
  vrObj := TJupiterRunnerNewScriptSQL.Create;
  try
    vrObj.Run(TJupiterListem.Create(EmptyStr, EmptyStr));
  finally
    FreeAndNil(vrObj);

    if tvItens.CanFocus then
      tvItens.SetFocus;

    Self.UpdateForm;
  end;
end;

procedure TFMain.MenuItem21Click(Sender: TObject);
begin
  vrJupiterApp.Log.AddLog(Now, vrJupiterApp.AppName, 'Abrindo tarefa atual: ' + vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.Current').Value);

  OpenFolder(vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.Current').Value);
end;

procedure TFMain.MenuItem22Click(Sender: TObject);
begin
  try
    Self.Internal_CurrentTask.TimeNote.Start;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFMain.MenuItem23Click(Sender: TObject);
begin
  try
    Self.Internal_CurrentTask.TimeNote.Finish;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFMain.MenuItem24Click(Sender: TObject);
begin
  Application.CreateForm(TFMessage, FMessage);
  try
    FMessage.ShowModal;
  finally
    FMessage.Release;
    FreeAndNil(FMessage);
  end;
end;

procedure TFMain.MenuItem25Click(Sender: TObject);
begin
  vrJupiterApp.Log.AddLog(Now, vrJupiterApp.AppName, 'Abrindo pasta: ' + TratarCaminho(ExtractFileDir(Application.ExeName) + '\modules\generator\'));

  OpenFolder(TratarCaminho(ExtractFileDir(Application.ExeName) + '\modules\generator\'));
end;

procedure TFMain.MenuItem2Click(Sender: TObject);
begin

end;

procedure TFMain.MenuItem4Click(Sender: TObject);
begin
  Application.MessageBox(PAnsiChar(vrJupiterApp.GetAboutInfo), PAnsiChar(Self.Caption), MB_ICONINFORMATION + MB_OK);
end;

procedure TFMain.MenuItem6Click(Sender: TObject);
var
  vrObj : TJupiterActionTasksNewTask;
begin
  vrObj := TJupiterActionTasksNewTask.Create;
  try
    vrObj.Run(TJupiterListem.Create(EmptyStr, EmptyStr));
  finally
    FreeAndNil(vrObj);

    if tvItens.CanFocus then
      tvItens.SetFocus;

    Self.UpdateForm;
  end;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  Self.FProcessing := False;
end;

procedure TFMain.edSearchChange(Sender: TObject);
begin
  if Self.FProcessing then
    Exit;

  try
    Self.Search(edSearch.Text);

    if Assigned(Self.FCurrentForm) then
       Self.FCurrentForm.Search(edSearch.Text);
  finally
    Self.UpdateForm;
  end;
end;

procedure TFMain.ApplicationProperties1Restore(Sender: TObject);
begin
  if miAutoUpdate.Checked then
    Self.UpdateForm;
end;

procedure TFMain.ApplicationProperties1Activate(Sender: TObject);
begin

end;

procedure TFMain.ApplicationProperties1ShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin

end;

procedure TFMain.miModulesClick(Sender: TObject);
begin

end;

end.

