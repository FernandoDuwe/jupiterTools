unit uSystemMonitor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, uJupiterForm,
  jupiterformutils, JupiterApp, jupiterScript, JupiterDataProvider,
  JupiterConsts, uJupiterAction, jupiterDesktopApp;

type

  { TFSystemMonitor }

  TFSystemMonitor = class(TFJupiterForm)
    lvForms: TListView;
    lvMessages: TListView;
    lvThreads: TListView;
    lvScripts: TListView;
    lvDataProviders: TListView;
    lvScriptList: TListView;
    pcPages: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    tsMessages: TTabSheet;
    tsScriptList: TTabSheet;
    tsDataProviders: TTabSheet;
    tsScripts: TTabSheet;
  private
    procedure Internal_PrepareForm; override;

    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_UpdateCalcs; override;

    procedure Internal_OnDeleteAsset(Sender: TObject);
    procedure Internal_OnCleanAsset(Sender: TObject);
  public

  end;

var
  FSystemMonitor: TFSystemMonitor;

implementation

{$R *.lfm}

{ TFSystemMonitor }

procedure TFSystemMonitor.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

//  Self.ActionGroup.AddAction(TJupiterAction.Create('Encerrar', 'Clique aqui para finalizar o recurso atual', ICON_DELETE, @Internal_OnDeleteAsset));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Limpar', 'Clique aqui para limpar os logs de sistema', ICON_NEW, @Internal_OnCleanAsset));
end;

procedure TFSystemMonitor.Internal_UpdateComponents;
begin
  Self.Hint := 'Nesta tela você pode controlar todos os recursos em uso do sistema';

  inherited Internal_UpdateComponents;

  lvScripts.Column[0].Width := PercentOfScreen(lvScripts.Width, 50);
  lvDataProviders.Column[0].Width := PercentOfScreen(lvDataProviders.Width, 50);

  lvScriptList.Column[0].Width := PercentOfScreen(lvScriptList.Width, 25);
  lvScriptList.Column[1].Width := PercentOfScreen(lvScriptList.Width, 25);
  lvScriptList.Column[2].Width := PercentOfScreen(lvScriptList.Width, 25);
  lvScriptList.Column[3].Width := PercentOfScreen(lvScriptList.Width, 25);

  lvMessages.Column[0].Width := PercentOfScreen(lvMessages.Width, 25);
  lvMessages.Column[1].Width := PercentOfScreen(lvMessages.Width, 25);
  lvMessages.Column[2].Width := PercentOfScreen(lvMessages.Width, 25);
  lvMessages.Column[3].Width := PercentOfScreen(lvMessages.Width, 25);

  lvThreads.Column[0].Width := PercentOfScreen(lvMessages.Width, 10);
  lvThreads.Column[1].Width := PercentOfScreen(lvMessages.Width, 30);
  lvThreads.Column[2].Width := PercentOfScreen(lvMessages.Width, 30);
  lvThreads.Column[3].Width := PercentOfScreen(lvMessages.Width, 30);

//  Self.ActionGroup.GetActionAtIndex(0).Disable;
end;

procedure TFSystemMonitor.Internal_UpdateDatasets;
var
  vrVez : Integer;
  vrItem : TListItem;
begin
  inherited Internal_UpdateDatasets;

  lvScripts.Items.Clear;
  lvDataProviders.Items.Clear;
  lvForms.Items.Clear;
  lvScriptList.Clear;
  lvMessages.Items.Clear;
  lvThreads.Items.Clear;
  try
    for vrVez := 0 to vrJupiterApp.Scripts.Count - 1 do
      with TJupiterScript(vrJupiterApp.Scripts.GetAtIndex(vrVez)) do
      begin
        vrItem := lvScripts.Items.Add;
        vrItem.Caption := TJupiterScript(vrJupiterApp.Scripts.GetAtIndex(vrVez)).ScriptID;
      end;

    for vrVez := 0 to vrJupiterApp.DataProviders.Count - 1 do
      with TJupiterDataProvider(vrJupiterApp.DataProviders.GetAtIndex(vrVez)) do
      begin
        vrItem := lvDataProviders.Items.Add;
        vrItem.Caption := TJupiterDataProvider(vrJupiterApp.DataProviders.GetAtIndex(vrVez)).ProviderID;
      end;

    for vrVez := 0 to TJupiterDesktopApp(vrJupiterApp).FormList.Count - 1 do
      with TFJupiterForm(TJupiterDesktopApp(vrJupiterApp).FormList.GetAtIndex(vrVez)) do
      begin
        vrItem := lvForms.Items.Add;
        vrItem.Caption := TFJupiterForm(TJupiterDesktopApp(vrJupiterApp).FormList.GetAtIndex(vrVez)).FormID;
      end;

    for vrVez := vrJupiterApp.ScriptList.Count - 1 downto 0 do
      with vrJupiterApp.ScriptList.GetRowByIndex(vrVez) do
      begin
        vrItem := lvScriptList.Items.Add;
        vrItem.Caption := Fields.VariableById('script').Value;

        vrItem.SubItems.Add(Fields.VariableById('messages').Value);
        vrItem.SubItems.Add(Fields.VariableById('runMessages').Value);
        vrItem.SubItems.Add(Fields.VariableById('executed').Value);
      end;

    for vrVez := vrJupiterApp.MessageList.Count - 1 downto 0 do
          with vrJupiterApp.MessageList.GetRowByIndex(vrVez) do
          begin
            vrItem := lvMessages.Items.Add;
            vrItem.Caption := Fields.VariableById('title').Value;

            vrItem.SubItems.Add(Fields.VariableById('message').Value);
            vrItem.SubItems.Add(Fields.VariableById('origin').Value);
            vrItem.SubItems.Add(Fields.VariableById('dateTime').Value);
          end;

    for vrVez := 0 to vrJupiterApp.ThreadList.Count - 1 do
    begin
      if not Assigned(vrJupiterApp.ThreadList.ThreadByIndex(vrVez)) then
        Continue;

      vrItem := lvThreads.Items.Add;
      vrItem.Caption := IntToStr(vrJupiterApp.ThreadList.ThreadByIndex(vrVez).ID);

      vrItem.SubItems.Add(vrJupiterApp.ThreadList.ThreadByIndex(vrVez).Title);
      vrItem.SubItems.Add(FormatDateTime(FORMAT_DATETIME, vrJupiterApp.ThreadList.ThreadByIndex(vrVez).StartedAt));

      if vrJupiterApp.ThreadList.ThreadByIndex(vrVez).EndedAt <> 0.0 then
        vrItem.SubItems.Add(FormatDateTime(FORMAT_DATETIME, vrJupiterApp.ThreadList.ThreadByIndex(vrVez).EndedAt))
      else
        vrItem.SubItems.Add(EmptyStr);
    end;
  finally
  end;
end;

procedure TFSystemMonitor.Internal_UpdateCalcs;
begin
  inherited Internal_UpdateCalcs;

  pcPages.Pages[0].Caption := Format('Scripts (%0:d)', [lvScripts.Items.Count]);
  pcPages.Pages[1].Caption := Format('Data Providers (%0:d)', [lvDataProviders.Items.Count]);
  pcPages.Pages[2].Caption := Format('Formulários (%0:d)', [lvForms.Items.Count]);
  pcPages.Pages[3].Caption := Format('Log de Scripts (%0:d)', [lvScriptList.Items.Count]);
  pcPages.Pages[4].Caption := Format('Mensagens (%0:d)', [lvMessages.Items.Count]);
  pcPages.Pages[5].Caption := Format('Threads (%0:d)', [lvThreads.Items.Count]);
end;

procedure TFSystemMonitor.Internal_OnDeleteAsset(Sender: TObject);
begin
  //
end;

procedure TFSystemMonitor.Internal_OnCleanAsset(Sender: TObject);
begin
  try
    vrJupiterApp.ScriptList.ClearRows;

    vrJupiterApp.MessageList.ClearRows;
  finally
    Self.UpdateForm();
  end;
end;

end.

