unit uInternetExplorer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, IpHtml,
  uJupiterForm, JupiterConsts, JupiterApp, uJupiterAction, uWVBrowser, Messages,
  uWVWindowParent, uWVLoader, uWVBrowserBase, uWVTypes, uWVEvents;



type

  { TFInternetExplorer }

  TFInternetExplorer = class(TFJupiterForm)
    Panel1: TPanel;
    Timer1: TTimer;
    wvBrowser: TWVBrowser;
    wvPanel: TWVWindowParent;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure wvBrowserAfterCreated(Sender: TObject);
    procedure wvBrowserDocumentTitleChanged(Sender: TObject);
    procedure wvBrowserInitializationError(Sender: TObject; aErrorCode: HRESULT; const aErrorMessage: wvstring);
  private
    procedure Internal_OnUpdate(Sender: TObject);
    procedure Internal_OnBack(Sender: TObject);
    procedure Internal_OnNext(Sender: TObject);

    // It's necessary to handle these messages to call NotifyParentWindowPositionChanged or some page elements will be misaligned.
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;

    procedure Internal_PrepareForm; override;
    procedure Internal_UpdateComponents; override;
  public

  end;

var
  FInternetExplorer: TFInternetExplorer;

implementation

{$R *.lfm}

{ TFInternetExplorer }

procedure TFInternetExplorer.FormShow(Sender: TObject);
begin
  inherited;

  if GlobalWebView2Loader.InitializationError then
    ShowMessage(UTF8Encode(GlobalWebView2Loader.ErrorMessage))
  else
    if GlobalWebView2Loader.Initialized then
    begin
      vrJupiterApp.AddMessage('Navegador', 'Criando navegador', Self.ClassName);

      wvBrowser.CreateBrowser(wvPanel.Handle);

      Self.UpdateForm();
    end
    else
    begin
      vrJupiterApp.AddMessage('Navegador', 'WebLoader ainda não iniciado', Self.ClassName);

      Timer1.Enabled := True;
    end;
end;

procedure TFInternetExplorer.FormCreate(Sender: TObject);
begin
  inherited;

  wvBrowser.BrowserExecPath := vrJupiterApp.Params.VariableById('InternetExplorer.BrowserExec.Path').Value;
  wvBrowser.Language        := vrJupiterApp.Params.VariableById('InternetExplorer.Language').Value;
  wvBrowser.DefaultURL      := vrJupiterApp.Params.VariableById('InternetExplorer.DefaultURL').Value;
  wvBrowser.UserDataFolder  := vrJupiterApp.Params.VariableById('InternetExplorer.UserDataFolder.Path').Value;;
end;

procedure TFInternetExplorer.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;

  if GlobalWebView2Loader.Initialized then
  begin
    wvBrowser.CreateBrowser(wvPanel.Handle);

    Internal_OnUpdate(Sender);
  end
  else
    Timer1.Enabled := True;
end;

procedure TFInternetExplorer.wvBrowserAfterCreated(Sender: TObject);
begin
  wvPanel.UpdateSize;
end;

procedure TFInternetExplorer.wvBrowserDocumentTitleChanged(Sender: TObject);
begin
  Self.Caption := UTF8Encode(wvBrowser.DocumentTitle);

  Self.UpdateForm();
end;

procedure TFInternetExplorer.wvBrowserInitializationError(Sender: TObject; aErrorCode: HRESULT; const aErrorMessage: wvstring);
begin
  ShowMessage(UTF8Encode(aErrorMessage));
end;

procedure TFInternetExplorer.Internal_OnUpdate(Sender: TObject);
begin
  if Params.Exists('params') then
  begin
    wvBrowser.DefaultURL := Params.VariableById('params').Value;

    wvBrowser.Navigate(UTF8Decode(Params.VariableById('params').Value))
  end
  else
    wvBrowser.Navigate(UTF8Decode(wvBrowser.DefaultURL));

  Self.UpdateForm();
end;

procedure TFInternetExplorer.Internal_OnBack(Sender: TObject);
begin
  wvBrowser.GoBack;
end;

procedure TFInternetExplorer.Internal_OnNext(Sender: TObject);
begin
  wvBrowser.GoForward;
end;

procedure TFInternetExplorer.WMMove(var aMessage: TWMMove);
begin
  inherited;

  if (wvBrowser <> nil) then
    wvBrowser.NotifyParentWindowPositionChanged;
end;

procedure TFInternetExplorer.WMMoving(var aMessage: TMessage);
begin
  inherited;

  if (wvBrowser <> nil) then
    wvBrowser.NotifyParentWindowPositionChanged;
end;

procedure TFInternetExplorer.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  if vrJupiterApp.Params.VariableById('InternetExplorer.ShowMainActionsInForm').AsBool then
  begin
    Self.ActionGroup.AddAction(TJupiterAction.Create('Voltar', 'Clique aqui para voltar', ICON_LEFT, @Internal_OnBack));
    Self.ActionGroup.AddAction(TJupiterAction.Create('Avançar', 'Clique aqui para avançar', ICON_RIGHT, @Internal_OnNext));
    Self.ActionGroup.AddAction(TJupiterAction.Create('Atualizar', 'Clique aqui para atualizar', ICON_REFRESH, @Internal_OnUpdate));
  end;

  Internal_OnUpdate(Self);
end;

procedure TFInternetExplorer.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if Self.ActionGroup.Count >= 1 then
  begin
    if wvBrowser.CanGoBack then
      ActionGroup.GetActionAtIndex(0).Enable
    else
      ActionGroup.GetActionAtIndex(0).Disable;
  end;

  if Self.ActionGroup.Count >= 2 then
  begin
    if wvBrowser.CanGoForward then
      ActionGroup.GetActionAtIndex(1).Enable
    else
      ActionGroup.GetActionAtIndex(1).Disable;
  end;
end;

initialization
  GlobalWebView2Loader                := TWVLoader.Create(nil);
  GlobalWebView2Loader.UserDataFolder := UTF8Decode(ExtractFileDir(Application.ExeName) + '\CustomCache');
  GlobalWebView2Loader.StartWebView2;

end.

