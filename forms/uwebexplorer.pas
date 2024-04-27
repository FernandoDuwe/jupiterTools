unit uwebexplorer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, uWVBrowser,
  uWVWindowParent, IpHtml, Iphttpbroker, JupiterForm, JupiterEnviroment,
  uWVLoader;

type

  { TFWebExplorer }

  TFWebExplorer = class(TFJupiterForm)
    Timer1: TTimer;
    WVBrowser1: TWVBrowser;
    WVWindowParent1: TWVWindowParent;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure WVBrowser1AfterCreated(Sender: TObject);
  private
  protected
    procedure Internal_PrepareForm; override;

    procedure UpdateForm(prUpdateDatasets : Boolean = True; prUpdateComponentes : Boolean = True; prUpdateCalcs : Boolean = True); override;
  public

  end;

var
  FWebExplorer: TFWebExplorer;

implementation

uses uWVTypes, uWVCoreWebView2WebResourceRequest, uWVCoreWebView2HttpRequestHeaders;

{$R *.lfm}

{ TFWebExplorer }

procedure TFWebExplorer.WVBrowser1AfterCreated(Sender: TObject);
begin
end;

procedure TFWebExplorer.FormShow(Sender: TObject);
begin
  inherited;

  if GlobalWebView2Loader.InitializationError then
     ShowMessage(GlobalWebView2Loader.ErrorMessage)
  else
    if GlobalWebView2Loader.Initialized then
      WVBrowser1.CreateBrowser(WVWindowParent1.Handle)
    else
      Timer1.Enabled := True;
end;

procedure TFWebExplorer.FormCreate(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
begin
  inherited;

  vrEnviroment := TJupiterEnviroment.Create;
  try
    if not Assigned(GlobalWebView2Loader) then
    begin
      GlobalWebView2Loader := TWVLoader.Create(nil);

      vrEnviroment.CreatePath('/temp/customcache/');

      GlobalWebView2Loader.UserDataFolder := UTF8Decode(vrEnviroment.FullPath('/temp/customcache/'));

      // Set GlobalWebView2Loader.BrowserExecPath if you don't want to use the evergreen version of WebView Runtime
      // GlobalWebView2Loader.BrowserExecPath := 'C:\Program Files\BraveSoftware\Brave-Browser\Application\brave.exe';

      // Uncomment these lines to enable the debug log in 'CustomCache\EBWebView\chrome_debug.log'
      //GlobalWebView2Loader.DebugLog       := TWV2DebugLog.dlEnabled;
      //GlobalWebView2Loader.DebugLogLevel  := TWV2DebugLogLevel.dllInfo;

      GlobalWebView2Loader.StartWebView2;
    end;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFWebExplorer.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;

  if GlobalWebView2Loader.Initialized then
    WVBrowser1.CreateBrowser(WVWindowParent1.Handle)
   else
    Timer1.Enabled := True;
end;

procedure TFWebExplorer.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  WVBrowser1.BrowserExecPath := 'C:\Program Files\BraveSoftware\Brave-Browser\Application\brave.exe';

  WVBrowser1.CreateBrowser(WVWindowParent1.Handle);

  if Self.Params.Exists('destiny') then
    WVBrowser1.DefaultURL := Self.Params.VariableById('destiny').Value
  else
    WVBrowser1.DefaultURL := 'https://www.google.com/';

  WVBrowser1.Navigate(WVBrowser1.DefaultURL);
end;

procedure TFWebExplorer.UpdateForm(prUpdateDatasets: Boolean; prUpdateComponentes: Boolean; prUpdateCalcs: Boolean);
begin
  inherited UpdateForm(prUpdateDatasets, prUpdateComponentes, prUpdateCalcs);

  WVBrowser1.Refresh;
end;

initialization

end.

