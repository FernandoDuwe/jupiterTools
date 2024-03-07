unit uwebexplorer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, uWVBrowser,
  uWVWindowParent, IpHtml, Iphttpbroker, JupiterForm;

type

  { TFWebExplorer }

  TFWebExplorer = class(TFJupiterForm)
    Timer1: TTimer;
    WVBrowser1: TWVBrowser;
    WVWindowParent1: TWVWindowParent;
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure WVBrowser1AfterCreated(Sender: TObject);
  private
  protected
    procedure Internal_PrepareForm; override;
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

  {
  if GlobalWebView2Loader.InitializationError then
     ShowMessage(GlobalWebView2Loader.ErrorMessage)
  else
    if GlobalWebView2Loader.Initialized then
      WVBrowser1.CreateBrowser(WVWindowParent1.Handle)
    else
      Timer1.Enabled := True;
      }
end;

procedure TFWebExplorer.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
       {
  if GlobalWebView2Loader.Initialized then
    WVBrowser1.CreateBrowser(WVWindowParent1.Handle)
   else
    Timer1.Enabled := True;
    }
end;

procedure TFWebExplorer.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  WVBrowser1.BrowserExecPath := 'C:\Program Files\BraveSoftware\Brave-Browser\Application\brave.exe';

  WVBrowser1.CreateBrowser(WVWindowParent1.Handle);

  if Self.Params.Exists('destiny') then
    WVBrowser1.DefaultURL := Self.Params.VariableById('destiny').Value
  else
    WVBrowser1.DefaultURL := 'about:blank';

  WVBrowser1.Navigate(WVBrowser1.DefaultURL);
end;

end.

