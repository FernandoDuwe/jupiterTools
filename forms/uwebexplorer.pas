unit uwebexplorer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, uWVBrowser,
  uWVWindowParent, IpHtml, Iphttpbroker, JupiterForm;

type

  { TFWebExplorer }

  TFWebExplorer = class(TFJupiterForm)
    WVBrowser1: TWVBrowser;
    WVWindowParent1: TWVWindowParent;
  private
  protected
    procedure Internal_PrepareForm; override;
  public

  end;

var
  FWebExplorer: TFWebExplorer;

implementation

{$R *.lfm}

{ TFWebExplorer }

procedure TFWebExplorer.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  if Self.Params.Exists('destiny') then
    WVBrowser1.DefaultURL := Self.Params.VariableById('destiny').Value
  else
    WVBrowser1.DefaultURL := 'about:blank';

  WVBrowser1.CreateBrowser(WVWindowParent1.Handle);

  WVBrowser1.Navigate(WVBrowser1.DefaultURL);
end;

end.

