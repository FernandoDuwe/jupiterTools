unit uwebexplorer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  JupiterEnviroment, JupiterForm;

type

  { TFWebExplorer }

  TFWebExplorer = class(TFJupiterForm)
    Timer1: TTimer;
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

{$R *.lfm}

{ TFWebExplorer }

procedure TFWebExplorer.WVBrowser1AfterCreated(Sender: TObject);
begin
end;

procedure TFWebExplorer.FormShow(Sender: TObject);
begin
  inherited;
end;

procedure TFWebExplorer.FormCreate(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
begin
  inherited;

  vrEnviroment := TJupiterEnviroment.Create;
  try
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFWebExplorer.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
end;

procedure TFWebExplorer.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;
end;

procedure TFWebExplorer.UpdateForm(prUpdateDatasets: Boolean; prUpdateComponentes: Boolean; prUpdateCalcs: Boolean);
begin
  inherited UpdateForm(prUpdateDatasets, prUpdateComponentes, prUpdateCalcs);
end;

initialization

end.

