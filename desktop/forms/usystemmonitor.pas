unit uSystemMonitor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, uJupiterForm,
  jupiterformutils, JupiterApp, jupiterScript, JupiterDataProvider;

type

  { TFSystemMonitor }

  TFSystemMonitor = class(TFJupiterForm)
    lvScripts: TListView;
    lvDataProviders: TListView;
    pcPages: TPageControl;
    tsDataProviders: TTabSheet;
    tsScripts: TTabSheet;
  private
    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
  public

  end;

var
  FSystemMonitor: TFSystemMonitor;

implementation

{$R *.lfm}

{ TFSystemMonitor }

procedure TFSystemMonitor.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  lvScripts.Column[0].Width := PercentOfScreen(lvScripts.Width, 50);
  lvDataProviders.Column[0].Width := PercentOfScreen(lvDataProviders.Width, 50);
end;

procedure TFSystemMonitor.Internal_UpdateDatasets;
var
  vrVez : Integer;
  vrItem : TListItem;
begin
  inherited Internal_UpdateDatasets;

  lvScripts.Items.Clear;
  lvDataProviders.Items.Clear;
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
  finally
  end;
end;

end.

