unit uUniqueFileExplorer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, PairSplitter,
  ShellCtrls, uJupiterForm, JupiterConsts, uJupiterAction;

type

  { TFUniqueFileExplorer }

  TFUniqueFileExplorer = class(TFJupiterForm)
    procedure PageControl1Change(Sender: TObject);
  private
    procedure Internal_PrepareForm; override;
  public
    procedure Internal_OnOpenFolder(Sender: TObject);
  end;

var
  FUniqueFileExplorer: TFUniqueFileExplorer;

implementation

{$R *.lfm}

{ TFUniqueFileExplorer }

procedure TFUniqueFileExplorer.PageControl1Change(Sender: TObject);
begin

end;

procedure TFUniqueFileExplorer.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Abrir pasta', 'Clique aqui para abrir a pasta atual externamente', ICON_OPEN, @Internal_OnOpenFolder));
end;

procedure TFUniqueFileExplorer.Internal_OnOpenFolder(Sender: TObject);
begin

end;

end.

