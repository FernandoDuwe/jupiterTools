unit uFileExplorer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ShellCtrls, ExtCtrls,
  uJupiterForm, JupiterConsts, jupiterformutils, JupiterEnviroment,
  uJupiterRunnableScript, uJupiterAction;

type

  { TFFileExplorer }

  TFFileExplorer = class(TFJupiterForm)
    slvExporer: TShellListView;
    spDivider: TSplitter;
    stvFolders: TShellTreeView;
    procedure slvExporerDblClick(Sender: TObject);
  private
    procedure Internal_UpdateComponents; override;

    procedure Internal_PrepareForm; override;

    procedure Internal_OnOpenFolder(Sender: TObject);
    procedure Internal_OnAsReport(Sender: TObject);
    procedure Internal_OnAsList(Sender: TObject);
    procedure Internal_OnAsIcons(Sender: TObject);
    procedure Internal_OnAsSmallIcons(Sender: TObject);
  public

  end;

var
  FFileExplorer: TFFileExplorer;

implementation

uses ComCtrls;

{$R *.lfm}

{ TFFileExplorer }

procedure TFFileExplorer.slvExporerDblClick(Sender: TObject);
begin
  if not Assigned(slvExporer.Selected) then
    Exit;

  JupiterRunnableScript_RunCommand(slvExporer.Root + slvExporer.Selected.Caption);
end;

procedure TFFileExplorer.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  stvFolders.Width := PercentOfScreen(Self.Width, Self.PercentDivisor);

  Self.Caption := slvExporer.Root;
end;

procedure TFFileExplorer.Internal_PrepareForm;
var
  vrEnviroment : TJupiterEnviroment;
begin
  inherited Internal_PrepareForm;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Abrir pasta', 'Clique aqui para abrir a pasta atual externamente', ICON_OPEN, @Internal_OnOpenFolder));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Relatório', 'Exibir itens como relatório', NULL_KEY, @Internal_OnAsReport));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Lista', 'Exibir itens como lista', NULL_KEY, @Internal_OnAsList));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Ícones', 'Exibir itens como ícones', NULL_KEY, @Internal_OnAsIcons));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Ícones pequenos', 'Exibir itens como ícones pequenos', NULL_KEY, @Internal_OnAsSmallIcons));

  vrEnviroment := TJupiterEnviroment.Create;
  try
    if not Self.Params.Exists('path') then
      Self.Params.AddVariable('path', vrEnviroment.BasePath, 'Endereço');

    slvExporer.Root := Self.Params.VariableById('path').Value;
    stvFolders.Root := Self.Params.VariableById('path').Value;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFFileExplorer.Internal_OnOpenFolder(Sender: TObject);
begin
  JupiterRunnableScript_RunCommand(Self.Params.VariableById('path').Value);
end;

procedure TFFileExplorer.Internal_OnAsReport(Sender: TObject);
begin
  slvExporer.ViewStyle := vsReport;
end;

procedure TFFileExplorer.Internal_OnAsList(Sender: TObject);
begin
  slvExporer.ViewStyle := vsList;
end;

procedure TFFileExplorer.Internal_OnAsIcons(Sender: TObject);
begin
  slvExporer.ViewStyle := vsIcon;
end;

procedure TFFileExplorer.Internal_OnAsSmallIcons(Sender: TObject);
begin
  slvExporer.ViewStyle := vsSmallIcon;
end;

end.

