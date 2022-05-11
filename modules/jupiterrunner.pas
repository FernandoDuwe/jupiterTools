unit jupiterRunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, Forms, JupiterModule, JupiterApp, JupiterConsts, SysUtils;

type

  { TJupiterRunner }

  TJupiterRunner = class(TJupiterModule)
  protected
    procedure Internal_Initialize; override;
    function  Internal_GetIdentifier: String; override;
  public
    procedure GetTasks(var prTreeMenu : TTreeView); override;
  end;

implementation

{ TJupiterRunner }

procedure TJupiterRunner.Internal_Initialize;
begin
  inherited Internal_Initialize;

  if not DirectoryExists(ExtractFileDir(Application.ExeName) + '/modules/runner/') then
     CreateDir(ExtractFileDir(Application.ExeName) + '/modules/runner/');
end;

function TJupiterRunner.Internal_GetIdentifier: String;
begin
  Result := 'JupiterTools.Modules.Runner';
end;

procedure TJupiterRunner.GetTasks(var prTreeMenu: TTreeView);
var
  vrNode : TTreeNode;
  vrSubNode : TTreeNode;
begin
  inherited GetTasks(prTreeMenu);

  vrNode               := prTreeMenu.Items.Add(nil, 'Scritps');
  vrNode.ImageIndex    := ICON_SCRIPTS;
  vrNode.SelectedIndex := ICON_SCRIPTS;

  vrNode.Data := TJupiterListem.Create(Self.ID, '/');

  vrNode               := prTreeMenu.Items.Add(nil, 'Favoritos');
  vrNode.ImageIndex    := ICON_FAVORITE;
  vrNode.SelectedIndex := ICON_FAVORITE;

  vrNode.Data := TJupiterListem.Create(Self.ID, '/');
end;

end.

