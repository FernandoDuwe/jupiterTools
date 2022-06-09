unit jupiterchecklist;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ComCtrls, Forms, JupiterModule, JupiterApp, JupiterConsts, fileUtils,
  SysUtils, process;

type

  { TJupiterChecklist }

  TJupiterChecklist = class(TJupiterModule)
  protected
    procedure Internal_Initialize; override;
    function  Internal_GetIdentifier: String; override;

    procedure Internal_ListChecklists(var prTreeMenu : TTreeView; prOwner : TTreeNode);
  public
    procedure ListItems(prParams : TJupiterListem; var prList : TList); override;

    procedure GetTasks(var prTreeMenu : TTreeView); override;

    procedure RunListable(prParams : TJupiterListableItem); override;
  end;


implementation

uses LCLIntf;

{ TJupiterChecklist }

procedure TJupiterChecklist.Internal_Initialize;
begin
  inherited Internal_Initialize;

  if not DirectoryExists(TratarCaminho(ExtractFileDir(Application.ExeName) + '/modules/checklist/')) then
     CreateDir(TratarCaminho(ExtractFileDir(Application.ExeName) + '/modules/checklist/'));
end;

function TJupiterChecklist.Internal_GetIdentifier: String;
begin
  Result := 'JupiterTools.Modules.Checklist';
end;

procedure TJupiterChecklist.Internal_ListChecklists(var prTreeMenu: TTreeView;
  prOwner: TTreeNode);
var
  vrStr  : TStrings;
  vrVez  : Integer;
  vrNode : TTreeNode;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;

    ListFiles(TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/checklist/'), vrStr);

    for vrVez := 0 to vrStr.Count - 1 do
    begin
      if (AnsiUpperCase(ExtractFileExt(vrStr[vrVez])) <> '.CKL') then
        Continue;

      vrNode := prTreeMenu.Items.AddChild(prOwner, ExtractFileName(vrStr[vrVez]));
      vrNode.ImageIndex    := ICON_CHECKLIST;
      vrNode.SelectedIndex := ICON_CHECKLIST;
      vrNode.Data := TJupiterListem.Create(Self.ID, EmptyStr, vrStr[vrVez]);
    end;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;

end;

procedure TJupiterChecklist.ListItems(prParams: TJupiterListem;
  var prList: TList);
var
  vrObj : TJupiterListableItem;
begin
  inherited ListItems(prParams, prList);

  vrObj := TJupiterListableItem.Create();
  vrObj.Item      := TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/checklist/');
  vrObj.Descricao := 'Pasta de checklists';
  vrObj.Param     := TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/checklist/');

  prList.Add(vrObj);
end;

procedure TJupiterChecklist.GetTasks(var prTreeMenu: TTreeView);
var
  vrNode : TTreeNode;
  vrSubNode : TTreeNode;
begin
  inherited GetTasks(prTreeMenu);

  vrNode               := prTreeMenu.Items.Add(nil, 'Checklists');
  vrNode.ImageIndex    := ICON_CHECKLIST;
  vrNode.SelectedIndex := ICON_CHECKLIST;

  vrNode.Data := TJupiterListem.Create(Self.ID, '/', EmptyStr, 0, 'Listando agora o diretório de checklists. Dê um duplo clique para abrir.');

  Self.Internal_ListScripts(prTreeMenu, vrNode);

  vrNode.Expanded := True;
end;

procedure TJupiterChecklist.RunListable(prParams: TJupiterListableItem);
begin
  inherited RunListable(prParams);

  OpenFolder(prParams.Param);
end;

end.

