unit jupiterGenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ComCtrls, JupiterConsts, JupiterModule, JupiterCustomAction,
  fileUtils, uGeneratorActionEditor, SysUtils, Forms;

type
  { TJupiterActionGeneratorNewAction }

  TJupiterActionGeneratorNewAction = class(TJupiterAction)
  public
    constructor Create;

    procedure Run(prParams : TJupiterListem); override;
  end;

  { TJupiterGenerator }

  TJupiterGenerator = class(TJupiterModule)
  protected
    procedure Internal_Initialize; override;
    function  Internal_GetIdentifier: String; override;

    procedure Internal_ListActions(var prTreeMenu : TTreeView; prOwner : TTreeNode);
  public
    procedure ListItems(var prParams : TJupiterListem; var prList : TList); override;

    procedure GetTasks(var prTreeMenu : TTreeView); override;

    procedure ListActions(prParams : TJupiterListem; var prList : TList); override;

    procedure RunListable(var prParamsItem: TJupiterListem; var prParams : TJupiterListableItem); override;
  end;

implementation

uses Dialogs, JupiterApp;

{ TJupiterActionGeneratorNewAction }

constructor TJupiterActionGeneratorNewAction.Create;
begin
  Self.Title      := 'Nova Ação';
  Self.Hint       := 'Clique aqui para criar uma nova ação';
  Self.ImageIndex := ICON_FORM;
end;

procedure TJupiterActionGeneratorNewAction.Run(prParams: TJupiterListem);
var
  vrFile : String;
  vrObj  : TJupiterCustomAction;
begin
  InputQuery('Nova Ação', 'Informe o nome da Ação', vrFile);

  if Trim(vrFile) = EmptyStr then
    Exit;

  vrObj := TJupiterCustomAction.Create;
  try
    vrObj.FileName := TratarCaminho(ExtractFileDir(Application.ExeName) + '/modules/generator/' + vrFile + '.jgf');

    vrObj.Title      := vrFile;
    vrObj.Hint       := EmptyStr;
    vrObj.ImageIndex := ICON_FORM;

    vrJupiterApp.Log.AddLog(Now, Self.ClassName, 'Criada ação: ' + vrObj.FileName);

    vrObj.SaveToFile;
  finally
    FreeAndNil(vrObj);
  end;

  inherited Run(prParams);
end;

{ TJupiterGenerator }

procedure TJupiterGenerator.Internal_Initialize;
begin
  inherited Internal_Initialize;

  if not DirectoryExists(TratarCaminho(ExtractFileDir(Application.ExeName) + '/modules/generator/')) then
     CreateDir(TratarCaminho(ExtractFileDir(Application.ExeName) + '/modules/generator/'));
end;

function TJupiterGenerator.Internal_GetIdentifier: String;
begin
  Result := 'JupiterTools.Modules.Generator';
end;

procedure TJupiterGenerator.Internal_ListActions(var prTreeMenu: TTreeView; prOwner: TTreeNode);
var
  vrStr  : TStrings;
  vrVez  : Integer;
  vrNode : TTreeNode;
  vrObj  : TJupiterCustomAction;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    ListFiles(TratarCaminho(ExtractFileDir(Application.ExeName) + '/modules/generator/'), vrStr);

    for vrVez := 0 to vrStr.Count - 1 do
    begin
      if (AnsiUpperCase(ExtractFileExt(vrStr[vrVez])) <> '.JGF') then
        Continue;

      vrObj := TJupiterCustomAction.Create;
      try
        vrObj.FileName := vrStr[vrVez];
        vrObj.LoadFromFile;

        vrNode               := prTreeMenu.Items.AddChild(prOwner, vrObj.Title);
        vrNode.ImageIndex    := vrObj.ImageIndex;
        vrNode.SelectedIndex := vrObj.ImageIndex;
        vrNode.Data := TJupiterListem.Create(Self.ID, '/action', vrStr[vrVez], 30, vrObj.Hint);
      finally
        FreeAndNil(vrObj);
      end;
    end;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterGenerator.ListItems(var prParams: TJupiterListem; var prList: TList);
var
  vrStr  : TStrings;
  vrVez  : Integer;
  vrObj  : TJupiterCustomAction;
  vrItem : TJupiterListableItem;
begin
  if prParams.Task = '/' then
  begin
    Self.JupiterApp.Config.AddVariable(Self.JupiterApp.AppName + '.Variables.CurrentPath', TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/generator/'), 'Diretório atual');
    Self.JupiterApp.Config.AddVariable(Self.JupiterApp.AppName + '.Variables.CurrentFile', prParams.Params, 'Arquivo atual');

    vrStr := TStringList.Create;
    try
      vrStr.Clear;
      ListFiles(TratarCaminho(ExtractFileDir(Application.ExeName) + '/modules/generator/'), vrStr);

      for vrVez := 0 to vrStr.Count - 1 do
      begin
        if (AnsiUpperCase(ExtractFileExt(vrStr[vrVez])) <> '.JGF') then
          Continue;

        vrObj := TJupiterCustomAction.Create;
        try
          vrObj.FileName := vrStr[vrVez];
          vrObj.LoadFromFile;

          vrItem := TJupiterListableItem.Create();
          vrItem.Descricao  := vrObj.Hint;
          vrItem.Item       := vrObj.Title;
          vrItem.Param      := vrStr[vrVez];
          vrItem.ImageIndex := vrObj.ImageIndex;
          vrItem.Tag        := 1;

          prList.Add(vrItem);
        finally
          FreeAndNil(vrObj);
        end;
      end;
    finally
      vrStr.Clear;
      FreeAndNil(vrStr);
    end;
  end;

  inherited ListItems(prParams, prList);
end;

procedure TJupiterGenerator.GetTasks(var prTreeMenu: TTreeView);
var
  vrNode : TTreeNode;
begin
  inherited GetTasks(prTreeMenu);

  vrNode               := prTreeMenu.Items.Add(nil, 'Generator');
  vrNode.ImageIndex    := ICON_WIZARD;
  vrNode.SelectedIndex := ICON_WIZARD;

  vrNode.Data := TJupiterListem.Create(Self.ID, '/', EmptyStr, 0, 'Use o Generator para criar novas funcionalidades.');

  Self.Internal_ListActions(prTreeMenu, vrNode);

  vrNode.Expand(True);
end;

procedure TJupiterGenerator.ListActions(prParams: TJupiterListem; var prList: TList);
begin
  prList.Add(TJupiterActionGeneratorNewAction.Create);

  inherited ListActions(prParams, prList);
end;

procedure TJupiterGenerator.RunListable(var prParamsItem: TJupiterListem; var prParams: TJupiterListableItem);
begin
  inherited RunListable(prParamsItem, prParams);

  if prParams.Tag = 1 then
  begin
    Application.CreateForm(TFGeneratorActionEditor, FGeneratorActionEditor);
    try
      FGeneratorActionEditor.CustomAction.FileName := prParams.Param;
      FGeneratorActionEditor.ShowModal;
    finally
      FGeneratorActionEditor.Release;
      FreeAndNil(FGeneratorActionEditor);
    end;
  end;
end;

end.

