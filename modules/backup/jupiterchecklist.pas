unit jupiterchecklist;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ComCtrls, Forms, JupiterModule, JupiterApp, JupiterConsts, fileUtils,
  jupiterUtils, SysUtils, process, Dialogs;

type

  { TJupiterActionChecklistNewFileCheckList }

  TJupiterActionChecklistNewFileCheckList = class(TJupiterAction)
  public
    constructor Create;

    procedure Run(prParams : TJupiterListem); override;
  end;

  { TJupiterActionChecklistNewItem }

  TJupiterActionChecklistNewItem = class(TJupiterAction)
  public
    constructor Create;

    procedure Run(prParams : TJupiterListem); override;
  end;

  { TJupiterActionChecklistMarkAll }

  TJupiterActionChecklistMarkAll = class(TJupiterAction)
  public
    constructor Create;

    procedure Run(prParams : TJupiterListem); override;
  end;

  { TJupiterActionChecklistUnMarkAll }

  TJupiterActionChecklistUnMarkAll = class(TJupiterAction)
  public
    constructor Create;

    procedure Run(prParams : TJupiterListem); override;
  end;

  { TJupiterChecklist }

  TJupiterChecklist = class(TJupiterModule)
  protected
    procedure Internal_Initialize; override;
    function  Internal_GetIdentifier: String; override;

    procedure Internal_ListChecklists(var prTreeMenu : TTreeView; prOwner : TTreeNode);
    function  Internal_ChecklistCompleted(prFileName : String) : Boolean;
  public
    procedure ListItems(var prParams : TJupiterListem; var prList : TList); override;
    procedure ListActions(prParams : TJupiterListem; var prList : TList); override;

    procedure GetTasks(var prTreeMenu : TTreeView); override;

    procedure RunListable(var prParamsItem: TJupiterListem; var prParams : TJupiterListableItem); override;
  end;


implementation

uses LCLIntf;

{ TJupiterActionChecklistNewFileCheckList }

constructor TJupiterActionChecklistNewFileCheckList.Create;
begin
  Self.Title      := 'Nova Checklist';
  Self.Hint       := 'Clique aqui para criar um novo arquivo de checklist';
  Self.ImageIndex := ICON_CHECKLIST;
end;

procedure TJupiterActionChecklistNewFileCheckList.Run(prParams: TJupiterListem);
var
  vrFile : String;
begin
  InputQuery('Nova Checklist', 'Informe o nome da nova checklist (sem extensão)', vrFile);

  if Trim(vrFile) = EmptyStr then
    Exit;

  NovaChecklist(vrFile);

  vrJupiterApp.Log.AddLog(Now, Self.ClassName, 'Criada checklist: ' + vrFile);

  inherited Run(prParams);
end;

{ TJupiterActionChecklistMarkAll }

constructor TJupiterActionChecklistMarkAll.Create;
begin
  Self.Title      := 'Marcar todos';
  Self.Hint       := 'Clique aqui para marcar todas as opções';
  Self.ImageIndex := ICON_CHECKED;
end;

procedure TJupiterActionChecklistMarkAll.Run(prParams: TJupiterListem);
var
  vrStr    : TStrings;
  vrVez    : Integer;
  vrSubStr : TStrings;
begin
  inherited Run(prParams);

  vrStr := TStringList.Create;
  vrSubStr := TStringList.Create;
  try
    vrStr.LoadFromFile(prParams.Params);

    for vrVez := 1 to vrStr.Count - 1 do
    begin
      vrSubStr.Delimiter := ';';
      vrSubStr.DelimitedText := StringReplace(vrStr[vrVez], ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]);

      vrStr[vrVez] := Format('%0:s;1;', [StringReplace(vrSubStr[0], EMPTY_SPACE_SEPARATOR, ' ', [rfReplaceAll, rfIgnoreCase]), 0]);
    end;

    vrStr.SaveToFile(prParams.Params);

    vrJupiterApp.Log.AddLog(Now, 'Checklist', 'Limpando checklist: ' + ExtractFileName(prParams.Params));
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

{ TJupiterActionChecklistUnMarkAll }

constructor TJupiterActionChecklistUnMarkAll.Create;
begin
  Self.Title      := 'Desmarcar todos';
  Self.Hint       := 'Clique aqui para desmarcar todas as opções';
  Self.ImageIndex := NULL_KEY;
end;

procedure TJupiterActionChecklistUnMarkAll.Run(prParams: TJupiterListem);
var
  vrStr    : TStrings;
  vrVez    : Integer;
  vrSubStr : TStrings;
begin
  inherited Run(prParams);

  vrStr := TStringList.Create;
  vrSubStr := TStringList.Create;
  try
    vrStr.LoadFromFile(prParams.Params);

    for vrVez := 1 to vrStr.Count - 1 do
    begin
      vrSubStr.Delimiter := ';';
      vrSubStr.DelimitedText := StringReplace(vrStr[vrVez], ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]);

      vrStr[vrVez] := Format('%0:s;0;', [StringReplace(vrSubStr[0], EMPTY_SPACE_SEPARATOR, ' ', [rfReplaceAll, rfIgnoreCase]), 0]);
    end;

    vrStr.SaveToFile(prParams.Params);

    vrJupiterApp.Log.AddLog(Now, 'Checklist', 'Limpando checklist: ' + ExtractFileName(prParams.Params));
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

{ TJupiterActionChecklistNewItem }

constructor TJupiterActionChecklistNewItem.Create;
begin
  Self.Title      := 'Novo item';
  Self.Hint       := 'Clique aqui para adicionar um novo item a checklist';
  Self.ImageIndex := ICON_ADD;
end;

procedure TJupiterActionChecklistNewItem.Run(prParams: TJupiterListem);
var
  vrFile : String;
  vrStr  : TStrings;
begin
  InputQuery('Novo item em Checklist', 'Informe o passo a ser cumprido:', vrFile);

  if Trim(vrFile) = EmptyStr then
    Exit;

  vrStr := TStringList.Create;
  try
    vrStr.LoadFromFile(prParams.Params);

    vrStr.Add(Format('%0:s;;', [vrFile]));

    vrStr.SaveToFile(prParams.Params);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;

  inherited Run(prParams);
end;

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
      vrNode.Data := TJupiterListem.Create(Self.ID, '/checklistFile', vrStr[vrVez], 0, 'Dê um duplo clique para marcar/desmarcar a opção selecionada.');
    end;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;

end;

function TJupiterChecklist.Internal_ChecklistCompleted(prFileName: String): Boolean;
var
  vrVez : Integer;
  vrStr : TStrings;
begin
  Result := True;

  vrStr := TStringList.Create;
  try
    vrStr.LoadFromFile(prFileName);

    for vrVez := 1 to vrStr.Count - 1 do
      if StrToIntDef(GetCSVColumn(vrStr[vrVez] , 1), 0) = 0 then
      begin
        Result := False;
        Exit;
      end;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterChecklist.ListItems(var prParams: TJupiterListem;
  var prList: TList);
var
  vrObj    : TJupiterListableItem;
  vrStr    : TStrings;
  vrSubStr : TStrings;
  vrVez    : Integer;
begin
  inherited ListItems(prParams, prList);

  if prParams.Task = '/' then
  begin
    Self.JupiterApp.Config.AddVariable(Self.JupiterApp.AppName + '.Variables.CurrentPath', TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/checklist/'), 'Diretório atual');
    Self.JupiterApp.Config.AddVariable(Self.JupiterApp.AppName + '.Variables.CurrentFile', EmptyStr, 'Arquivo atual');

    vrObj := TJupiterListableItem.Create();
    vrObj.Descricao  := TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/checklist/');
    vrObj.Item       := 'Pasta de checklists';
    vrObj.Param      := TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/checklist/');
    vrObj.ImageIndex := ICON_FOLDER;
    vrObj.Tag        := 0;

    prList.Add(vrObj);
  end;

  if prParams.Task = '/checklistFile' then
  begin
    Self.JupiterApp.Config.AddVariable(Self.AppName + '.Variables.CurrentPath', TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/checklist/'), 'Diretório atual');
    Self.JupiterApp.Config.AddVariable(Self.AppName + '.Variables.CurrentFile', prParams.Params, 'Arquivo atual');

    vrStr := TStringList.Create;
    vrSubStr := TStringList.Create;
    try
      vrStr.LoadFromFile(prParams.Params);

      for vrVez := 1 to vrStr.Count - 1 do
      begin
        vrSubStr.Delimiter := ';';
        vrSubStr.DelimitedText := StringReplace(vrStr[vrVez], ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]);

        vrObj := TJupiterListableItem.Create();
        vrObj.Item        := StringReplace(vrSubStr[0], EMPTY_SPACE_SEPARATOR, ' ', [rfReplaceAll, rfIgnoreCase]);
        vrObj.Descricao   := EmptyStr;
        vrObj.Param       := IntToStr(vrVez);
        vrObj.Selecionado := StrToIntDef(StringReplace(vrSubStr[1], EMPTY_SPACE_SEPARATOR, ' ', [rfReplaceAll, rfIgnoreCase]), 0) = 1;
        vrObj.Tag         := 1;

        vrObj.SubItens.Add(prParams.Params);

        prList.Add(vrObj);
      end;

      if Self.Internal_ChecklistCompleted(prParams.Params) then
      begin
        prParams.HintType := htSuccess;
        prParams.Hint     := 'Checklist Completa!';
      end
      else
      begin
        prParams.HintType := htNone;
        prParams.Hint     := 'Dê um duplo clique para marcar/desmarcar a opção selecionada.';
      end;
    finally
      vrSubStr.Clear;
      FreeAndNil(vrSubStr);

      vrStr.Clear;
      FreeAndNil(vrStr);
    end;
  end;
end;

procedure TJupiterChecklist.ListActions(prParams: TJupiterListem; var prList: TList);
begin
  if prParams.Task = '/' then
    prList.Add(TJupiterActionChecklistNewFileCheckList.Create);

  if prParams.Task = '/checklistFile' then
  begin
    prList.Add(TJupiterActionChecklistNewItem.Create);

    prList.Add(TJupiterActionChecklistMarkAll.Create);

    prList.Add(TJupiterActionChecklistUnMarkAll.Create);
  end;

  inherited ListActions(prParams, prList);
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

  vrNode.Data := TJupiterListem.Create(Self.ID, '/', EmptyStr, 0, 'Listando agora o diretório de checklists.');

  Self.Internal_ListChecklists(prTreeMenu, vrNode);

  vrNode.Expanded := True;
end;

procedure TJupiterChecklist.RunListable(var prParamsItem: TJupiterListem; var prParams: TJupiterListableItem);
var
  vrStr    : TStrings;
  vrVez    : Integer;
  vrSubStr : TStrings;
begin
  inherited RunListable(prParamsItem, prParams);

  if prParams.Tag = 0 then
  begin
    OpenFolder(prParams.Param);

    Exit;
  end;

  if prParams.Tag = 1 then
  begin
    prParams.Selecionado := not prParams.Selecionado;

    vrStr := TStringList.Create;
    try
      vrStr.LoadFromFile(prParams.SubItens[0]);

      if prParams.Selecionado then
        vrStr[StrToIntDef(prParams.Param, 999)] := Format('%0:s;%1:d;', [prParams.Item, 1])
      else
        vrStr[StrToIntDef(prParams.Param, 999)] := Format('%0:s;%1:d;', [prParams.Item, 0]);

      vrStr.SaveToFile(prParams.SubItens[0]);
    finally
      vrStr.Clear;
      FreeAndNil(vrStr);
    end;
  end;

  if prParams.Tag = 2 then
  begin
    vrStr := TStringList.Create;
    vrSubStr := TStringList.Create;
    try
      vrStr.LoadFromFile(prParams.Param);

      for vrVez := 1 to vrStr.Count - 1 do
      begin
        vrSubStr.Delimiter := ';';
        vrSubStr.DelimitedText := StringReplace(vrStr[vrVez], ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]);

        vrStr[vrVez] := Format('%0:s;0;', [StringReplace(vrSubStr[0], EMPTY_SPACE_SEPARATOR, ' ', [rfReplaceAll, rfIgnoreCase]), 0]);
      end;

      vrStr.SaveToFile(prParams.Param);

      vrJupiterApp.Log.AddLog(Now, 'Checklist', 'Limpando checklist: ' + ExtractFileName(prParams.Param));
    finally
      vrStr.Clear;
      FreeAndNil(vrStr);
    end;
  end;
end;

end.

