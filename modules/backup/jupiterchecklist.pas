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

    procedure RunListable(var prParams : TJupiterListableItem); override;
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
      vrNode.Data := TJupiterListem.Create(Self.ID, '/checklistFile', vrStr[vrVez], 0, 'Dê um duplo clique para marcar/desmarcar a opção selecionada.');
    end;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;

end;

procedure TJupiterChecklist.ListItems(prParams: TJupiterListem;
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
    vrObj := TJupiterListableItem.Create();
    vrObj.Item       := TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/checklist/');
    vrObj.Descricao  := 'Pasta de checklists';
    vrObj.Param      := TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/checklist/');
    vrObj.ImageIndex := ICON_FOLDER;
    vrObj.Tag        := 0;

    prList.Add(vrObj);

    vrStr := TStringList.Create;
    try
      vrStr.Clear;

      ListFiles(TratarCaminho(ExtractFileDir(Application.ExeName) + GetDirectorySeparator + 'modules/checklist/'), vrStr);

      for vrVez := 0 to vrStr.Count - 1 do
      begin
        if (AnsiUpperCase(ExtractFileExt(vrStr[vrVez])) <> '.CKL') then
          Continue;

        vrObj := TJupiterListableItem.Create();
        vrObj.Item       := 'Limpar checklist: ' + ExtractFileName(vrStr[vrVez]);
        vrObj.Descricao  := 'Dê um duplo clique para limpar o arquivo de checklist';
        vrObj.Param      := vrStr[vrVez];
        vrObj.ImageIndex := ICON_DOCS;
        vrObj.Tag        := 2;

        prList.Add(vrObj);
      end;
    finally
      vrStr.Clear;
      FreeAndNil(vrStr);
    end;
  end;

  if prParams.Task = '/checklistFile' then
  begin
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
    finally
      vrSubStr.Clear;
      FreeAndNil(vrSubStr);

      vrStr.Clear;
      FreeAndNil(vrStr);
    end;
  end;
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

  vrNode.Data := TJupiterListem.Create(Self.ID, '/', EmptyStr, 0, 'Listando agora o diretório de checklists. Também exibindo a opção de limpar cada um dos arquivos de checklists cadastrados.');

  Self.Internal_ListChecklists(prTreeMenu, vrNode);

  vrNode.Expanded := True;
end;

procedure TJupiterChecklist.RunListable(var prParams: TJupiterListableItem);
var
  vrStr    : TStrings;
  vrVez    : Integer;
  vrSubStr : TStrings;
begin
  inherited RunListable(prParams);

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
    finally
      vrStr.Clear;
      FreeAndNil(vrStr);
    end;
  end;
end;

end.

