unit uGenerator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  DBGrids, uJupiterForm, JupiterApp, jupiterformutils, JupiterConsts,
  JupiterModule, JupiterRoute, uJupiterDesktopAppScript, uJupiterAction,
  jupiterDesktopApp, SQLDB, DB, uJupiterAppScript;

type

  { TFGenerator }

  TFGenerator = class(TFJupiterForm)
    lvTables: TListView;
    lvIcons: TListView;
    mmDetails: TMemo;
    mmProviderDetails: TMemo;
    mmTriggerDetails: TMemo;
    pcOptions: TPageControl;
    tsTriggers: TTabSheet;
    tsProviders: TTabSheet;
    tbTables: TTabSheet;
    tsGenerator: TTabSheet;
    tsIcons: TTabSheet;
    procedure lvTablesDblClick(Sender: TObject);
    procedure Internal_OnAboutClick(Sender: TObject);
  private
    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_PrepareForm; override;
  public

  end;

var
  FGenerator: TFGenerator;

implementation

{$R *.lfm}

{ TFGenerator }

procedure TFGenerator.lvTablesDblClick(Sender: TObject);
begin
  if not Assigned(lvTables.Selected) then
    Exit;

  JupiterAppDesktopOpenGridFromTable(TrimRight(lvTables.Selected.Caption));
end;

procedure TFGenerator.Internal_OnAboutClick(Sender: TObject);
begin
  JupiterAppScript_RunMacroById('menu.about.click');
end;

procedure TFGenerator.Internal_UpdateComponents;
begin
  Self.Hint := 'Você pode alterar todo o sistema a partir do módulo Generator. Crie novas rotas, altere as funções do sistema e adicione novas tabelas.';

  inherited Internal_UpdateComponents;
end;

procedure TFGenerator.Internal_UpdateDatasets;
var
  vrListItem : TListItem;
  vrVez : Integer;
  vrTables : TStrings;
begin
  inherited Internal_UpdateDatasets;

  lvIcons.Items.Clear;

  for vrVez := 0 to TJupiterDesktopApp(vrJupiterApp).ImageList.Count - 1 do
  begin
    vrListItem := lvIcons.Items.Add;
    vrListItem.Caption := IntToStr(vrVez);
    vrListItem.ImageIndex := vrVez;
    vrListItem.StateIndex := NULL_KEY;
  end;

  lvTables.Items.Clear;

  vrTables := TStringList.Create;
  try
    with vrJupiterApp.NewWizard do
    begin
      Connection.GetTableNames(vrTables);

      for vrVez := 0 to vrTables.Count - 1 do
      begin
        vrListItem := lvTables.Items.Add;
        vrListItem.Caption := vrTables[vrVez] + '   ';
        vrListItem.ImageIndex := ICON_GRID;
        vrListItem.StateIndex := ICON_GRID;
      end;
    end;
  finally
    FreeAndNil(vrTables);
  end;
end;

procedure TFGenerator.Internal_PrepareForm;
var
  vrVez : Integer;
  vrPrefix : String;
begin
  inherited Internal_PrepareForm;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Sobre', 'Sobre o Jupiter', ICON_WIZARD, @Internal_OnAboutClick));

  Self.ShowSearchBar := False;

  vrPrefix := '      ';

  lvIcons.LargeImages := TJupiterDesktopApp(vrJupiterApp).ImageList;
  lvIcons.SmallImages := TJupiterDesktopApp(vrJupiterApp).ImageList;

  lvTables.LargeImages := TJupiterDesktopApp(vrJupiterApp).ImageList;
  lvTables.SmallImages := TJupiterDesktopApp(vrJupiterApp).ImageList;

  mmDetails.Lines.Clear;
  mmDetails.Lines.Add(EmptyStr);
  mmDetails.Lines.Add('Aplicação');
  mmDetails.Lines.Add(vrPrefix + vrJupiterApp.AppID + ' - ' + vrJupiterApp.AppName);
  mmDetails.Lines.Add(EmptyStr);
  mmDetails.Lines.Add('Módulos:');

  for vrVez := 0 to vrJupiterApp.ModulesList.Count - 1 do
    with vrJupiterApp.ModulesList.GetModuleByIndex(vrVez) do
      mmDetails.Lines.Add(vrPrefix + ModuleID + ' - ' + ModuleTitle);

  mmDetails.Lines.Add(EmptyStr);
  mmDetails.Lines.Add('Rotas:');

  for vrVez := 0 to TJupiterDesktopApp(vrJupiterApp).FormRoutes.Count - 1 do
    with TJupiterFormRoute(TJupiterDesktopApp(vrJupiterApp).FormRoutes.GetAtIndex(vrVez)) do
      mmDetails.Lines.Add(vrPrefix + FormClass.ClassName + ' - ' + DestinyPath);

  mmProviderDetails.Lines.Clear;
  mmProviderDetails.Lines.Add(EmptyStr);
  mmProviderDetails.Lines.Add('Providers');
  mmProviderDetails.Lines.Add(vrPrefix + 'Os providers são utilizados para buscar informações, desde informações relativas a estrutura de arquivos e pastas, ou informações oríundas da base de dados interna.');
  mmProviderDetails.Lines.Add(EmptyStr);
  mmProviderDetails.Lines.Add(vrPrefix + 'JupiterCSVDataProvider: Provider para leitura de arquivos CSV');
  mmProviderDetails.Lines.Add(vrPrefix + vrPrefix + 'Os campos disponíveis são os mesmos campos presentes no arquivo');
  mmProviderDetails.Lines.Add(EmptyStr);
  mmProviderDetails.Lines.Add(vrPrefix + 'JupiterSQLDataProvider: Provider para consultas em banco de dados');
  mmProviderDetails.Lines.Add(vrPrefix + vrPrefix + 'Os campos disponíveis são os mesmos campos presentes na consulta');
  mmProviderDetails.Lines.Add(EmptyStr);
  mmProviderDetails.Lines.Add(vrPrefix + 'JupiterFileDataProvider: Provider para leitura de arquivos nas pastas do sistema operacional');
  mmProviderDetails.Lines.Add(vrPrefix + vrPrefix + 'Campos: FieldName, File, Extension');
  mmProviderDetails.Lines.Add(EmptyStr);
  mmProviderDetails.Lines.Add(vrPrefix + 'JupiterDirectoryDataProvider: Provider para leitura de pastas do sistema operacional');
  mmProviderDetails.Lines.Add(vrPrefix + vrPrefix + 'Campos: Folder, Path');
  mmProviderDetails.Lines.Add(EmptyStr);
  mmProviderDetails.Lines.Add(vrPrefix + 'JupiterTasksDataProvider: Provider para leitura das tarefas cadastradas na pasta de tarefas');
  mmProviderDetails.Lines.Add(vrPrefix + vrPrefix + 'Campos: Client, Task, Path');
  mmProviderDetails.Lines.Add(EmptyStr);
  mmProviderDetails.Lines.Add(vrPrefix + 'JupiterXMLDataProvider: Provider para leitura de arquivos XML');
  mmProviderDetails.Lines.Add(vrPrefix + vrPrefix + 'Os campos disponíveis são os mesmos campos presentes no arquivo');
  mmProviderDetails.Lines.Add(EmptyStr);

  mmTriggerDetails.Lines.Clear;
  mmTriggerDetails.Lines.Add(EmptyStr);
  mmTriggerDetails.Lines.Add('Triggers de sistema');
  mmTriggerDetails.Lines.Add(vrPrefix + TRIGGER_ONEXECUTE);
  mmTriggerDetails.Lines.Add(vrPrefix + TRIGGER_ONLOADDYNAMICDATA);
  mmTriggerDetails.Lines.Add(vrPrefix + TRIGGER_ONPROMPT);
  mmTriggerDetails.Lines.Add(vrPrefix + TRIGGER_ONSHOWPARAMS);
  mmTriggerDetails.Lines.Add(vrPrefix + TRIGGER_ONSTART);
  mmTriggerDetails.Lines.Add(vrPrefix + TRIGGER_ONCHECKLISTCHANGE);
  mmTriggerDetails.Lines.Add(vrPrefix + TRIGGER_ONUPDATE + ' (Informar o nome da macro a ser executada nas configurações do sistema)');

  mmTriggerDetails.Lines.Add(EmptyStr);
  mmTriggerDetails.Lines.Add('Triggers de tabela');
  mmTriggerDetails.Lines.Add(vrPrefix + TRIGGER_DATABASE_AFTERPOST + ', onde {0} é o nome da tabela');
end;

end.

