unit uHome;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, ComCtrls, JupiterForm, JupiterApp, JupiterConsts, JupiterRoute,
  JupiterDirectoryDataProvider, JupiterEnviroment, JupiterRunnable,
  JupiterVariable, JupiterFormGenerator, uMain, uConfig, LCLType;

type

  { TFHome }

  TFHome = class(TFJupiterForm)
    Image1: TImage;
    lbVersion: TLabel;
    lbTitle: TLabel;
    pnTitle: TPanel;
    sbBody: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure Label8DblClick(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    procedure lbConfigLink1Click(Sender: TObject);
    procedure lbConfigLink2Click(Sender: TObject);
    procedure lbConfigLinkClick(Sender: TObject);
    procedure lbNewTaskLink1Click(Sender: TObject);
    procedure lbNewTaskLinkClick(Sender: TObject);
    procedure pnTitleClick(Sender: TObject);
    procedure tvFoldersDblClick(Sender: TObject);
  private
    FFormGenerator : TJupiterFormGenerator;

    procedure Internal_PrepareForm; override;
  protected
    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;
    procedure Internal_SearchSubFolders(prFolder : String; prNode : TTreeNode);
  public

  end;

var
  FHome: TFHome;

implementation

{$R *.lfm}

{ TFHome }

procedure TFHome.FormResize(Sender: TObject);
begin
//  pnTitle.Top  := Round((sbBody.Height / 2) - (pnTitle.Height / 2));
//  pnTitle.Left := Round((sbBody.Width / 2) - (pnTitle.Width / 2));
end;

procedure TFHome.FormCreate(Sender: TObject);
begin
  inherited;

  Self.FFormGenerator := TJupiterFormGenerator.Create;
  Self.FFormGenerator.Container := sbBody;
end;

procedure TFHome.FormDestroy(Sender: TObject);
begin
  Self.FFormGenerator.Free;

  inherited;
end;

procedure TFHome.FormShow(Sender: TObject);
begin
  inherited;

  FormResize(Sender);

  Self.FFormGenerator.ActionList := Self.Actions;
  Self.FFormGenerator.Raised := True;
  Self.FFormGenerator.ActionColor := clWhite;
  Self.FFormGenerator.DrawForm;

  pnTitle.Align := alBottom;
  pnTitle.Align := alTop;
end;

procedure TFHome.Label5Click(Sender: TObject);
begin
  FMain.miPastasJupiter.Click;
end;

procedure TFHome.Label6Click(Sender: TObject);
begin

end;

procedure TFHome.Label7Click(Sender: TObject);
begin

end;

procedure TFHome.Label8DblClick(Sender: TObject);
begin

end;

procedure TFHome.Label9Click(Sender: TObject);
begin

end;

procedure TFHome.lbConfigLink1Click(Sender: TObject);
begin
  FMain.tbMessage.Click;
end;

procedure TFHome.lbConfigLink2Click(Sender: TObject);
begin
  FMain.MenuItem8.Click;
end;

procedure TFHome.lbConfigLinkClick(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(CONFIG_PATH), True);
end;

procedure TFHome.lbNewTaskLink1Click(Sender: TObject);
begin
  if FMain.miOpenCurrentTask.Enabled then
    vrJupiterApp.NavigateTo(TJupiterRoute.Create(TASK_FORM_PATH), True)
  else
    ShowMessage('A tarefa atual não foi definida');
end;

procedure TFHome.lbNewTaskLinkClick(Sender: TObject);
begin
  vrJupiterApp.NavigateTo(TJupiterRoute.Create(NEWTASK_FORM_PATH), True);
end;

procedure TFHome.pnTitleClick(Sender: TObject);
begin

end;

procedure TFHome.tvFoldersDblClick(Sender: TObject);
begin             {
  if not Assigned(tvFolders.Selected) then
    Exit;

  if not Assigned(tvFolders.Selected.Data) then
    Exit;

  TJupiterRunnable.Create(TJupiterVariableList(tvFolders.Selected.Data).VariableById('Path').Value, True);
  }
end;

procedure TFHome.Internal_PrepareForm;
begin
  Self.Params.AddVariable(FIELD_ID_GENERADOR, 'HomeForm', 'ID do formulário');
end;

procedure TFHome.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  lbTitle.Caption   := vrJupiterApp.AppName;
  lbVersion.Caption := 'Versão: ' + vrJupiterApp.GetVersion;
end;

procedure TFHome.Internal_UpdateDatasets;
var
  vrProvider   : TJupiterDirectoryDataProvider;
  vrEnviroment : TJupiterEnviroment;
  vrVez        : Integer;
  vrNode       : TTreeNode;
begin
  inherited Internal_UpdateDatasets;
  {
  tvFolders.Items.Clear;

  vrProvider   := TJupiterDirectoryDataProvider.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrProvider.Path := vrEnviroment.BasePath;
    vrProvider.SubFolders := False;
    vrProvider.ProvideData;

    for vrVez := 0 to vrProvider.Count - 1 do
      with vrProvider.GetRowByIndex(vrVez) do
      begin
        vrNode := tvFolders.Items.Add(nil, Fields.VariableById('Folder').Value);
        vrNode.ImageIndex    := ICON_OPEN;
        vrNode.SelectedIndex := ICON_OPEN;
        vrNode.Data := TJupiterVariableList.Create;

        TJupiterVariableList(vrNode.Data).CopyValues(Fields);

        Self.Internal_SearchSubFolders(Fields.VariableById('Path').Value, vrNode);
      end;

    tvFolders.FullExpand;
  finally
    FreeAndNil(vrProvider);
    FreeAndNil(vrEnviroment);
  end;                                     }
end;

procedure TFHome.Internal_SearchSubFolders(prFolder: String; prNode: TTreeNode);
var
  vrProvider   : TJupiterDirectoryDataProvider;
  vrVez        : Integer;
  vrNode       : TTreeNode;
begin
  inherited Internal_UpdateDatasets;
                  {
  vrProvider   := TJupiterDirectoryDataProvider.Create;
  try
    vrProvider.Path := prFolder;
    vrProvider.SubFolders := False;
    vrProvider.ProvideData;

    for vrVez := 0 to vrProvider.Count - 1 do
      with vrProvider.GetRowByIndex(vrVez) do
      begin
        vrNode := tvFolders.Items.AddChild(prNode, Fields.VariableById('Folder').Value);
        vrNode.ImageIndex    := ICON_OPEN;
        vrNode.SelectedIndex := ICON_OPEN;
        vrNode.Data := TJupiterVariableList.Create;

        TJupiterVariableList(vrNode.Data).CopyValues(Fields);

        Self.Internal_SearchSubFolders(Fields.VariableById('Path').Value, vrNode);
      end;
  finally
    FreeAndNil(vrProvider);
  end;             }
end;

end.

