unit ucurrentTaskPlugin;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, NppDockingForms,
  NppPlugin, Messages, LMessages, LCLIntf, LCLType, ComCtrls, ExtCtrls,
  StdCtrls;

type

  { TFCurrentTaskPlugin }

  TFCurrentTaskPlugin = class(TNppDockingForm)
    Button2: TButton;
    Button3: TButton;
    ilIconFamily: TImageList;
    Label1: TLabel;
    lbCurrentTask: TLabel;
    lvTimes: TListView;
    Panel2: TPanel;
    Panel3: TPanel;
    pnTimes: TPanel;
    pcTabs: TPageControl;
    pnTimes1: TPanel;
    pnTimes2: TPanel;
    TabSheet1: TTabSheet;
    tvFile: TTreeView;
    tsFiles: TTabSheet;
    tsTimes: TTabSheet;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormDock(Sender: TObject);
    procedure FormFloat(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvFileDblClick(Sender: TObject);
  private
    FTaskPath : String;

    procedure Internal_SetTaskPath(prNewPath : String);
    procedure Internal_Update;

    procedure Internal_ListFiles(prTreeNode : TTreeNode; prDirectory : String);
    procedure Internal_ListDirectories(prTreeNode : TTreeNode; prDirectory : String);
  published
    property TaskPath : String read FTaskPath write Internal_SetTaskPath;
  end;

var
  FCurrentTaskPlugin: TFCurrentTaskPlugin;

implementation

uses jupiterformutils, JupiterRunnable, jupiterfiledataprovider, JupiterEnviroment,
     jupiterdirectorydataprovider;

{$R *.lfm}

{ TFCurrentTaskPlugin }

procedure TFCurrentTaskPlugin.FormCreate(Sender: TObject);
begin
  Self.NppDefaultDockingMask := DWS_DF_FLOATING; // whats the default docking position
  Self.KeyPreview            := True; // special hack for input forms
  Self.OnFloat               := Self.FormFloat;
  Self.OnDock                := Self.FormDock;
end;

procedure TFCurrentTaskPlugin.Button2Click(Sender: TObject);
begin
  TJupiterRunnable.Create(Self.TaskPath, True);
end;

procedure TFCurrentTaskPlugin.FormHide(Sender: TObject);
begin
  SendMessage(Self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, Self.CmdID, 0);
end;

procedure TFCurrentTaskPlugin.FormDock(Sender: TObject);
begin
  SendMessage(Self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, Self.CmdID, 1);
end;

procedure TFCurrentTaskPlugin.FormFloat(Sender: TObject);
begin
  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 1);
end;

procedure TFCurrentTaskPlugin.FormResize(Sender: TObject);
begin
  lvTimes.Column[0].Width := PercentOfScreen(lvTimes.Width, 50);
  lvTimes.Column[1].Width := PercentOfScreen(lvTimes.Width, 49);
end;

procedure TFCurrentTaskPlugin.FormShow(Sender: TObject);
begin
  inherited;

  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 1);

  FormResize(Sender);
end;

procedure TFCurrentTaskPlugin.tvFileDblClick(Sender: TObject);
begin
  if not Assigned(tvFile.Selected) then
    Exit;

  if not Assigned(tvFile.Selected.Data) then
    Exit;

  TJupiterRunnable(tvFile.Selected.Data).Execute;
end;

procedure TFCurrentTaskPlugin.Internal_SetTaskPath(prNewPath: String);
begin
  Self.FTaskPath := prNewPath;

  lbCurrentTask.Caption := prNewPath;

  Self.Internal_Update;
end;

procedure TFCurrentTaskPlugin.Internal_Update;
begin
  tvFile.Items.Clear;

  Self.Internal_ListFiles(nil, Self.FTaskPath);
end;

procedure TFCurrentTaskPlugin.Internal_ListFiles(prTreeNode: TTreeNode; prDirectory: String);
var
  vrFile       : TJupiterFileDataProvider;
  vrVez        : Integer;
  vrNode       : TTreeNode;
  vrEnviroment : TJupiterEnviroment;
begin
  Self.Internal_ListDirectories(prTreeNode, prDirectory);

  vrEnviroment := TJupiterEnviroment.Create;
  vrFile       := TJupiterFileDataProvider.Create;
  try
    vrFile.Path := prDirectory;
    vrFile.ProvideData;

    for vrVez := 0 to vrFile.Size - 1 do
      with vrFile.GetRowByIndex(vrVez) do
      begin
        if Assigned(prTreeNode) then
          vrNode := tvFile.Items.AddChild(prTreeNode, Fields.VariableById('FieldName').Value)
        else
          vrNode := tvFile.Items.Add(nil, Fields.VariableById('FieldName').Value);

        vrNode.ImageIndex    := vrEnviroment.IconOfFile(Fields.VariableById('File').Value);
        vrNode.SelectedIndex := vrEnviroment.IconOfFile(Fields.VariableById('File').Value);

        vrNode.Data := TJupiterRunnable.Create(Fields.VariableById('File').Value);
      end;
  finally
    FreeAndNil(vrFile);
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TFCurrentTaskPlugin.Internal_ListDirectories(prTreeNode: TTreeNode; prDirectory: String);
var
  vrProvider   : TJupiterDirectoryDataProvider;
  vrNode       : TTreeNode;
  vrVez        : Integer;
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  vrProvider   := TJupiterDirectoryDataProvider.Create;
  try
    vrProvider.SubFolders := False;

    vrProvider.Path := prDirectory;

    vrProvider.ProvideData;

    for vrVez := 0 to vrProvider.Size - 1 do
      with vrProvider.GetRowByIndex(vrVez) do
      begin
        if Assigned(prTreeNode) then
          vrNode := tvFile.Items.AddChild(prTreeNode, Fields.VariableById('Folder').Value)
        else
          vrNode := tvFile.Items.Add(nil, Fields.VariableById('Folder').Value);

        vrNode.ImageIndex    := vrEnviroment.IconOfFile(Fields.VariableById('Path').Value);
        vrNode.SelectedIndex := vrEnviroment.IconOfFile(Fields.VariableById('Path').Value);

        vrNode.Data := TJupiterRunnable.Create(Fields.VariableById('Path').Value);

        Self.Internal_ListFiles(vrNode, Fields.VariableById('Path').Value);
      end;
  finally
    FreeAndNil(vrProvider);
    FreeAndNil(vrEnviroment);
  end;
end;

end.

