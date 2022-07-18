unit uCurrentTask;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons, uJupiterForm, JupiterApp, JupiterModule, fileUtils,
  JupiterConsts, JupiterTasks;

type

  { TFCurrentTask }

  TFCurrentTask = class(TJupiterForm)
    btFolder: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbCurrentTask: TLabel;
    lvFiles: TListView;
    lvTimes: TListView;
    pnTaskBar: TPanel;
    pnTimes: TPanel;
    pnFiles: TPanel;
    pnBody: TPanel;
    pnHeader: TPanel;
    sbEndTime: TSpeedButton;
    sbStartTime: TSpeedButton;
    Splitter1: TSplitter;
    procedure btFolderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvFilesDblClick(Sender: TObject);
    procedure sbEndTimeClick(Sender: TObject);
    procedure sbStartTimeClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FStarted : Boolean;

    procedure Internal_ListTimes;

    procedure Internal_UpdateComponents; override;
    procedure Internal_UpdateDatasets; override;

    function CurrentTask : TJupiterTaskDetails;
  public

  end;

var
  FCurrentTask: TFCurrentTask;

implementation

uses uMain;

{$R *.lfm}

{ TFCurrentTask }

procedure TFCurrentTask.btFolderClick(Sender: TObject);
begin
  vrJupiterApp.Log.AddLog(Now, Self.Caption, 'Abrindo tarefa atual: ' + vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.Current').Value);

  OpenFolder(vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.Current').Value);
end;

procedure TFCurrentTask.FormCreate(Sender: TObject);
begin
  Self.FStarted := False;
end;

procedure TFCurrentTask.FormShow(Sender: TObject);
begin

end;

procedure TFCurrentTask.lvFilesDblClick(Sender: TObject);
begin
  if not Assigned(lvFiles.Selected) then
    Exit;

  if not Assigned(lvFiles.Selected.Data) then
    Exit;

  vrJupiterApp.Log.AddLog(Now, Self.Caption, 'Abrindo arquivo: ' + TJupiterListableItem(lvFiles.Selected.Data).Param);

  Self.CurrentTask.ExecuteFile(TJupiterListableItem(lvFiles.Selected.Data).Param);
end;

procedure TFCurrentTask.sbEndTimeClick(Sender: TObject);
begin
  try
    Self.CurrentTask.TimeNote.Finish;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFCurrentTask.sbStartTimeClick(Sender: TObject);
begin
  try
    Self.CurrentTask.TimeNote.Start;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFCurrentTask.SpeedButton1Click(Sender: TObject);
begin

end;

procedure TFCurrentTask.Internal_ListTimes;
var
  vrVez  : Integer;
  vrList : TList;
  vrStr  : TStrings;
  vrItem : TListItem;
begin
  lvTimes.Items.Clear;

  vrList := TList.Create;
  vrStr  := TStringList.Create;
  try
    vrStr.Clear;
    vrList.Clear;

    Self.CurrentTask.TimeNote.GetTimes(vrList);

    Self.FStarted := False;

    for vrVez := 0 to vrList.Count - 1 do
    begin
       vrStr.Clear;
       vrStr.Delimiter     := ';';
       vrStr.DelimitedText := TJupiterListableItem(vrList[vrVez]).SubItens[0];

       vrItem := lvTimes.Items.Add;

       vrItem.Caption := Format('%0:s %1:s', [vrStr[1], vrStr[2]]);

       Self.FStarted := True;

       if TJupiterListableItem(vrList[vrVez]).SubItens.Count > 1 then
       begin
         vrStr.Clear;
         vrStr.Delimiter     := ';';
         vrStr.DelimitedText := TJupiterListableItem(vrList[vrVez]).SubItens[1];

         vrItem.SubItems.Add(Format('%0:s %1:s', [vrStr[1], vrStr[2]]));

         Self.FStarted := False;
       end;
    end;
  finally
    while vrList.Count > 0 do
    begin
      TJupiterListableItem(vrList[0]).Free;

      vrList.Delete(0);
    end;

    vrStr.Clear;
    FreeAndNil(vrStr);

    vrList.Clear;
    FreeAndNil(vrList);
  end;
end;

procedure TFCurrentTask.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  lbCurrentTask.Caption := vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.CurrentNumber').Value;
  btFolder.Enabled := ((vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.Current') <> nil) and
                       (DirectoryExists(vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.Current').Value)));

  sbStartTime.Enabled := not Self.FStarted;
  sbEndTime.Enabled   := Self.FStarted;

  lvFiles.Font.Size := StrToInt(vrJupiterApp.Config.GetByID('JupiterTools.UI.Display.FontSize').Value);
  lvTimes.Font.Size := StrToInt(vrJupiterApp.Config.GetByID('JupiterTools.UI.Display.FontSize').Value);
end;

procedure TFCurrentTask.Internal_UpdateDatasets;
var
  vrNode     : TListItem;
  vrStr      : TStrings;
  vrVez      : Integer;
  vrListable : TJupiterListableItem;
  vrDir      : String;
begin
  inherited Internal_UpdateDatasets;

  lvFiles.Items.Clear;

  vrJupiterApp.Config.AddVariable(vrJupiterApp.AppName + '.Variables.CurrentPath', vrJupiterApp.Config.GetByID('JupiterTools.Modules.Tasks.Current').Value, 'Diretório atual');
  vrJupiterApp.Config.AddVariable(vrJupiterApp.AppName + '.Variables.CurrentFile', EmptyStr, 'Arquivo atual');

  vrStr :=  TStringList.Create;
  try
    vrStr.Clear;

    Self.CurrentTask.ListTaskFiles(vrStr);

    for vrVez := 0 to vrStr.Count - 1 do
    begin
      vrNode         := lvFiles.Items.Add;
      vrNode.Caption := ExtractFileName(vrStr[vrVez]);

      vrDir := StringReplace(ExtractFileDir(vrStr[vrVez]) + GetDirectorySeparator, Self.CurrentTask.Path, EmptyStr, [rfIgnoreCase, rfReplaceAll]);

      if vrDir = EmptyStr then
        vrDir := '.';

      vrNode.SubItems.Add(vrDir);

      vrListable       := TJupiterListableItem.Create;
      vrListable.Param := vrStr[vrVez];

      vrNode.Data := vrListable;
    end;

    Self.Internal_ListTimes;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

function TFCurrentTask.CurrentTask: TJupiterTaskDetails;
begin
  Result := TJupiterTasks(vrJupiterApp.GetModuleByID('JupiterTools.Modules.Tasks')).CreateTaskDetail;
end;

end.

