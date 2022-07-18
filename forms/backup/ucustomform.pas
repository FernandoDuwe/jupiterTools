unit uCustomForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  uJupiterForm, JupiterCustomAction, JupiterApp, fileUtils, jupiterScript,
  StdCtrls, Menus;

type

  { TFCustomForm }

  TFCustomForm = class(TJupiterForm)
    imgInfo: TImage;
    lbTitle: TLabel;
    lbInfo: TLabel;
    lbHint: TLabel;
    pnHint: TPanel;
    pnTaskBar: TPanel;
    pnInfo: TPanel;
    ScrollBox1: TScrollBox;
    sbOk: TSpeedButton;
    sbSeparator: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pnInfoClick(Sender: TObject);
    procedure sbOkClick(Sender: TObject);
  private
    FCustomAction : TJupiterCustomAction;

    procedure Internal_CreateForm();
    procedure Internal_CreateActions();
    procedure Internal_BtnClick(Sender: TObject);

    procedure Internal_UpdateComponents; override;
  published
    property CustomAction : TJupiterCustomAction read FCustomAction write FCustomAction;
  public

  end;

var
  FCustomForm: TFCustomForm;

implementation

uses uMain, process;

{$R *.lfm}

{ TFCustomForm }

procedure TFCustomForm.FormCreate(Sender: TObject);
begin
  Self.FCustomAction := TJupiterCustomAction.Create;
end;

procedure TFCustomForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Self.FCustomAction);
end;

procedure TFCustomForm.FormShow(Sender: TObject);
begin
  Self.Caption := Self.CustomAction.Title;

  vrJupiterApp.Config.AddVariable(vrJupiterApp.AppName + '.Variables.CurrentPath', TratarCaminho(ExtractFileDir(Self.CustomAction.FileName)), 'Diretório atual');
  vrJupiterApp.Config.AddVariable(vrJupiterApp.AppName + '.Variables.CurrentFile', Self.CustomAction.FileName, 'Arquivo atual');

  Self.Internal_CreateForm();
  Self.Internal_CreateActions();
end;

procedure TFCustomForm.pnInfoClick(Sender: TObject);
begin

end;

procedure TFCustomForm.sbOkClick(Sender: TObject);
var
  vrVez   : Integer;
  vrEdt   : TEdit;
  vrField : TJupiterCustomActionField;
begin
  try
    for vrVez := 0 to Self.ComponentCount - 1 do
    begin
      if not (Self.Components[vrVez] is TEdit) then
        Continue;

      vrEdt   := TEdit(Self.Components[vrVez]);
      vrField := Self.CustomAction.FieldByIndex(vrEdt.Tag);

      vrJupiterApp.Config.AddConfig(vrField.ConfigID, vrEdt.Text, vrField.Hint);
    end;
  finally
    pnHint.Visible := True;
  end;
end;

procedure TFCustomForm.Internal_CreateForm;
var
  vrVez : Integer;
  vrLbl : TLabel;
  vrEdt : TEdit;
  vrTop : Integer;
begin
  vrTop := 16;

  sbOk.Enabled := Self.CustomAction.FieldCount > 0;

  for vrVez := 0 to Self.CustomAction.FieldCount - 1 do
  begin
    vrLbl         := TLabel.Create(Self);
    vrLbl.Parent  := ScrollBox1;
    vrLbl.Top     := vrTop;
    vrLbl.Left    := 16;
    vrLbl.Caption := Self.CustomAction.FieldByIndex(vrVez).Title;

    vrTop := vrTop + vrLbl.Height + 4;

    vrEdt          := TEdit.Create(Self);
    vrEdt.Parent   := ScrollBox1;
    vrEdt.Top      := vrTop;
    vrEdt.Left     := 16;
    vrEdt.Hint     := Self.CustomAction.FieldByIndex(vrVez).Hint;
    vrEdt.ShowHint := True;
    vrEdt.Width    := ScrollBox1.Width - 40;
    vrEdt.Anchors  := [akTop, akLeft, akRight];
    vrEdt.TabOrder := vrVez + 1;

    if vrJupiterApp.Config.Exists(Self.CustomAction.FieldByIndex(vrVez).ConfigID) then
      vrEdt.Caption := vrJupiterApp.Config.GetByID(Self.CustomAction.FieldByIndex(vrVez).ConfigID).Value
    else
      vrEdt.Caption := EmptyStr;

    vrEdt.Tag := vrVez;

    vrTop := vrTop + vrEdt.Height + 22;
  end;
end;

procedure TFCustomForm.Internal_CreateActions;
var
  vrBtn  : TSpeedButton;
  vrLeft : Integer;
  vrVez  : Integer;
begin
  vrLeft := 8;
  vrLeft := vrLeft + (sbOk.Width) + 8;

  for vrVez := 0 to Self.CustomAction.ActionCount -1 do
  begin
    vrBtn            := TSpeedButton.Create(pnTaskBar);
    vrBtn.Parent     := pnTaskBar;
    vrBtn.Top        := 8;
    vrBtn.Left       := vrLeft;
    vrBtn.Height     := 33;
    vrBtn.Width      := 144;
    vrBtn.Caption    := Self.CustomAction.ActionByIndex(vrVez).Title;
    vrBtn.ImageIndex := Self.CustomAction.ActionByIndex(vrVez).ImageIndex;
    vrBtn.Images     := FMain.ilMainIcons;
    vrBtn.Hint       := Self.CustomAction.ActionByIndex(vrVez).Hint;
    vrBtn.ShowHint   := True;
    vrBtn.Tag        := vrVez;
    vrBtn.AutoSize   := False;
    vrBtn.OnClick    := @Internal_BtnClick;

    vrLeft := vrLeft + (vrBtn.Width) + 8;
  end;
end;

procedure TFCustomForm.Internal_BtnClick(Sender: TObject);
var
  vrFile   : String;
  vrScript : TJupiterScript;
  vrOutput : String;
begin
  sbOk.Click;

  vrFile :=  TratarCaminho(Self.CustomAction.ActionByIndex(TSpeedButton(Sender).Tag).ScriptFile);

  if AnsiUpperCase(ExtractFileExt(vrFile)) = '.BAT' then
  begin
    vrScript := TJupiterScript.Create;
    try
      vrScript.FileName := vrFile;
      vrScript.ResolveAndSave(TratarCaminho(ExtractFileDir(Application.ExeName) + '/temp/temp.bat'));

      RunCommand(vrJupiterApp.Config.GetByID('JupiterTools.Modules.Runner.ExecutorBATScript').Value, [TratarCaminho(ExtractFileDir(Application.ExeName) + '/temp/temp.bat')], vrOutput, [poRunIdle]);

      vrJupiterApp.Log.AddLog(Now, Self.Caption, 'Executado externamente: ' + ExtractFileName(vrFile));
    finally
      FreeAndNil(vrScript);
    end;
  end
  else
  begin
    if DirectoryExists(vrFile) then
    begin
      vrJupiterApp.Log.AddLog(Now, 'Runner', 'Abrindo pasta: ' + vrFile);

      OpenFolder(vrFile);
    end
    else
    begin
      vrJupiterApp.Log.AddLog(Now, 'Runner', 'Abrindo executável: ' + vrFile);

      OpenFile(vrFile);
    end;
  end;
end;

procedure TFCustomForm.Internal_UpdateComponents;
begin
  lbTitle.Caption   := Self.CustomAction.Title;
  lbHint.Caption    := Self.CustomAction.Hint;

  inherited Internal_UpdateComponents;
end;

end.

