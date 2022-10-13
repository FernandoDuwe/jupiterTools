unit JupiterForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ComCtrls, StdCtrls, Menus, JupiterRoute, JupiterConsts, JupiterVariable,
  JupiterAction;

type

  { TFJupiterForm }

  TFJupiterForm = class(TForm)
    imgInfo: TImage;
    lbHelp: TLabel;
    miViewParams: TMenuItem;
    miRefresh: TMenuItem;
    miEditGenerator: TMenuItem;
    pnHint: TPanel;
    ppMenu: TPopupMenu;
    sbActions: TScrollBox;
    sbStatus: TStatusBar;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miFormParamsClick(Sender: TObject);
    procedure miRefreshClick(Sender: TObject);
    procedure miViewParamsClick(Sender: TObject);
  private
    FActions  : TJupiterActionList;
    FParams   : TJupiterVariableList;
    FRoute    : TJupiterRoute;
    FHint     : String;
    FPrepared : Boolean;

    procedure Internal_SetHint(prHint : String);
  published
    property Actions  : TJupiterActionList   read FActions  write FActions;
    property Hint     : String               read FHint     write Internal_SetHint;
    property Prepared : Boolean              read FPrepared write FPrepared;
    property Params   : TJupiterVariableList read FParams   write FParams;
    property Route    : TJupiterRoute        read FRoute    write FRoute;

  protected
    procedure Internal_UpdateComponents; virtual;
    procedure Internal_UpdateDatasets; virtual;
    procedure Internal_UpdateCalcs; virtual;

    procedure Internal_PrepareForm; virtual;

  public
    procedure PrepareForm; virtual;
    procedure UpdateForm; virtual;
  end;

var
  FJupiterForm: TFJupiterForm;

implementation

uses JupiterDialogForm;

{$R *.lfm}

{ TFJupiterForm }

procedure TFJupiterForm.FormActivate(Sender: TObject);
begin
  Self.UpdateForm;
end;

procedure TFJupiterForm.FormCreate(Sender: TObject);
begin
  Self.FPrepared := False;

  Self.FActions := TJupiterActionList.Create;
  Self.FParams  := TJupiterVariableList.Create;
  Self.FRoute   := TJupiterRoute.Create(ROOT_PATH);
end;

procedure TFJupiterForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Self.FActions);
  FreeAndNil(Self.FParams);
  FreeAndNil(Self.FRoute);
end;

procedure TFJupiterForm.FormShow(Sender: TObject);
begin
  try
    Self.PrepareForm;
  finally
    Self.UpdateForm;
  end;
end;

procedure TFJupiterForm.miFormParamsClick(Sender: TObject);
begin
end;

procedure TFJupiterForm.miRefreshClick(Sender: TObject);
begin
  Self.UpdateForm;
end;

procedure TFJupiterForm.miViewParamsClick(Sender: TObject);
var
  vrDialog : TJupiterDialogForm;
begin
  vrDialog := TJupiterDialogForm.Create;
  try
    vrDialog.Title    := 'Parâmetros de ' + Self.Caption;
    vrDialog.Hint     := 'Parâmetros utilizados por esse formulário. Esses parâmetros são controlados pelo Jupiter.';
    vrDialog.OnlyShow := True;

    vrDialog.Fields.CopyValues(Self.Params);

    vrDialog.Show;
  finally
    FreeAndNil(vrDialog);
  end;
end;

procedure TFJupiterForm.Internal_SetHint(prHint: String);
begin
  Self.FHint := prHint;

  lbHelp.Caption := prHint;
  pnHint.Visible := Trim(Self.FHint) <> EmptyStr;

  if pnHint.Visible then
  begin
    pnHint.Height := 66;
    lb
  end;

  Self.UpdateForm;
end;

procedure TFJupiterForm.Internal_UpdateComponents;
begin
  sbActions.Visible := Self.Actions.Size > 0;

  miEditGenerator.Enabled := Self.Params.Exists('Generator.FormId');
  miViewParams.Enabled    := Self.Params.Size > 0;
end;

procedure TFJupiterForm.Internal_UpdateDatasets;
begin
  //
end;

procedure TFJupiterForm.Internal_UpdateCalcs;
begin
  //
end;

procedure TFJupiterForm.Internal_PrepareForm;
begin
  //
end;

procedure TFJupiterForm.PrepareForm;
begin
  if Self.Prepared then
    Exit;

  try
    sbStatus.Visible := Self.BorderStyle <> bsNone;

    Self.Internal_PrepareForm;
  finally
    Self.Actions.BuildActions(sbActions);

    Self.Prepared := True;
  end;
end;

procedure TFJupiterForm.UpdateForm;
begin
  Self.Internal_UpdateDatasets;
  Self.Internal_UpdateComponents;
  Self.Internal_UpdateCalcs;
end;

end.

