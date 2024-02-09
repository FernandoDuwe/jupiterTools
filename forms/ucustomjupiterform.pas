unit uCustomJupiterForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, JupiterForm,
  JupiterAction, JupiterConsts, JupiterRunnable, JupiterFormGenerator,
  jupiterformutils, JupiterApp, JupiterStandardModule;

type

  { TFCustomJupiterForm }

  TFCustomJupiterForm = class(TFJupiterForm)
    sbBody: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDontShowActionInForm : Boolean;

    FFormGenerator : TJupiterFormGenerator;
  published
    property DontShowActionInForm : Boolean read FDontShowActionInForm write FDontShowActionInForm;

    property FormGenerator : TJupiterFormGenerator read FFormGenerator write FFormGenerator;
  protected
    procedure Internal_PrepareForm; override;

    procedure Internal_SaveGeneratorClick(Sender : TObject); override;
  public
    procedure PrepareForm; override;
    procedure Search(prSearch : String); override;
  end;

var
  FCustomJupiterForm: TFCustomJupiterForm;

implementation

{$R *.lfm}

{ TFCustomJupiterForm }

procedure TFCustomJupiterForm.FormCreate(Sender: TObject);
begin
  inherited;

  Self.DontShowActionInForm := False;

  Self.FFormGenerator := TJupiterFormGenerator.Create;
  Self.FFormGenerator.Container := sbBody;
end;

procedure TFCustomJupiterForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFormGenerator);

  inherited;
end;

procedure TFCustomJupiterForm.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.FormGenerator.ActionsInForm := TJupiterStandardModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Standard')).ShowActionsInForm;
  sbActions.Visible := not TJupiterStandardModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Standard')).ShowActionsInForm;
end;

procedure TFCustomJupiterForm.Internal_SaveGeneratorClick(Sender: TObject);
begin
  inherited Internal_SaveGeneratorClick(Sender);

  if Assigned(Self.FormGenerator.Variables) then
  begin
    Self.FormGenerator.Variables.Validate;
    Self.FormGenerator.Variables.Save;
  end;
end;

procedure TFCustomJupiterForm.PrepareForm;
begin
  inherited PrepareForm;

  Self.FFormGenerator.OnKeyUp := Self.OnKeyUp;

  sbBody.Visible := False;
  try
    DrawForm(Self);

    Self.FFormGenerator.Variables.CopyFromVariableList(Self.Generator.Fields);

    if (((TJupiterStandardModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Standard')).ShowActionsInForm) and (Params.Exists(FIELD_ID_GENERADOR))) and (not Self.DontShowActionInForm)) then
       Self.FormGenerator.ActionList := Self.Actions;

    if ((Assigned(Self.FFormGenerator.Variables)) and (Self.FFormGenerator.Variables.Size > 0)) then
      Self.FFormGenerator.DrawForm;
  finally
    sbBody.Visible := True;
  end;
end;

procedure TFCustomJupiterForm.Search(prSearch: String);
begin
  inherited Search(prSearch);

  DrawFormInSearch(Self, prSearch, $005151FF);
end;

end.

