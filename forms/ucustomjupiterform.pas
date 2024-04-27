unit uCustomJupiterForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  JupiterForm, JupiterAction, JupiterConsts, JupiterRunnable,
  JupiterFormGenerator, jupiterformutils, JupiterApp, JupiterStandardModule;

type

  { TFCustomJupiterForm }

  TFCustomJupiterForm = class(TFJupiterForm)
    pnBody: TPanel;
    sbBody: TScrollBox;
    tmrUpdateAutoComponents: TTimer;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrUpdateAutoComponentsStartTimer(Sender: TObject);
    procedure tmrUpdateAutoComponentsTimer(Sender: TObject);
  protected
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

    procedure UpdateForm(prUpdateDatasets : Boolean = True; prUpdateComponentes : Boolean = True; prUpdateCalcs : Boolean = True); override;
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
  Self.FFormGenerator.Form := Self;
  Self.FFormGenerator.Container := sbBody;
end;

procedure TFCustomJupiterForm.FormActivate(Sender: TObject);
begin
  inherited;

  tmrUpdateAutoComponents.Enabled := Self.Showing and Self.FFormGenerator.HasAutoValueVariables;
end;

procedure TFCustomJupiterForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  tmrUpdateAutoComponents.Enabled := False;
end;

procedure TFCustomJupiterForm.FormDeactivate(Sender: TObject);
begin
  tmrUpdateAutoComponents.Enabled := False;
end;

procedure TFCustomJupiterForm.FormDestroy(Sender: TObject);
begin
  tmrUpdateAutoComponents.Enabled := False;

  FreeAndNil(FFormGenerator);

  inherited;
end;

procedure TFCustomJupiterForm.tmrUpdateAutoComponentsStartTimer(Sender: TObject
  );
begin

end;

procedure TFCustomJupiterForm.tmrUpdateAutoComponentsTimer(Sender: TObject);
begin
  Self.FFormGenerator.UpdateForm;

  tmrUpdateAutoComponents.Enabled := Self.Showing and Self.FFormGenerator.HasAutoValueVariables;
end;

procedure TFCustomJupiterForm.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  if Assigned(vrJupiterApp) then
  begin
    Self.FormGenerator.ActionsInForm := TJupiterStandardModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Standard')).ShowActionsInForm;

    sbActions.Visible := not TJupiterStandardModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Standard')).ShowActionsInForm;
  end;
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
  if Self.Params.Exists('UpdateTimerIntervalTime') then
    tmrUpdateAutoComponents.Interval := StrToInt(Self.Params.VariableById('UpdateTimerIntervalTime').Value)
  else
    Self.Params.AddVariable('UpdateTimerIntervalTime', IntToStr(tmrUpdateAutoComponents.Interval), 'Intervalo de tempo em que os campos calculados são atualizados em tela');

  inherited PrepareForm;

  Self.FFormGenerator.OnKeyUp := Self.OnKeyUp;

  sbBody.Visible := False;
  try
    Self.FFormGenerator.Variables.CopyFromVariableList(Self.Generator.Fields);

    if Assigned(vrJupiterApp) then
      if (((TJupiterStandardModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Standard')).ShowActionsInForm) and (Params.Exists(FIELD_ID_GENERADOR))) and (not Self.DontShowActionInForm)) then
         Self.FormGenerator.ActionList := Self.Actions;

    if Self.Params.Exists('ShowActionsInForm') then
    begin
      Self.FormGenerator.ActionsInForm := Self.Params.VariableById('ShowActionsInForm').Value = '1';

      if Self.FormGenerator.ActionsInForm then
        Self.FormGenerator.ActionList := Self.Actions;
    end;

    if ((Assigned(Self.FFormGenerator.Variables)) and (Self.FFormGenerator.Variables.Size > 0)) then
      Self.FFormGenerator.DrawForm;

    DrawForm(Self);
  finally
    tmrUpdateAutoComponents.Enabled := Self.Showing and Self.FFormGenerator.HasAutoValueVariables;

    sbBody.Visible := True;
  end;
end;

procedure TFCustomJupiterForm.Search(prSearch: String);
begin
  inherited Search(prSearch);

  DrawFormInSearch(Self, prSearch, $005151FF);
end;

procedure TFCustomJupiterForm.UpdateForm(prUpdateDatasets: Boolean; prUpdateComponentes: Boolean; prUpdateCalcs: Boolean);
begin
  tmrUpdateAutoComponents.Enabled := False;
  try
    if prUpdateComponentes then
      Self.FFormGenerator.UpdateForm;

    inherited UpdateForm(prUpdateDatasets, prUpdateComponentes, prUpdateCalcs);
  finally
    tmrUpdateAutoComponents.Enabled := Self.Showing and Self.FFormGenerator.HasAutoValueVariables;
  end;
end;

end.

