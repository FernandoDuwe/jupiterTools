unit uJupiterForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, ExtCtrls,
  ButtonPanel;

type

  { TFJupiterForm }

  TFJupiterForm = class(TForm)
    acOptions: TActionList;
    tmrAutoUpdater: TTimer;
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  published
    procedure Internal_UpdateComponents; virtual;
    procedure Internal_UpdateDatasets; virtual;
    procedure Internal_UpdateCalcs; virtual;
    procedure Internal_PrepareForm; virtual;
  public
    procedure PrepareForm; virtual;
    procedure UpdateForm(prUpdateDatasets : Boolean = True; prUpdateComponentes : Boolean = True; prUpdateCalcs : Boolean = True); virtual;
  end;

var
  FJupiterForm: TFJupiterForm;

implementation

{$R *.lfm}

{ TFJupiterForm }

procedure TFJupiterForm.FormShow(Sender: TObject);
begin
  Self.PrepareForm;
end;

procedure TFJupiterForm.FormActivate(Sender: TObject);
begin
  Self.UpdateForm();
end;

procedure TFJupiterForm.Internal_UpdateComponents;
begin

end;

procedure TFJupiterForm.Internal_UpdateDatasets;
begin

end;

procedure TFJupiterForm.Internal_UpdateCalcs;
begin

end;

procedure TFJupiterForm.Internal_PrepareForm;
begin

end;

procedure TFJupiterForm.PrepareForm;
begin
  Self.Internal_PrepareForm;
end;

procedure TFJupiterForm.UpdateForm(prUpdateDatasets: Boolean; prUpdateComponentes: Boolean; prUpdateCalcs: Boolean);
begin
  if prUpdateDatasets then
    Self.Internal_UpdateDatasets;

  if prUpdateComponentes then
    Self.Internal_UpdateComponents;

  if prUpdateCalcs then
    Self.Internal_UpdateCalcs;
end;

end.

