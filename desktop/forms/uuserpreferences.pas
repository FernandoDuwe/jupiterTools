unit uUserPreferences;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Spin,
  uJupiterForm, JupiterConsts, jupiterformutils, JupiterApp, uJupiterAction,
  jupiterformcomponenttils, StdCtrls;

type

  { TFUserPreferences }

  TFUserPreferences = class(TFJupiterForm)
    sbBody: TScrollBox;
    procedure TrackBar1Change(Sender: TObject);
    procedure CheckBoxChange(Sender: TObject);
  private
     procedure Internal_PrepareForm; override;

     procedure Internal_OnSave(Sender: TObject);
  public

  end;

var
  FUserPreferences: TFUserPreferences;

implementation

{$R *.lfm}

{ TFUserPreferences }

procedure TFUserPreferences.TrackBar1Change(Sender: TObject);
begin
  Self.Params.VariableById(FIELD_FONT_SIZE).Value := IntToStr(TTrackBar(Sender).Position);
end;

procedure TFUserPreferences.CheckBoxChange(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    Self.Params.VariableById(FORM_ALWAYS_MODAL).Value := BOOL_TRUE_STR
  else
    Self.Params.VariableById(FORM_ALWAYS_MODAL).Value := BOOL_FALSE_STR;
end;

procedure TFUserPreferences.Internal_PrepareForm;
var
  vrCurrentLine : Integer;
  vrReference : TJupiterComponentReference;
begin
  Self.Hint := 'Customize o sistema de acordo com suas necessidades, aumente a fonte e controle como o sistema se comportará com você';

  inherited Internal_PrepareForm;

  Self.Params.AddVariable(FORM_ALWAYS_MODAL, vrJupiterApp.Params.VariableById(FORM_ALWAYS_MODAL).Value, FORM_ALWAYS_MODAL);
  Self.Params.AddVariable(FIELD_FONT_SIZE, vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value, FIELD_FONT_SIZE);

  Self.ActionGroup.AddAction(TJupiterAction.Create('Salvar', 'Clique aqui para salvar', ICON_SAVE, @Internal_OnSave));

  vrCurrentLine := FORM_MARGIN_TOP;

  vrReference := JupiterComponentsNewLabel('Tamanho da fonte',
                                           TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody);

  vrCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;

  vrReference := JupiterComponentsNewTrackBar(GetFontSize(), 6, 64, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody, @TrackBar1Change);

  vrCurrentLine := vrReference.Bottom + FORM_MARGIN_TOP + FORM_MARGIN_BOTTOM;

  vrReference := JupiterComponentsNewLabel('Formulários sempre exibidos como "modal"',
                                           TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody);

  vrCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;

  vrReference := JupiterComponentsNewCheckBox('Formulários sempre modais', vrJupiterApp.Params.VariableById(FORM_ALWAYS_MODAL).AsBool, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), sbBody, @CheckBoxChange);
end;

procedure TFUserPreferences.Internal_OnSave(Sender: TObject);
begin
  try
    vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value := Self.Params.VariableById(FIELD_FONT_SIZE).Value;

    vrJupiterApp.Params.VariableById(FORM_ALWAYS_MODAL).Value := Self.Params.VariableById(FORM_ALWAYS_MODAL).Value;
  finally
    Self.DoSecureClose;
  end;
end;

end.

