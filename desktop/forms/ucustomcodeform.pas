unit uCustomCodeForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, uJupiterForm,
  JupiterApp, JupiterObject, jupiterformutils, JupiterConsts, jupiterDesktopApp,
  jupiterformcomponenttils, uJupiterAction;

type

  { TFCustomCodeForm }

  TFCustomCodeForm = class(TFJupiterForm)
    sbBody: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCurrentLine : Integer;

    procedure Internal_OnCheckBoxChange(Sender: TObject);
  public
    procedure AddLabel(prLabelCaption : String);
    procedure AddEdit();
    procedure AddCombBox(prDataProviderID, prColumn, prVariableID : String);
    procedure AddCheckBox(prVariableId, prText : String; prValue : Boolean);
    procedure AddAction(prCaption, prHint : String; prIcon : Integer; prMacroID : String);
  end;

var
  FCustomCodeForm: TFCustomCodeForm;

implementation

{$R *.lfm}

{ TFCustomCodeForm }

procedure TFCustomCodeForm.FormCreate(Sender: TObject);
begin
  Self.FCurrentLine := 0;

  if Assigned(vrJupiterApp) then
    if Assigned(TJupiterDesktopApp(vrJupiterApp).FormList) then
      TJupiterDesktopApp(vrJupiterApp).FormList.AddSimpleObject(Self);

  inherited;
end;

procedure TFCustomCodeForm.FormDestroy(Sender: TObject);
begin
  if Assigned(vrJupiterApp) then
    if Assigned(TJupiterDesktopApp(vrJupiterApp).FormList) then
      TJupiterDesktopApp(vrJupiterApp).DeleteFormById(Self.FormID);

  inherited;
end;

procedure TFCustomCodeForm.Internal_OnCheckBoxChange(Sender: TObject);
begin
  //
end;

procedure TFCustomCodeForm.AddLabel(prLabelCaption: String);
var
  vrReference : TJupiterComponentReference;
begin
  Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

  vrReference := JupiterComponentsNewLabel(prLabelCaption, TJupiterPosition.Create(Self.FCurrentLine, FORM_MARGIN_LEFT), sbBody);

  Self.FCurrentLine := vrReference.Bottom;
end;

procedure TFCustomCodeForm.AddEdit();
var
  vrReference : TJupiterComponentReference;
begin
  Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

  vrReference := JupiterComponentsNewEdit(EmptyStr, TJupiterPosition.Create(Self.FCurrentLine, FORM_MARGIN_LEFT), sbBody);

  Self.FCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;
end;

procedure TFCustomCodeForm.AddCombBox(prDataProviderID, prColumn, prVariableID: String);
var
  vrReference : TJupiterComponentReference;
begin
  Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

  vrReference := JupiterComponentsNewComboBox(prDataProviderID, prColumn, TJupiterPosition.Create(Self.FCurrentLine, FORM_MARGIN_LEFT), sbBody);

  Self.FCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;
end;

procedure TFCustomCodeForm.AddCheckBox(prVariableId, prText: String; prValue: Boolean);
var
  vrReference : TJupiterComponentReference;
begin
  Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

  vrReference := JupiterComponentsNewCheckBox(prText, prValue, TJupiterPosition.Create(Self.FCurrentLine, FORM_MARGIN_LEFT), sbBody, @Internal_OnCheckBoxChange);

  Self.FCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;
end;

procedure TFCustomCodeForm.AddAction(prCaption, prHint : String; prIcon : Integer; prMacroID : String);
begin
  Self.ActionGroup.AddAction(TJupiterAction.Create(prCaption, prHint, prIcon, prMacroID));
end;

end.

