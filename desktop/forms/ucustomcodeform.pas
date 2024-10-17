unit uCustomCodeForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, uJupiterForm,
  JupiterApp, JupiterObject, jupiterformutils, JupiterConsts, jupiterDesktopApp,
  jupiterformcomponenttils;

type

  { TFCustomCodeForm }

  TFCustomCodeForm = class(TFJupiterForm)
    sbBody: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCurrentLine : Integer;
  public
    procedure AddLabel(prLabelCaption : String);
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

procedure TFCustomCodeForm.AddLabel(prLabelCaption: String);
var
  vrReference : TJupiterComponentReference;
begin
  Self.FCurrentLine := Self.FCurrentLine + FORM_MARGIN_TOP;

  vrReference := JupiterComponentsNewLabel(prLabelCaption, TJupiterPosition.Create(Self.FCurrentLine, FORM_MARGIN_LEFT), sbBody);

  Self.FCurrentLine := vrReference.Bottom;
end;

end.

