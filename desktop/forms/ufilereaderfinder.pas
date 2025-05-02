unit uFileReaderFinder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  uJupiterForm, jupiterStringUtils, jupiterformutils, JupiterConsts,
  jupiterformcomponenttils;

type

  { TFFileReaderFinder }

  TFFileReaderFinder = class(TFJupiterForm)
    pnLeft: TPanel;
    Splitter1: TSplitter;
    tvFile: TTreeView;
    procedure Splitter1Moved(Sender: TObject);
  private
    procedure Internal_OnCheckBoxClick(Sender: TObject);

    procedure Internal_UpdateComponents; override;

    procedure Internal_PrepareForm; override;
    procedure Internal_BuildForm;
  public

  end;

var
  FFileReaderFinder: TFFileReaderFinder;

implementation

{$R *.lfm}

{ TFFileReaderFinder }

procedure TFFileReaderFinder.Splitter1Moved(Sender: TObject);
begin
  miLookColumn.Checked := False;
end;

procedure TFFileReaderFinder.Internal_OnCheckBoxClick(Sender: TObject);
begin
  //
end;

procedure TFFileReaderFinder.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  Self.Caption := 'Pesquisa em arquivos: ' + jupiterStringUtilsGetLastPathName(Self.Params.VariableById('path').Value);

  if miLookColumn.Checked then
    pnLeft.Width := PercentOfScreen(Self.Width, Self.PercentDivisor);
end;

procedure TFFileReaderFinder.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.ShowSearchBar := True;

  Self.Hint := Self.Params.VariableById('path').Value;

  Self.Internal_BuildForm;
end;

procedure TFFileReaderFinder.Internal_BuildForm;
var
  vrCurrentLine : Integer;
  vrReference : TJupiterComponentReference;
begin
  vrCurrentLine := FORM_MARGIN_TOP;

  // Tópicos de pesquisa
  vrReference := JupiterComponentsNewLabel('Tópicos de pesquisa', TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), pnLeft);
  vrCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;

  vrReference := JupiterComponentsNewEdit(EmptyStr, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), pnLeft);
  vrCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;

  vrReference := JupiterComponentsNewEdit(EmptyStr, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), pnLeft);
  vrCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;

  vrReference := JupiterComponentsNewEdit(EmptyStr, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), pnLeft);

  vrCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM_TONEXT;

  // Extensões
  vrReference := JupiterComponentsNewLabel('Extensões', TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), pnLeft);
  vrCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM;

  vrReference := JupiterComponentsNewEdit(EmptyStr, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), pnLeft);

  vrCurrentLine := vrReference.Bottom + FORM_MARGIN_BOTTOM_TONEXT;

  vrReference := JupiterComponentsNewCheckBox('Coincidir tôpicos de pesquisa na mesma linha', False, TJupiterPosition.Create(vrCurrentLine, FORM_MARGIN_LEFT), pnLeft, @Internal_OnCheckBoxClick);
end;

end.

