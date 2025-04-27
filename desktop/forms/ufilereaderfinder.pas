unit uFileReaderFinder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, uJupiterForm,
  jupiterStringUtils;

type

  { TFFileReaderFinder }

  TFFileReaderFinder = class(TFJupiterForm)
  private
    procedure Internal_UpdateComponents; override;

    procedure Internal_PrepareForm; override;
  public

  end;

var
  FFileReaderFinder: TFFileReaderFinder;

implementation

{$R *.lfm}

{ TFFileReaderFinder }

procedure TFFileReaderFinder.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  Self.Caption := 'Pesquisa em arquivos: ' + jupiterStringUtilsGetLastPathName(Self.Params.VariableById('path').Value);
end;

procedure TFFileReaderFinder.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.ShowSearchBar := True;

  Self.Hint := Self.Params.VariableById('path').Value;
end;

end.

