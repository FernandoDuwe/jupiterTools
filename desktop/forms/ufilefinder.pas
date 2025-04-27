unit uFileFinder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, uJupiterForm,
  jupiterStringUtils;

type

  { TFFileFinder }

  TFFileFinder = class(TFJupiterForm)
    tvFileTree: TTreeView;
  private
    procedure Internal_UpdateComponents; override;

    procedure Internal_PrepareForm; override;
  public

  end;

var
  FFileFinder: TFFileFinder;

implementation

{$R *.lfm}

{ TFFileFinder }

procedure TFFileFinder.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  Self.Caption := 'Pesquisar: ' + jupiterStringUtilsGetLastPathName(Self.Params.VariableById('path').Value);
end;

procedure TFFileFinder.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.ShowSearchBar := True;

  Self.Hint := Self.Params.VariableById('path').Value;
end;

end.

