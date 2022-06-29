unit uNewFavoriteItem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  uJupiterForm;

type

  { TFNewFavoriteItem }

  TFNewFavoriteItem = class(TJupiterForm)
    btCancel: TButton;
    btSave: TButton;
    edTitulo: TLabeledEdit;
    edPath: TLabeledEdit;
    pnBottom: TPanel;
    pnBody: TPanel;
    procedure btSaveClick(Sender: TObject);
    procedure edTituloChange(Sender: TObject);
  private
    FIsApplication : Boolean;

    procedure Internal_UpdateComponents; override;
  published
    property IsApplication : Boolean read FIsApplication write FIsApplication;
  public

  end;

var
  FNewFavoriteItem: TFNewFavoriteItem;

implementation

{$R *.lfm}

{ TFNewFavoriteItem }

procedure TFNewFavoriteItem.edTituloChange(Sender: TObject);
begin
  Self.UpdateForm;
end;

procedure TFNewFavoriteItem.btSaveClick(Sender: TObject);
begin
end;

procedure TFNewFavoriteItem.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if Self.IsApplication then
  begin
    Self.Caption := 'Nova aplicação Favorita';

    edPath.EditLabel.Caption := 'Caminho arquivo executável';
  end
  else
  begin
    Self.Caption := 'Nova pasta Favorita';

    edPath.EditLabel.Caption := 'Caminho pasta';
  end;

  btSave.Enabled := (Trim(edTitulo.Text) <> EmptyStr) and (Trim(edPath.Text) <> EmptyStr);

  if not btSave.Enabled then
    Exit;

  if Self.IsApplication then
    btSave.Enabled := FileExists(edPath.Text)
  else
    btSave.Enabled := DirectoryExists(edPath.Text);
end;

end.

