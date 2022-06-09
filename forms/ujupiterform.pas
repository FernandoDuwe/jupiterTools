unit uJupiterForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { TJupiterForm }

  TJupiterForm = class(TForm)
    procedure FormActivate(Sender: TObject);
  protected
    FSearchParam : String;

    procedure Internal_UpdateComponents; virtual;
    procedure Internal_UpdateDatasets; virtual;
    procedure Internal_UpdateCalcs; virtual;
  public
    procedure UpdateForm;
    procedure Search(prSearch : String);
  end;

var
  JupiterForm: TJupiterForm;

implementation

{$R *.lfm}

{ TJupiterForm }

procedure TJupiterForm.FormActivate(Sender: TObject);
begin
  Self.UpdateForm;
end;

procedure TJupiterForm.Internal_UpdateComponents;
begin
     //
end;

procedure TJupiterForm.Internal_UpdateDatasets;
begin
     //
end;

procedure TJupiterForm.Internal_UpdateCalcs;
begin
     //
end;

procedure TJupiterForm.UpdateForm;
begin
  Self.Internal_UpdateDatasets;
  Self.Internal_UpdateComponents;
  Self.Internal_UpdateCalcs;
end;

procedure TJupiterForm.Search(prSearch: String);
begin
  Self.FSearchParam := prSearch;
end;

end.

