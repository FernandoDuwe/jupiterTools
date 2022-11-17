unit JupiterFormField;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, SysUtils, JupiterConsts, JupiterObject, JupiterVariable,
  JupiterVariableForm, ExtCtrls, Forms, StdCtrls;

type

  { TJupiterFormField }

  TJupiterFormField = class(TJupiterObject)
  protected
    FVariable : TJupiterVariableForm;
    FTabOrder : Integer;
    FPanel    : TPanel;

    function Internal_GenerateHint : String;
    function Internal_CreateContainer(prOwner : TScrollBox) : TPanel;
    function Internal_CreatetTitle(prContainer : TPanel) : TLabel;
    function Internal_CreateEdit(prContainer : TPanel; prTop : Integer) : TEdit;

    procedure Internal_Change(prSender : TObject);
  published
    property Panel    : TPanel read FPanel;
    property TabOrder : Integer read FTabOrder write FTabOrder default 1;
    property Variable : TJupiterVariableForm read FVariable write FVariable;
  public
    procedure Draw(prOwner : TScrollBox);
  end;

implementation

uses StrUtils;

{ TJupiterFormField }

function TJupiterFormField.Internal_GenerateHint: String;
begin
  Result := Format('%1:s%0:s%0:sID: %2:s%0:sObrigatório: %3:s%0:sSomente Leitura: %4:s%0:sGravável: %5:s',
                   [#13#10,
                    Self.Variable.Title,
                    Self.Variable.ID,
                    IfThen(Self.Variable.Required, 'Sim', 'Não'),
                    IfThen(Self.Variable.ReadOnly, 'Sim', 'Não'),
                    IfThen(Self.Variable.Save, 'Sim', 'Não')]);
end;

function TJupiterFormField.Internal_CreateContainer(prOwner: TScrollBox): TPanel;
begin
  Result            := TPanel.Create(prOwner);
  Result.Parent     := prOwner;
  Result.Align      := alTop;
  Result.Height     := 50;
  Result.Caption    := EmptyStr;
  Result.BevelOuter := bvNone;
  Result.TabStop    := False;
  Result.TabOrder   := GENERATOR_SYSLAYER + Self.TabOrder;
end;

function TJupiterFormField.Internal_CreatetTitle(prContainer: TPanel): TLabel;
begin
  Result         := TLabel.Create(prContainer);
  Result.Parent  := prContainer;
  Result.Top     := FORM_MARGIN_TOP;
  Result.Left    := FORM_MARGIN_LEFT;
  Result.Caption := IfThen(Trim(Self.Variable.Title) = EmptyStr, Self.Variable.ID, Self.Variable.Title);
end;

function TJupiterFormField.Internal_CreateEdit(prContainer: TPanel; prTop : Integer): TEdit;
begin
  Result          := TEdit.Create(prContainer);
  Result.Parent   := prContainer;
  Result.Top      := prTop;
  Result.Left     := FORM_MARGIN_LEFT;
  Result.Width    := prContainer.Width - (FORM_MARGIN_LEFT + FORM_MARGIN_RIGHT);
  Result.Caption  := Self.Variable.Value;
  Result.Anchors  := [akTop, akLeft, akRight];
  Result.Enabled  := not Self.Variable.ReadOnly;
  Result.OnChange := @Self.Internal_Change;
  Result.Hint     := Self.Internal_GenerateHint;
  Result.ShowHint := True;
  Result.TabOrder := 1;
  Result.TabStop  := True;
end;

procedure TJupiterFormField.Internal_Change(prSender: TObject);
begin
  Self.Variable.Value := TEdit(prSender).Caption;
end;

procedure TJupiterFormField.Draw(prOwner: TScrollBox);
var
  vrContainer : TPanel;
  vrLabel : TLabel;
  vrEdit : TEdit;
begin
  try
    vrContainer := Self.Internal_CreateContainer(prOwner);
    vrLabel     := Self.Internal_CreatetTitle(vrContainer);
    vrEdit      := Self.Internal_CreateEdit(vrContainer, ((vrLabel.Top + vrLabel.Height) + FORM_MARGIN_BOTTOM));
  finally
    vrContainer.Height := (vrEdit.Top + vrEdit.Height) + FORM_MARGIN_BOTTOM;

    Self.FPanel := vrContainer;
  end;
end;

end.

