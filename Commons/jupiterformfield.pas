unit JupiterFormField;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, SysUtils, JupiterConsts, JupiterObject, JupiterVariable,
  JupiterVariableForm, JupiterVariableDataProvider, JupiterRunnable,
  JupiterEnviroment, ExtCtrls, Forms, StdCtrls;

type

  { TJupiterFormField }

  TJupiterFormField = class(TJupiterObject)
  private
    function Internal_GetValue: String;
  protected
    FVariable    : TJupiterVariableForm;
    FTabOrder    : Integer;
    FPanel       : TPanel;
    FPanelButton : TPanel;
    FButtonTop   : Integer;
    FOnKeyUp     : TKeyEvent;

    FEdit : TEdit;
    FCmb  : TComboBox;

    function  Internal_GenerateHint : String;
    function  Internal_CreateContainer(prOwner : TScrollBox) : TPanel;
    function  Internal_CreateButtonContainer(prContainer : TPanel) : TPanel;
    procedure Internal_CreateButtons(var prContainer : TPanel);
    function  Internal_CreatetTitle(prContainer : TPanel) : TLabel;
    function  Internal_CreateEdit(prContainer : TPanel; prTop : Integer) : TEdit;
    function  Internal_CreateCombo(prContainer : TPanel; prTop : Integer) : TComboBox;

    procedure Internal_Change(prSender : TObject);
    procedure Internal_RunButtonClick(Sender : TObject);
    procedure Internal_CopyButtonClick(Sender : TObject);
    procedure Internal_CopyButtonDbClick(Sender : TObject);
  published
    property Panel       : TPanel read FPanel;
    property PanelButton : TPanel read FPanelButton;
    property TabOrder    : Integer read FTabOrder write FTabOrder default 1;
    property Variable    : TJupiterVariableForm read FVariable write FVariable;
    property Value       : String read Internal_GetValue;

    property OnKeyUp : TKeyEvent read FOnKeyUp write FOnKeyUp;
  public
    procedure Draw(prOwner : TScrollBox);
  end;

implementation

uses Buttons, JupiterApp, StrUtils, Graphics;

{ TJupiterFormField }

function TJupiterFormField.Internal_GetValue: String;
begin
  Result := EmptyStr;

  if Assigned(Self.FEdit) then
    Result := Self.FEdit.Text;

  if Assigned(Self.FCmb) then
    Result := Self.FCmb.Text;
end;

function TJupiterFormField.Internal_GenerateHint: String;
begin
  Result := Format('%1:s%0:s%0:sID: %2:s%0:sObrigatório: %3:s%0:sSomente Leitura: %4:s%0:sGravável: %5:s%0:s%0:s%6:s',
                   [#13#10,
                    Self.Variable.Title,
                    Self.Variable.ID,
                    IfThen(Self.Variable.Required, 'Sim', 'Não'),
                    IfThen(Self.Variable.ReadOnly, 'Sim', 'Não'),
                    IfThen(Self.Variable.Save, 'Sim', 'Não'),
                    'Dê um duplo clique para copiar o nome do campo para a área de trasferência']);
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

function TJupiterFormField.Internal_CreateButtonContainer(prContainer : TPanel): TPanel;
begin
  Result            := TPanel.Create(prContainer);
  Result.Parent     := prContainer;
  Result.Align      := alRight;
  Result.Width      := FORM_MARGIN_RIGHT;
  Result.Caption    := EmptyStr;
  Result.BevelOuter := bvNone;
  Result.TabStop    := False;

  Self.Internal_CreateButtons(Result);
end;

procedure TJupiterFormField.Internal_CreateButtons(var prContainer: TPanel);
var
  vrButton : TSpeedButton;
  vrCont   : Integer;
begin
  vrCont := 0;

  if Self.Variable.CopyButton then
    vrCont := vrCont + 1;

  if Self.Variable.RunButton then
    vrCont := vrCont + 1;

  prContainer.Width := (vrCont * 34) + FORM_MARGIN_LEFT;

  vrCont := 0;

  if Self.Variable.RunButton then
  begin
    vrButton            := TSpeedButton.Create(prContainer);
    vrButton.Parent     := prContainer;
    vrButton.Height     := 32;
    vrButton.Width      := 32;
    vrButton.Hint       := 'Clique aqui para executar';
    vrButton.ShowHint   := True;
    vrButton.Top        := Self.FButtonTop - 5;
    vrButton.Left       := (vrCont * 34);
    vrButton.Flat       := True;
    {$IFNDEF JUPITERCLI}
    vrButton.Images     := vrJupiterApp.MainIcons;
    {$ENDIF}
    vrButton.ImageIndex := ICON_PLAY;
    vrButton.OnClick    := @Internal_RunButtonClick;

    vrCont := vrCont + 1;
  end;

  if Self.Variable.CopyButton then
  begin
    vrButton          := TSpeedButton.Create(prContainer);
    vrButton.Parent   := prContainer;
    vrButton.Height   := 32;
    vrButton.Width    := 32;
    vrButton.Hint     := 'Clique aqui para copiar o conteúdo';
    vrButton.ShowHint := True;
    vrButton.Top      := Self.FButtonTop - 5;
    vrButton.Left     := (vrCont * 34);
    vrButton.Flat     := True;
    {$IFNDEF JUPITERCLI}
    vrButton.Images   := vrJupiterApp.MainIcons;
    {$ENDIF}
    vrButton.ImageIndex := ICON_COPY;
    vrButton.OnClick    := @Internal_CopyButtonClick;
//    vrButton.OnDblClick := @Internal_CopyButtonDbClick;

    vrCont := vrCont + 1;
  end;
end;

function TJupiterFormField.Internal_CreatetTitle(prContainer: TPanel): TLabel;
var
  vrSufix : String;
begin
  vrSufix := EmptyStr;

  if Self.Variable.ReadOnly then
    vrSufix := ' º';

  if Self.Variable.Required then
    vrSufix := ' *';

  Result           := TLabel.Create(prContainer);
  Result.Parent    := prContainer;
  Result.Top       := FORM_MARGIN_TOP;
  Result.Left      := FORM_MARGIN_LEFT;
  Result.Caption   := IfThen(Trim(Self.Variable.Title) = EmptyStr, Self.Variable.ID, Self.Variable.Title) + vrSufix;
  Result.Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
end;

function TJupiterFormField.Internal_CreateEdit(prContainer: TPanel; prTop : Integer): TEdit;
begin
  Result          := TEdit.Create(prContainer);
  Result.Parent   := prContainer;
  Result.Top      := prTop;
  Result.Left     := FORM_MARGIN_LEFT;
  Result.Width    := prContainer.Width - (FORM_MARGIN_LEFT + Self.PanelButton.Width);
  Result.Caption  := Self.Variable.Value;
  Result.Anchors  := [akTop, akLeft, akRight];
  Result.ReadOnly := Self.Variable.ReadOnly;
  Result.OnChange := @Self.Internal_Change;
  Result.Hint     := Self.Internal_GenerateHint;
  Result.ShowHint := True;
  Result.TabOrder := 1;
  Result.TabStop  := True;
  Result.Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);
  Result.OnKeyUp   := Self.OnKeyUp;

  Result.OnDblClick := @Internal_CopyButtonDbClick;

  if (not Self.Variable.CleanOnShow) then
  begin
    Result.Text := vrJupiterApp.Params.ResolveString(Self.Variable.Value);
  end
  else
    Result.Text := EmptyStr;
end;

function TJupiterFormField.Internal_CreateCombo(prContainer: TPanel; prTop: Integer): TComboBox;
var
  vrStr : TStrings;
begin
  Result          := TComboBox.Create(prContainer);
  Result.Parent   := prContainer;
  Result.Top      := prTop;
  Result.Left     := FORM_MARGIN_LEFT;
  Result.Width    := prContainer.Width - (FORM_MARGIN_LEFT + Self.PanelButton.Width);
  Result.Anchors  := [akTop, akLeft, akRight];
  Result.ReadOnly := Self.Variable.ReadOnly;
  Result.OnChange := @Self.Internal_Change;
  Result.Hint     := Self.Internal_GenerateHint;
  Result.ShowHint := True;
  Result.TabOrder := 1;
  Result.TabStop  := True;

  Result.Font.Size := StrToInt(vrJupiterApp.Params.VariableById(FIELD_FONT_SIZE).Value);

  Result.OnDblClick := @Internal_CopyButtonDbClick;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;

    if vrJupiterApp.Params.Exists(Self.Variable.ListVariable) then
    begin
      if vrJupiterApp.Params.VariableById(Self.Variable.ListVariable) is TJupiterVariableDataProvider then
        TJupiterVariableDataProvider(vrJupiterApp.Params.VariableById(Self.Variable.ListVariable)).AsList(vrStr)
      else
        vrJupiterApp.Params.VariableById(Self.Variable.ListVariable).AsList(vrStr);

      Result.Items.Clear;
      Result.Items.AddStrings(vrStr);
    end;

    if (not Self.Variable.CleanOnShow) then
    begin
      Result.ItemIndex := Result.Items.IndexOf(vrJupiterApp.Params.ResolveString(Self.Variable.Value));
      Result.Text      := vrJupiterApp.Params.ResolveString(Self.Variable.Value);
    end
    else
    begin
      Result.ItemIndex := -1;
      Result.Text      := EmptyStr;
    end;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterFormField.Internal_Change(prSender: TObject);
begin
  if Self.Variable.ComponentType = FIELD_TYPE_EDIT then
    Self.Variable.Value := TEdit(prSender).Caption
  else
    Self.Variable.Value := TComboBox(prSender).Text;
end;

procedure TJupiterFormField.Internal_RunButtonClick(Sender: TObject);
begin
  if Self.Value = EmptyStr then
    Exit;

  TJupiterRunnable.Create(Self.Value, True);
end;

procedure TJupiterFormField.Internal_CopyButtonClick(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
begin
  if Self.Value = EmptyStr then
    Exit;

  vrEnviroment := TJupiterEnviroment.Create();
  try
    vrEnviroment.CopyToClipboard(Self.Value);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterFormField.Internal_CopyButtonDbClick(Sender: TObject);
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create();
  try
    vrEnviroment.CopyToClipboard(Self.Variable.ID);
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

procedure TJupiterFormField.Draw(prOwner: TScrollBox);
var
  vrContainer : TPanel;
  vrLabel     : TLabel;
begin
  try
    vrContainer := Self.Internal_CreateContainer(prOwner);
    vrLabel     := Self.Internal_CreatetTitle(vrContainer);

    Self.FButtonTop := ((vrLabel.Top + vrLabel.Height) + FORM_MARGIN_BOTTOM);

    Self.FPanelButton := Self.Internal_CreateButtonContainer(vrContainer);

    if Self.Variable.ComponentType = FIELD_TYPE_EDIT then
      Self.FEdit := Self.Internal_CreateEdit(vrContainer, ((vrLabel.Top + vrLabel.Height) + FORM_MARGIN_BOTTOM))
    else
      Self.FCmb := Self.Internal_CreateCombo(vrContainer, ((vrLabel.Top + vrLabel.Height) + FORM_MARGIN_BOTTOM));
  finally
    if Self.Variable.ComponentType = FIELD_TYPE_EDIT then
      vrContainer.Height := (Self.FEdit.Top + Self.FEdit.Height) + FORM_MARGIN_BOTTOM_TONEXT
    else
      vrContainer.Height := (Self.FCmb.Top + Self.FCmb.Height) + FORM_MARGIN_BOTTOM_TONEXT;

    Self.FPanel := vrContainer;
  end;
end;

end.

