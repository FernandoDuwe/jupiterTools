unit uRichTextEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, RichMemo, uJupiterForm,
  JupiterConsts, JupiterApp, jupiterDatabaseWizard, JupiterModule,
  uJupiterStringUtilsScript, LCLType, uJupiterAction, jupiterDesktopApp,
  SQLDB;

type

  { TFRichTextEditor }

  TFRichTextEditor = class(TFJupiterForm)
    RichMemo1: TRichMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure RichMemo1Change(Sender: TObject);
  private
    FEdited : Boolean;

    procedure Internal_UpdateComponents; override;
    procedure Internal_PrepareForm; override;

    procedure Internal_OnSave(Sender: TObject);
    procedure Internal_OnAumentarFonte(Sender: TObject);
    procedure Internal_OnDiminuirFonte(Sender: TObject);

    procedure Internal_OnNegritoFonte(Sender: TObject);
    procedure Internal_OnItalicoFonte(Sender: TObject);
    procedure Internal_OnSublinhadoFonte(Sender: TObject);

    function Internal_EnableWorkMenu : Boolean; override;
    procedure Internal_AddToWorkMenu; override;

    function Internal_GetRouteName : String;
    function Internal_GetMacroName : String;
  public

  end;

var
  FRichTextEditor: TFRichTextEditor;

implementation

{$R *.lfm}

{ TFRichTextEditor }

procedure TFRichTextEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  inherited;

  if Self.FEdited then
    if Application.MessageBox('Deseja salvar?', 'Confirmação', MB_YESNO + MB_ICONQUESTION) = ID_YES then
      Self.Internal_OnSave(Sender);
end;

procedure TFRichTextEditor.RichMemo1Change(Sender: TObject);
begin
  if not Self.Prepared then
    Exit;

  if Self.FEdited then
    Exit;

  Self.FEdited := True;

  Self.UpdateForm();
end;

procedure TFRichTextEditor.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  if Self.FEdited then
    Self.ActionGroup.GetActionAtIndex(0).Enable;

  if not Self.FEdited then
    Self.ActionGroup.GetActionAtIndex(0).Disable;
end;

procedure TFRichTextEditor.Internal_PrepareForm;
var
  vrFS : TFileStream;
begin
  inherited Internal_PrepareForm;

  Self.FEdited := False;

  Self.ActionGroup.AddAction(TJupiterAction.Create('Salvar', 'Clique aqui para abrir salvar o arquivo', ICON_SAVE, @Internal_OnSave));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Aumentar fonte', 'Clique aqui para aumentar a fonte', ICON_CURTASK, @Internal_OnAumentarFonte));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Diminuir fonte', 'Clique aqui para diminuir a fonte', ICON_CURTASK, @Internal_OnDiminuirFonte));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Negrito', 'Clique aqui para aplicar negrito ao texto', NULL_KEY, @Internal_OnNegritoFonte));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Itálico', 'Clique aqui para aplicar sublinhado ao texto', NULL_KEY, @Internal_OnItalicoFonte));

  Self.ActionGroup.AddAction(TJupiterAction.Create('Sublinhado', 'Clique aqui para aplicar sublinhado ao texto', NULL_KEY, @Internal_OnSublinhadoFonte));

  if Self.Params.Exists('path') then
  begin
    RichMemo1.Lines.Clear;

    Self.Caption := ExtractFileName(Self.Params.VariableById('path').Value);
    Self.Hint := Self.Params.VariableById('path').Value;

    vrFS := TFileStream.Create(Utf8ToAnsi(Self.Params.VariableById('path').Value), fmOpenRead or fmShareDenyNone);
    try
      RichMemo1.LoadRichText(vrFS);
    finally
      vrFS.Free;
    end;
  end
  else
  begin
    RichMemo1.Lines.Clear;
    RichMemo1.Lines.Text := vrJupiterApp.NewWizard.Resolve(Self.Params.VariableById('table').Value,
                                                          Self.Params.VariableById('field').Value,
                                                          ' ID = ' + Self.Params.VariableById('id').Value);

    Self.Caption := String.Format('%0:s', [TJupiterDesktopApp(vrJupiterApp).NewWizard.GetTableDescription(Self.Params.VariableById('table').Value, StrToInt(Self.Params.VariableById('id').Value))]);
    Self.Hint := Self.Caption;
  end;

  Self.FEdited := False;
end;

procedure TFRichTextEditor.Internal_OnSave(Sender: TObject);
var
  vrQry : TSQLQuery;
  vrWizard : TJupiterDatabaseWizard;
  vrFS : TFileStream;
begin
  Self.FEdited := False;

  vrWizard := vrJupiterApp.NewWizard;
  vrQry    := vrWizard.NewQuery;
  try
    if Self.Params.Exists('path') then
    begin
      vrFS := TFileStream.Create(Utf8ToAnsi(Self.Params.VariableById('path').Value), fmCreate);
      try
        RichMemo1.SaveRichText(vrFS);
      finally
        vrFS.Free;
      end;
    end
    else
    begin
      vrQry.SQL.Add(' UPDATE ' + Self.Params.VariableById('table').Value);
      vrQry.SQL.Add(' SET ' + Self.Params.VariableById('field').Value + ' = :PRTEXT ');
      vrQry.SQL.Add(' WHERE ID = ' + Self.Params.VariableById('id').Value);
      vrQry.ParamByName('PRTEXT').AsString := RichMemo1.Lines.Text;

      if not vrWizard.Transaction.Active then
        vrWizard.Transaction.StartTransaction;

      try
        vrQry.ExecSQL;

        vrWizard.Transaction.CommitRetaining;
      except
        vrWizard.Transaction.RollbackRetaining;

        raise;
      end;
    end;
  finally
    FreeAndNil(vrWizard);
    FreeAndNil(vrQry);

    Self.UpdateForm();
  end;
end;

procedure TFRichTextEditor.Internal_OnAumentarFonte(Sender: TObject);
begin
  RichMemo1.Font.Size := RichMemo1.Font.Size + 1;
end;

procedure TFRichTextEditor.Internal_OnDiminuirFonte(Sender: TObject);
begin
  RichMemo1.Font.Size := RichMemo1.Font.Size - 1;
end;

procedure TFRichTextEditor.Internal_OnNegritoFonte(Sender: TObject);
var
  vrParams : TFontParams;
begin
  RichMemo1.GetTextAttributes(RichMemo1.SelStart, vrParams);

  if fsBold in vrParams.Style then
    Exclude(vrParams.Style, fsBold)
  else
    Include(vrParams.Style, fsBold);

  RichMemo1.SetTextAttributes(RichMemo1.SelStart, RichMemo1.SelLength, vrParams);
end;

procedure TFRichTextEditor.Internal_OnItalicoFonte(Sender: TObject);
var
  vrParams : TFontParams;
begin
  RichMemo1.GetTextAttributes(RichMemo1.SelStart, vrParams);

  if fsItalic in vrParams.Style then
    Exclude(vrParams.Style, fsItalic)
  else
    Include(vrParams.Style, fsItalic);

  RichMemo1.SetTextAttributes(RichMemo1.SelStart, RichMemo1.SelLength, vrParams);
end;

procedure TFRichTextEditor.Internal_OnSublinhadoFonte(Sender: TObject);
var
  vrParams : TFontParams;
begin
  RichMemo1.GetTextAttributes(RichMemo1.SelStart, vrParams);

  if fsUnderline in vrParams.Style then
    Exclude(vrParams.Style, fsUnderline)
  else
    Include(vrParams.Style, fsUnderline);

  RichMemo1.SetTextAttributes(RichMemo1.SelStart, RichMemo1.SelLength, vrParams);
end;

function TFRichTextEditor.Internal_EnableWorkMenu: Boolean;
begin
  Result := True;
end;

procedure TFRichTextEditor.Internal_AddToWorkMenu;
var
  vrModule : TJupiterModule;
  vrWizard : TJupiterDatabaseWizard;
begin
  inherited Internal_AddToWorkMenu;

  vrWizard := vrJupiterApp.NewWizard;
  vrModule := TJupiterModule.Create;
  try
    if vrModule.CreateMacroIfDontExists(Self.Internal_GetMacroName, 'Clique do item de menu ' + ExtractFileName(Self.Params.VariableById('path').Value), CreateStringListToMacro(' OpenTextEditorForm(''' + Self.Params.VariableById('path').Value + '''); ')) then
      vrModule.CreateRouteIfDontExists(Self.Caption, Self.Internal_GetRouteName, vrWizard.GetLastID('MACROS'), ICON_DOCFILE, 1000);
  finally
    FreeAndNil(vrModule);
    FreeAndNil(vrWizard);
  end;
end;

function TFRichTextEditor.Internal_GetRouteName: String;
begin
  if Self.Params.Exists('path') then
    Result := vrJupiterApp.Params.VariableById('Menus.Work.Route').Value + '/file_' + FormatDateTime('ddmmyyyy_hhnnss', Now);

  Result := Self.Caption;
end;

function TFRichTextEditor.Internal_GetMacroName: String;
begin
  Result := JupiterStringUtilsScript_Replace(Copy(Self.Internal_GetRouteName, 2), '/', '.');
end;

end.

