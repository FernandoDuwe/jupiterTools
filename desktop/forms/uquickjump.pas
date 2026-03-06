unit uQuickJump;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, SynEdit,
  SynCompletion, SynHighlighterPas, uJupiterForm, jupiterformutils,
  JupiterConsts, jupiterDatabaseWizard, SQLDB, JupiterApp, uJupiterAction,
  uJupiterDesktopAppScript, uJupiterDatabaseScript;

type

  { TFQuickJump }

  TFQuickJump = class(TFJupiterForm)
    seQuickAccess: TSynEdit;
    SynCompletion1: TSynCompletion;
    SynPasSyn1: TSynPasSyn;
    procedure SynCompletion1BeforeExecute(ASender: TSynBaseCompletion; var ACurrentString: String; var APosition: Integer; var AnX, AnY: Integer; var AnResult: TOnBeforeExeucteFlags);
  private
    FFilter : String;

    procedure Internal_UpdateTags;
    procedure Internal_PrepareForm; override;
    procedure Internal_OnPlay(Sender: TObject);
    procedure Internal_PopularRegistros;
  public

  end;

var
  FQuickJump: TFQuickJump;

implementation

{$R *.lfm}

{ TFQuickJump }

procedure TFQuickJump.SynCompletion1BeforeExecute(ASender: TSynBaseCompletion; var ACurrentString: String; var APosition: Integer; var AnX, AnY: Integer; var AnResult: TOnBeforeExeucteFlags);
var
  vrWizard : TJupiterDatabaseWizard;
  vrTableList : TStrings;
  vrVez : Integer;
  vrLine : String;
begin
  SynCompletion1.ItemList.Clear;

  Self.Internal_UpdateTags;

  vrWizard := vrJupiterApp.NewWizard;
  vrTableList := TStringList.Create;
  try
    vrTableList.Clear;

    vrLine := seQuickAccess.Lines[seQuickAccess.CaretY - 1];

    if Pos('>', vrLine) <> 0 then
      Self.Internal_PopularRegistros
    else
    begin
      vrWizard.Connection.GetTableNames(vrTableList, False);

      for vrVez := 0 to vrTableList.Count - 1 do
        vrTableList[vrVez] := vrTableList[vrVez] + ' > ';

      SynCompletion1.ItemList.AddStrings(vrTableList);
    end;
  finally
    FreeAndNil(vrTableList);
    FreeAndNil(vrWizard);
  end;
end;

procedure TFQuickJump.Internal_UpdateTags;
var
  vrVez : Integer;
begin
  Self.FFilter := EmptyStr;

  for vrVez := 0 to seQuickAccess.Lines.Count - 1 do
    if Copy(TrimLeft(seQuickAccess.Lines[vrVez]), 1, 1) = '?' then
      Self.FFilter := Trim(Copy(TrimLeft(seQuickAccess.Lines[vrVez]), 2));
end;

procedure TFQuickJump.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.FFilter := EmptyStr;

  SynCompletion1.Width := PercentOfScreen(Self.Width, 50);

  Self.Height := 300;
  Self.Width  := PercentOfScreen(Screen.Width, 50);

  Self.ActionGroup.AddAction(TJupiterAction.Create('Executar', 'Clique aqui para executar o script', ICON_PLAY, @Internal_OnPlay));

  seQuickAccess.Lines.Clear;

  Self.Hint := 'Acesse rapidamente os registros na sua base de dados';
end;

procedure TFQuickJump.Internal_OnPlay(Sender: TObject);
var
  vrVez : Integer;
begin
  try
    for vrVez := 0 to seQuickAccess.Lines.Count -1 do
    begin
      if Trim(seQuickAccess.Lines[vrVez]) = EmptyStr then
        Continue;

      if Copy(TrimLeft(seQuickAccess.Lines[vrVez]), 1, 1) = '?' then
        Continue;

      JupiterAppDesktopRunQuickJumpScript(seQuickAccess.Lines[vrVez]);
    end;
  finally
    if Trim(seQuickAccess.Lines.Text) <> EmptyStr then
      DoSecureClose;
  end;
end;

procedure TFQuickJump.Internal_PopularRegistros;
var
  vrQry : TSQLQuery;
  vrWizard : TJupiterDatabaseWizard;
  vrTable : String;
  vrLabel : String;
begin
  vrTable := seQuickAccess.Lines[seQuickAccess.CaretY - 1];
  vrTable := StringReplace(vrTable, '>', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
  vrTable := StringReplace(vrTable, ' ', EmptyStr, [rfIgnoreCase, rfReplaceAll]);
  vrTable := StringReplace(vrTable, #13, EmptyStr, [rfIgnoreCase, rfReplaceAll]);
  vrTable := StringReplace(vrTable, #10, EmptyStr, [rfIgnoreCase, rfReplaceAll]);

  vrWizard := vrJupiterApp.NewWizard;
  vrQry := vrWizard.NewQuery;
  try
    vrQry.SQL.Add(' SELECT * FROM ' + vrTable);
    vrQry.Open;

    while not vrQry.EOF do
    begin
      if Self.FFilter = EmptyStr then
      begin
        SynCompletion1.ItemList.Add(vrQry.FieldByName('ID').AsString + ' // ' + JupiterDatabaseScript_ResolveRecordTable(vrTable, vrQry.FieldByName('ID').AsInteger));

        vrQry.Next;

        Continue;
      end;

      vrLabel := JupiterDatabaseScript_ResolveRecordTable(vrTable, vrQry.FieldByName('ID').AsInteger);

      if Pos(AnsiUpperCase(Self.FFilter), AnsiUpperCase(vrLabel)) <> 0 then
        SynCompletion1.ItemList.Add(vrQry.FieldByName('ID').AsString + ' // ' + JupiterDatabaseScript_ResolveRecordTable(vrTable, vrQry.FieldByName('ID').AsInteger));

      vrQry.Next;
    end;
  finally
    FreeAndNil(vrQry);
    FreeAndNil(vrWizard);
  end;
end;

end.

