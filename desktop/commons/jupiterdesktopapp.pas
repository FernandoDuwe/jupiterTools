unit jupiterDesktopApp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterApp, JupiterObject, jupiterScript, JupiterRoute,
  JupiterVariable, jupiterDatabaseWizard, JupiterConsts,
  JupiterFileDataProvider, JupiterEnviroment, uJupiterFormDesktopAppScript,
  uJupiterAction, Forms, Controls;

type

  { TJupiterDesktopApp }

  TJupiterDesktopApp = class(TJupiterApp)
  private
    FFormRoutes : TJupiterObjectList;
    FImageList  : TImageList;
    FFormList   : TJupiterObjectList;
  protected
    procedure Internal_AddScriptLibraries(var prScript : TJupiterScript); override;

    procedure Internal_Prepare; override;
  published
    property FormRoutes : TJupiterObjectList read FFormRoutes write FFormRoutes;
    property ImageList : TImageList read FImageList write FImageList;
    property FormList : TJupiterObjectList read FFormList write FFormList;
  public
    function NewFormByRoute(prRoute : String) : TForm;
    function OpenForm(prRoute, prParams : String) : String;
    function OpenForm(prRoute : String; prParams : TJupiterVariableList) : String;
    procedure OpenForm(prForm : TForm);

    function GetFormById(prFormID : String) : TForm;
    procedure DeleteFormById(prFormID : String);
    function GenerateContextMenu(prSearch : String = '') : TJupiterActionGroup;
    procedure GetExternalImages;

    // Pins
    procedure CreatePin(prTableName : String; prID : Integer);
    procedure RemovePin(prTableName : String; prID : Integer);
    function PinExists(prTableName : String; prID : Integer) : Boolean;

    constructor Create(prAppID, prAppName : String); override;
    destructor Destroy; override;
  end;

implementation

uses uJupiterForm, uMain, uJupiterDesktopAppScript, SQLDB, Graphics;

{ TJupiterDesktopApp }

procedure TJupiterDesktopApp.Internal_AddScriptLibraries(var prScript: TJupiterScript);
begin
  inherited Internal_AddScriptLibraries(prScript);

  prScript.LibraryList.Add(TJupiterDesktopAppScript.Create);
  prScript.LibraryList.Add(TJupiterFormDesktopAppScript.Create);
end;

procedure TJupiterDesktopApp.Internal_Prepare;
begin
  inherited;
end;

function TJupiterDesktopApp.NewFormByRoute(prRoute: String): TForm;
var
   vrVez : Integer;
begin
  Result := nil;

  for vrVez := 0 to Self.FormRoutes.Count - 1 do
    with TJupiterFormRoute(Self.FormRoutes.GetAtIndex(vrVez)) do
    begin
      if Path = prRoute then
      begin
        Result := TFJupiterForm(FormClass.Create(Application.MainForm));
        Exit;
      end;
    end;
end;

function TJupiterDesktopApp.OpenForm(prRoute, prParams: String) : String;
var
  vrForm : TForm;
begin
  vrForm := Self.NewFormByRoute(prRoute);

  if not Assigned(vrForm) then
    vrForm := TFJupiterForm.Create(Application.MainForm);

  if ((vrForm is TFJupiterForm) and (prParams <> EmptyStr)) then
    TFJupiterForm(vrForm).Params.AddVariable('Params', prParams);

  Result := TFJupiterForm(vrForm).FormID;

  Self.OpenForm(vrForm);
end;

function TJupiterDesktopApp.OpenForm(prRoute : String; prParams: TJupiterVariableList): String;
var
  vrForm : TForm;
begin
  vrForm := Self.NewFormByRoute(prRoute);

  if not Assigned(vrForm) then
    vrForm := TFJupiterForm.Create(Application.MainForm);

  if (vrForm is TFJupiterForm) then
    TFJupiterForm(vrForm).Params.CopyValues(prParams);

  Result := TFJupiterForm(vrForm).FormID;

  Self.OpenForm(vrForm);
end;

procedure TJupiterDesktopApp.OpenForm(prForm: TForm);
begin
  prForm.Align       := alClient;
  prForm.WindowState := wsMaximized;

  if Application.MainForm is TFMain then
    TFMain(Application.MainForm).NewTab(prForm);
end;

function TJupiterDesktopApp.GetFormById(prFormID: String): TForm;
var
  vrVez : Integer;
begin
  Result := nil;

  if not Assigned(FormList) then Exit;

  for vrVez := 0 to Self.FormList.Count - 1 do
    with TFJupiterForm(Self.FormList.GetAtIndex(vrVez)) do
      if FormID = prFormID then
      begin
        Result := TFJupiterForm(Self.FormList.GetAtIndex(vrVez));
        Exit;
      end;

end;

procedure TJupiterDesktopApp.DeleteFormById(prFormID: String);
var
  vrVez : Integer;
begin
  if not Assigned(FormList) then Exit;

  for vrVez := 0 to Self.FormList.Count - 1 do
    with TFJupiterForm(Self.FormList.GetAtIndex(vrVez)) do
      if FormID = prFormID then
      begin
        Self.FormList.DeleteListItem(vrVez);

        Exit;
      end;
end;

function TJupiterDesktopApp.GenerateContextMenu(prSearch : String = '') : TJupiterActionGroup;
var
  vrWizard : TJupiterDatabaseWizard;
  vrStringList : TStrings;
  vrVez : Integer;
  vrSQL : TSQLQuery;
  vrStrAux : String;
begin
  Result   := TJupiterActionGroup.Create;
  vrWizard := Self.NewWizard;
  vrStringList := TStringList.Create;
  vrSQL := vrWizard.NewQuery;
  try
    vrStringList.Clear;

    vrSQL.SQL.Add(' SELECT R1.TITLE, M1.NAME AS HINT, R1.ROUTE, M1.MACROID ');
    vrSQL.SQL.Add(' FROM ROUTES R1 ');
    vrSQL.SQL.Add('   INNER JOIN MACROS M1 ON (R1.DESTINY = M1.ID) ');
    vrSQL.SQL.Add(' WHERE UPPER(R1.ROUTE) LIKE "/CONTEXT/%" ');

    if Trim(prSearch) <> EmptyStr then
      vrSQL.SQL.Add(' AND UPPER(R1.TITLE) LIKE "%' + AnsiUpperCase(prSearch) + '%"  ');

    vrSQL.SQL.Add(' ORDER BY 1 ');
    vrSQL.Open;
    vrSQL.First;

    while not vrSQL.EOF do
    begin
      Result.Add(TJupiterAction.Create(vrSQL.FieldByName('TITLE').AsString, vrSQL.FieldByName('HINT').AsString, NULL_KEY, vrSQL.FieldByName('MACROID').AsString));

      vrSQL.Next;
    end;

    vrSQL.Close;
    vrSQL.SQL.Clear;
    vrSQL.SQL.Add(' SELECT TABLENAME, RECORDKEY FROM RECORDPIN ');
    vrSQL.Open;

    vrSQL.First;

    while not vrSQL.EOF do
    begin
      vrStrAux := NewWizard.GetTableDescription(vrSQL.FieldByName('TABLENAME').AsString, vrSQL.FieldByName('RECORDKEY').AsInteger);

      if Trim(prSearch) = EmptyStr then
      begin
        Result.Add(TJupiterAction.Create(vrStrAux, 'Abrir registro marcado como fixo', ICON_PIN, CreateStringListToMacro('')));

        vrSQL.Next;

        Continue;
      end;

      if Pos(AnsiUpperCase(prSearch), AnsiUpperCase(vrStrAux)) > 0 then
        Result.Add(TJupiterAction.Create(vrStrAux, 'Abrir registro marcado como fixo', ICON_PIN, CreateStringListToMacro('')));

      vrSQL.Next;
    end;

    vrWizard.Connection.GetTableNames(vrStringList, False);

    for vrVez := 0 to vrStringList.Count - 1 do
    begin
      if Trim(prSearch) = EmptyStr then
      begin
        Result.Add(TJupiterAction.Create('Tabela: ' + vrStringList[vrVez], 'Abrir grid da tabela ' + vrStringList[vrVez], ICON_GRID, CreateStringListToMacro('  OpenGridFromTable(''' + vrStringList[vrVez] + '''); ')));
        Continue;
      end;

      if Pos(AnsiUpperCase(prSearch), AnsiUpperCase(vrStringList[vrVez])) > 0 then
        Result.Add(TJupiterAction.Create('Tabela: ' + vrStringList[vrVez], 'Abrir grid da tabela ' + vrStringList[vrVez], ICON_GRID, CreateStringListToMacro('  OpenGridFromTable(''' + vrStringList[vrVez] + '''); ')));
    end;
  finally
    FreeAndNil(vrWizard);
    FreeAndNil(vrStringList);
    FreeAndNil(vrSQL);
  end;
end;

procedure TJupiterDesktopApp.GetExternalImages;
var
  vrProvider : TJupiterFileDataProvider;
  vrEnviroment : TJupiterEnviroment;
  vrVez : Integer;
  vrBitmap  : TBitmap;
  vrPicture : TPicture;
begin
  inherited Internal_Prepare;

  vrProvider := TJupiterFileDataProvider.Create;
  vrEnviroment := TJupiterEnviroment.Create;
  try
    vrProvider.Path := vrEnviroment.FullPath('/assets/');
    vrProvider.ProvideData;

    for vrVez := 0 to vrProvider.Count - 1 do
      with vrProvider.GetRowByIndex(vrVez) do
      begin
        if not vrEnviroment.IsPictureFile(Fields.VariableById('File').Value) then
          Continue;

        vrPicture := TPicture.Create;
        try
          vrPicture.LoadFromFile(Fields.VariableById('File').Value);

          vrBitmap := TBitmap.Create;
          vrBitmap.Assign(vrPicture.Graphic);

          Self.ImageList.Add(vrBitmap, nil);
        finally
          FreeAndNil(vrPicture);
        end;
      end;
  finally
    FreeAndNil(vrProvider);
    FreeAndNil(vrEnviroment);
  end;

end;

procedure TJupiterDesktopApp.CreatePin(prTableName: String; prID: Integer);
begin
  Self.NewWizard.ExecuteScript(CreateStringList(' INSERT INTO RECORDPIN (TABLENAME, RECORDKEY) VALUES ("' + prTableName + '", ' + IntToStr(prID) + ') '));
end;

procedure TJupiterDesktopApp.RemovePin(prTableName: String; prID: Integer);
begin
  Self.NewWizard.ExecuteScript(CreateStringList(' DELETE FROM RECORDPIN WHERE TABLENAME = "' + prTableName + '" AND RECORDKEY = ' + IntToStr(prID) + ' '));
end;

function TJupiterDesktopApp.PinExists(prTableName: String; prID: Integer): Boolean;
begin
  Result := Self.NewWizard.Exists('RECORDPIN', ' TABLENAME = "' + prTableName + '" AND RECORDKEY = ' + IntToStr(prID) + ' ');
end;

constructor TJupiterDesktopApp.Create(prAppID, prAppName: String);
begin
  inherited Create(prAppID, prAppName);

  Self.FFormRoutes := TJupiterObjectList.Create;

  Self.FFormList := TJupiterObjectList.Create;
end;

destructor TJupiterDesktopApp.Destroy;
begin
  FreeAndNil(Self.FFormRoutes);

  FreeAndNil(Self.FFormList);

  inherited Destroy;
end;

end.

