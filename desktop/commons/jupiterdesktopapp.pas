unit jupiterDesktopApp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterApp, JupiterObject, jupiterScript, JupiterRoute,
  uJupiterFormDesktopAppScript, Forms, Controls;

type

  { TJupiterDesktopApp }

  TJupiterDesktopApp = class(TJupiterApp)
  private
    FFormRoutes : TJupiterObjectList;
    FImageList  : TImageList;
    FFormList   : TJupiterObjectList;
  protected
    procedure Internal_AddScriptLibraries(var prScript : TJupiterScript); override;
  published
    property FormRoutes : TJupiterObjectList read FFormRoutes write FFormRoutes;
    property ImageList : TImageList read FImageList write FImageList;
    property FormList : TJupiterObjectList read FFormList write FFormList;
  public
    function NewFormByRoute(prRoute : String) : TForm;
    function OpenForm(prRoute, prParams : String) : String;
    procedure OpenForm(prForm : TForm);

    function GetFormById(prFormID : String) : TForm;
    procedure DeleteFormById(prFormID : String);

    constructor Create(prAppID, prAppName : String); override;
    destructor Destroy; override;
  end;

implementation

uses uJupiterForm, uMain, uJupiterDesktopAppScript;

{ TJupiterDesktopApp }

procedure TJupiterDesktopApp.Internal_AddScriptLibraries(var prScript: TJupiterScript);
begin
  inherited Internal_AddScriptLibraries(prScript);

  prScript.LibraryList.Add(TJupiterDesktopAppScript.Create);
  prScript.LibraryList.Add(TJupiterFormDesktopAppScript.Create);
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

