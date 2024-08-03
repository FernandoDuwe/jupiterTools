unit jupiterDesktopApp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterApp, JupiterObject, jupiterScript, JupiterRoute,
  Forms, Controls;

type

  { TJupiterDesktopApp }

  TJupiterDesktopApp = class(TJupiterApp)
  private
    FFormRoutes : TJupiterObjectList;
    FImageList  : TImageList;
  protected
    procedure Internal_AddScriptLibraries(var prScript : TJupiterScript); override;
  published
    property FormRoutes : TJupiterObjectList read FFormRoutes write FFormRoutes;
    property ImageList : TImageList read FImageList write FImageList;
  public
    function NewFormByRoute(prRoute : String) : TForm;
    procedure OpenForm(prRoute : String);
    procedure OpenForm(prForm : TForm);

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

procedure TJupiterDesktopApp.OpenForm(prRoute: String);
var
  vrForm : TForm;
begin
  vrForm := Self.NewFormByRoute(prRoute);

  if not Assigned(vrForm) then
    vrForm := TFJupiterForm.Create(Application.MainForm);

  Self.OpenForm(vrForm);
end;

procedure TJupiterDesktopApp.OpenForm(prForm: TForm);
begin
  prForm.Align       := alClient;
  prForm.WindowState := wsMaximized;

  if Application.MainForm is TFMain then
    TFMain(Application.MainForm).NewTab(prForm);
end;

constructor TJupiterDesktopApp.Create(prAppID, prAppName: String);
begin
  inherited Create(prAppID, prAppName);

  Self.FFormRoutes := TJupiterObjectList.Create;
end;

destructor TJupiterDesktopApp.Destroy;
begin
  FreeAndNil(Self.FFormRoutes);

  inherited Destroy;
end;

end.

