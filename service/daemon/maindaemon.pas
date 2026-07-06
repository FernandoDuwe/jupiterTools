unit maindaemon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jupiterthread, DaemonApp;

type

  { TMainDaemon1 }

  TMainDaemon1 = class(TDaemon)
  private
    FThreadList : TJupiterThreadList;
  public
    Function Start : Boolean; override;
    Function Stop : Boolean; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainDaemon1: TMainDaemon1;

implementation

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TMainDaemon1)
end;

{$R *.lfm}

{ TMainDaemon1 }

function TMainDaemon1.Start: Boolean;
var
  vrVez : Integer;
begin
  Result := inherited Start;

  for vrVez := 0 to Self.FThreadList.Count - 1 do
    Self.FThreadList.ThreadByIndex(vrVez).Resume;
end;

function TMainDaemon1.Stop: Boolean;
begin
  Self.FThreadList.StopAll;

  Result := inherited Stop;
end;

constructor TMainDaemon1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.Definition.Name := 'Elara';
  Self.Definition.DisplayName := 'Elara: Serviço de execução do Jupiter';

  Self.FThreadList := TJupiterThreadList.Create;
end;

destructor TMainDaemon1.Destroy;
begin
  Self.FThreadList.Free;

  inherited Destroy;
end;


initialization
  RegisterDaemon;
end.

