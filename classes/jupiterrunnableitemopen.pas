unit jupiterrunnableitemopen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JupiterRunnableItem, jupiterconsts, Dos, Process;

type
  { TJupiterRunnableItemOpen }

  TJupiterRunnableItemOpen = class(TJupiterRunnableItem)
  public
    procedure Internal_Execute; override;

    class function ListAction : String; override; static;
  end;

implementation

{ TJupiterRunnableItemOpen }

procedure TJupiterRunnableItemOpen.Internal_Execute;
var
   vrProcess : TProcess;
begin
  inherited Internal_Execute;

  vrProcess := TProcess.Create(nil);
  try
    vrProcess.Executable := Self.Item.Param;

    vrProcess.Options := vrProcess.Options + [poNewConsole, poUsePipes];

    vrProcess.Execute;
  finally
    FreeAndNil(vrProcess);
  end;
end;

class function TJupiterRunnableItemOpen.ListAction: String;
begin
  Result := 'Open';
end;

end.

