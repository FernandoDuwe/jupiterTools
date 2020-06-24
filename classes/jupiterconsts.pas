unit jupiterconsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  JUPITER_ICON_DIRECTORY : Integer = 0;
  JUPITER_ICON_FILE      : Integer = 1;
  JUPITER_ICON_BATCH     : Integer = 2;
  JUPITER_ICON_DOCKER    : Integer = 3;

type
  // Controls a thread status
  TJupiterRunnableItemStatus = (jrsNotStarted, jrsRunning, jrsDone, jrsError);

  // Use to monitor the change of status on a thread
  TJupiterRunnableItemChangeStatus = procedure(prSender : TObject; prStatus : TJupiterRunnableItemStatus) of object;

  { TJupiterListItem }

  TJupiterListItem = class(TObject)
  public
    Title       : String;
    Description : String;
    Param       : String;
    Icon        : Integer;

    constructor Create(prTitle, prDescription, prParam : String; prIcon : Integer);
  end;

  // Use to add a item to a form list
  TJupiterRunnableItemAddItem = procedure(prSender : TObject; prItem : TJupiterListItem) of object;

implementation

{ TJupiterListItem }

constructor TJupiterListItem.Create(prTitle, prDescription, prParam: String; prIcon: Integer);
begin
  Self.Title       := prTitle;
  Self.Description := prDescription;
  Self.Param       := prParam;
  Self.Icon        := prIcon;
end;

end.

