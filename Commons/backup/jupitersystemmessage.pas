unit JupiterSystemMessage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterConsts, JupiterObject;

type

  { TJupiterSystemMessage }

  TJupiterSystemMessage = class(TJupiterObject)
  private
    FTitle     : String;
    FOrigin    : String;
    FTimestamp : TDateTime;
    FDetails   : TStrings;

    procedure Internal_SetDetails(prDetails : TStrings);
  published
    property Title     : String    read FTitle;
    property Origin    : String    read FOrigin;
    property Timestamp : TDateTime read FTimestamp;
    property Details   : TStrings  read FDetails write Internal_SetDetails;
  public
    constructor Create(prTitle, prOrigin, prDetails : String);
    destructor Destroy; override;
  end;

implementation

{ TJupiterSystemMessage }

procedure TJupiterSystemMessage.Internal_SetDetails(prDetails: TStrings);
var
  vrVez : Integer;
begin
  Self.FDetails.Clear;

  for vrVez := 0 to prDetails.Count - 1 do
    Self.FDetails.Add(prDetails[vrVez]);
end;

constructor TJupiterSystemMessage.Create(prTitle, prOrigin, prDetails: String);
begin
  Self.FTitle     := prTitle;
  Self.FOrigin    := prOrigin;
  Self.FTimestamp := Now;

  Self.FDetails := TStringList.Create;
  Self.FDetails.Clear;

  if prDetails <> EmptyStr then
    Self.FDetails.Add(prDetails);
end;

end.

