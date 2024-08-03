unit jupiterStringUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

  function JupiterStringUtilsNormalizeToPresent(prText : String) : String;

implementation

function JupiterStringUtilsNormalizeToPresent(prText: String): String;
begin
  if prText = EmptyStr then
    Result := prText;

  prText    := AnsiLowerCase(prText);
  prText[1] := AnsiUpperCase(prText)[1];

  Result := prText;
end;

end.

