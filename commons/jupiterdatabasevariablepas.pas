unit JupiterDatabaseVariablepas;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterVariable;

type
  TJupiterDatabaseVariable = class(TJupiterVariable)
  private
    FID : Integer;

    procedure Internal_SetValue(prNewValue : String); virtual;
  published
    property ID : Integer read FID write FID;
  end;

implementation

end.

