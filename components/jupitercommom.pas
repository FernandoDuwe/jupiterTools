{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit jupiterCommom;

{$warn 5023 off : no warning about unused units}
interface

uses
  JupiterFormTab, JupiterFormTabSheet, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('JupiterFormTab', @JupiterFormTab.Register);
  RegisterUnit('JupiterFormTabSheet', @JupiterFormTabSheet.Register);
end;

initialization
  RegisterPackage('jupiterCommom', @Register);
end.
