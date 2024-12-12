unit JupiterDBLookupComboBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBCtrls;

type
  TJupiterDBLookupComboBox = class(TDBLookupComboBox)
  private

  protected

  public

  published

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Data Controls',[TJupiterDBLookupComboBox]);
end;

end.
