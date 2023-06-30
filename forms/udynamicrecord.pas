unit uDynamicRecord;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, uCustomJupiterForm,
  JupiterDataProvider;

type

  { TFDynamicRecord }

  TFDynamicRecord = class(TFCustomJupiterForm)
  private
     FProvider      : TJupiterDataProvider;
     FCurrentRecord : TJupiterDataProviderRow;

     procedure Internal_SetCurrentRecord(prNewRecord : TJupiterDataProviderRow);
  published
     property CurrentRecord : TJupiterDataProviderRow read FCurrentRecord write Internal_SetCurrentRecord;
     property Provider      : TJupiterDataProvider    read FProvider      write FProvider;
  public

  end;

var
  FDynamicRecord: TFDynamicRecord;

implementation

{$R *.lfm}

{ TFDynamicRecord }

procedure TFDynamicRecord.Internal_SetCurrentRecord(prNewRecord: TJupiterDataProviderRow);
begin
  Self.FCurrentRecord := prNewRecord;

  Self.Generator.Fields.CopyFromVariableList(prNewRecord.Fields);
end;

end.

