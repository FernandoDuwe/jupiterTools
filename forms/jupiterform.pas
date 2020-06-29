unit JupiterForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, jupiterconsts,
  JupiterRunnableItemListDirectory, JupiterParams, JupiterRunnableItem,
  JupiterRunnableItemListFromFile, jupiter;

type

  { TJupiterForm }

  TJupiterForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
     procedure Internal_Prepare;          virtual;
     procedure Internal_UpdateDatasets;   virtual;
     procedure Internal_UpdateComponents; virtual;
     procedure Internal_UpdateCalcFields; virtual;

     // Events
     procedure Internal_ItemChangeStatus(prSender : TObject; prStatus : TJupiterRunnableItemStatus); virtual;
     procedure Internal_ItemAddItem(prSender : TObject; prItem : TJupiterListItem); virtual;
  public
     procedure UpdateForm;

     function ActionFactory(prParam : TJupiterAction; prItem : TJupiterListItem) : TJupiterRunnableItem;
  end;

var
  FJupiterForm: TJupiterForm;

implementation

{$R *.lfm}

{ TFJupiterForm }

procedure TJupiterForm.FormCreate(Sender: TObject);
begin
  Self.Internal_Prepare;
end;

procedure TJupiterForm.FormShow(Sender: TObject);
begin
  Self.UpdateForm;
end;

procedure TJupiterForm.Internal_Prepare;
begin
  //
end;

procedure TJupiterForm.Internal_UpdateDatasets;
begin
  //
end;

procedure TJupiterForm.Internal_UpdateComponents;
begin
  //
end;

procedure TJupiterForm.Internal_UpdateCalcFields;
begin
  //
end;

procedure TJupiterForm.Internal_ItemChangeStatus(prSender: TObject; prStatus: TJupiterRunnableItemStatus);
begin
  //
end;

procedure TJupiterForm.Internal_ItemAddItem(prSender: TObject; prItem: TJupiterListItem);
begin
  //
end;

procedure TJupiterForm.UpdateForm;
begin
  try
    Self.Internal_UpdateDatasets;
  finally
    Self.Internal_UpdateComponents;
    Self.Internal_UpdateCalcFields;
  end;
end;

function TJupiterForm.ActionFactory(prParam: TJupiterAction; prItem : TJupiterListItem): TJupiterRunnableItem;
var
  vrObj : TJupiter;
begin
  Result := nil;

  vrObj := TJupiter.Create;
  try
    if not Assigned(prParam) then
        Result := vrObj.CreateRunnable('open')
    else
        Result := vrObj.CreateRunnable(prParam.RunnableAction);

    if Assigned(prParam) then
        Result.Param := prParam;

    if Assigned(prItem) then
        Result.Item := prItem;

    Result.OnAddItem      := @Self.Internal_ItemAddItem;
    Result.OnChangeStatus := @Self.Internal_ItemChangeStatus;

    Result.Start;
  finally
    FreeAndNil(vrObj);
  end;
end;

end.

