unit uNewTask;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  uJupiterForm, jupiterformutils;

type

  { TFNewTask }

  TFNewTask = class(TFJupiterForm)
    lvExplorer: TListView;
    splDivider: TSplitter;
    tvTreeMenu: TTreeView;
  private
    procedure Internal_UpdateComponents; override;
  public

  end;

var
  FNewTask: TFNewTask;

implementation

{$R *.lfm}

{ TFNewTask }

procedure TFNewTask.Internal_UpdateComponents;
begin
  inherited Internal_UpdateComponents;

  tvTreeMenu.Width := PercentOfScreen(Self.Width, 30);
end;

end.

