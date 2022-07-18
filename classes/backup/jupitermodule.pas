unit JupiterModule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterApp, ComCtrls, JupiterConsts;

type

    { TJupiterModule }

    TJupiterModule = class(TObject)
    private
      FModuleId    : String;
      FModuleTitle : String;
      FJupiterApp  : TJupiterApp;
    protected
      procedure Internal_Initialize; virtual;
      function  Internal_GetIdentifier: String; virtual;
    published
      property AppName    : String      read FAppName;
      property ID         : String      read Internal_GetIdentifier;
      property JupiterApp : TJupiterApp read FJupiterApp write FJupiterApp;
    public
      constructor Create(prJupiterApp : TJupiterApp);

      procedure GetTasks(var prTreeMenu : TTreeView); virtual;

      procedure ListItems(var prParams : TJupiterListem; var prList : TList); virtual;
      procedure ListActions(prParams : TJupiterListem; var prList : TList); virtual;
      procedure RunListable(var prParamsItem: TJupiterListem; var prParams : TJupiterListableItem); virtual;
    end;

implementation

{ TJupiterModule }

procedure TJupiterModule.Internal_Initialize;
begin
  //
end;

function TJupiterModule.Internal_GetIdentifier: String;
begin
  Result := EmptyStr;
end;

constructor TJupiterModule.Create(prJupiterApp : TJupiterApp);
begin
  Self.JupiterApp := prJupiterApp;

  Self.Internal_Initialize;
end;

procedure TJupiterModule.GetTasks(var prTreeMenu: TTreeView);
begin
  //
end;

procedure TJupiterModule.ListItems(var prParams : TJupiterListem; var prList: TList);
begin
  //
end;

procedure TJupiterModule.ListActions(prParams: TJupiterListem; var prList: TList);
var
  vrUpdateAction : TJupiterAction;
begin
  vrUpdateAction            := TJupiterAction.Create;
  vrUpdateAction.Title      := 'Atualizar';
  vrUpdateAction.ImageIndex := ICON_UPDATE;
  vrUpdateAction.Hint       := 'Clique aqui para atualizar o formulário';

  prList.Add(vrUpdateAction);
end;

procedure TJupiterModule.RunListable(var prParamsItem: TJupiterListem; var prParams: TJupiterListableItem);
begin
  //
end;

end.

