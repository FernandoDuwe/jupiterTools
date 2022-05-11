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
      property ID         : String      read Internal_GetIdentifier;
      property JupiterApp : TJupiterApp read FJupiterApp write FJupiterApp;
    public
      constructor Create(prJupiterApp : TJupiterApp);

      procedure GetTasks(var prTreeMenu : TTreeView); virtual;

      procedure ListItems(prParams : TJupiterListem; var prList : TList); virtual;
      procedure RunListable(prParams : TJupiterListableItem); virtual;
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

procedure TJupiterModule.ListItems(prParams : TJupiterListem; var prList: TList);
begin
  //
end;

end.

