unit uuserpreferences;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  uCustomJupiterForm, JupiterAction, JupiterConsts, JupiterApp,
  JupiterStandardModule;

type

  { TFUserPreferences }

  TFUserPreferences = class(TFCustomJupiterForm)
    cbCompactMode: TCheckBox;
    cbShowActionsInForm: TCheckBox;
    cbHideMenuTree: TCheckBox;
    edStartRoute: TEdit;
    Label2: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    pnCompactMode: TPanel;
    pnShowActionsInForm: TPanel;
    pnHideMenuTree: TPanel;
  private
    procedure Internal_SaveActionClick(Sender : TObject);

    procedure Internal_PrepareForm; override;
  public

  end;

var
  FUserPreferences: TFUserPreferences;

implementation

{$R *.lfm}

{ TFUserPreferences }

procedure TFUserPreferences.Internal_SaveActionClick(Sender: TObject);
var
  vrStr : String;
begin
  vrStr := EmptyStr;

  if cbCompactMode.Checked then
    vrStr := vrStr + ' #COMPACTMODE ';

  if cbShowActionsInForm.Checked then
    vrStr := vrStr + ' #SHOWACTIONSINFORM ';

  if cbHideMenuTree.Checked then
    vrStr := vrStr + ' #HIDEMENUTREE ';

  TJupiterStandardModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Standard')).SetUserPreferences(vrStr);

  TJupiterStandardModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Standard')).SetUserStartRoute(edStartRoute.Text);

  Self.Close;
end;

procedure TFUserPreferences.Internal_PrepareForm;
begin
  inherited Internal_PrepareForm;

  Self.Hint := 'Preferências do usuário';

  Self.Actions.Add(TJupiterAction.Create('Salvar', @Internal_SaveActionClick));

  with TJupiterAction(Self.Actions.GetLastObject) do
  begin
    Hint := 'Clique aqui para salvar a ação';
    Icon := ICON_SAVE;
  end;

  cbCompactMode.Checked := TJupiterStandardModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Standard')).CompactMode;

  cbShowActionsInForm.Checked := TJupiterStandardModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Standard')).ShowActionsInForm;

  cbHideMenuTree.Checked := TJupiterStandardModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Standard')).HideMenuTree;

  edStartRoute.Text := TJupiterStandardModule(vrJupiterApp.ModulesList.GetModuleById('Jupiter.Standard')).GetUserStartRoute;
end;

end.

