unit JupiterDialogForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Controls, Forms, JupiterObject, JupiterVariableForm, JupiterAction,
  JupiterConsts, SysUtils, uCustomJupiterForm;

type

  { TJupiterDialogForm }

  TJupiterDialogForm = class(TJupiterObject)
  private
    FTitle       : String;
    FHint        : String;
    FFields      : TJupiterVariableFormList;
    FCurrentForm : TFCustomJupiterForm;
    FOnlyShow    : Boolean;

    procedure Internal_BtnClick(Sender: TObject);
  published
    property Fields   : TJupiterVariableFormList read FFields   write FFields;
    property Title    : String                   read FTitle    write FTitle;
    property Hint     : String                   read FHint     write FHint;
    property OnlyShow : Boolean                  read FOnlyShow write FOnlyShow;
  public
    function Show : Boolean;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses LCLType;

{ TJupiterDialogForm }

procedure TJupiterDialogForm.Internal_BtnClick(Sender: TObject);
begin
  try
    Self.Fields.Validate;

    Self.FCurrentForm.ModalResult := mrOK;
  except
    Application.MessageBox(PAnsiChar(Exception(ExceptObject).Message), PAnsiChar(Self.Title), MB_ICONERROR + MB_OK);
  end;
end;

function TJupiterDialogForm.Show: Boolean;
begin
  Result := False;

  Application.CreateForm(TFCustomJupiterForm, FCurrentForm);
  try
    FCurrentForm.IsModal := True;
    FCurrentForm.Caption                 := Self.Title;

    FCurrentForm.FormGenerator.Variables.CopyFromVariableList(Self.Fields);
    FCurrentForm.FormGenerator.Variables.CopyList := Self.Fields;

    FCurrentForm.Hint                    := Self.Hint;
    FCurrentForm.Height                  := Round(Screen.Height / 2);
    FCurrentForm.Width                   := Round(Screen.Width / 2);

    if not Self.OnlyShow then
    begin
      FCurrentForm.Actions.Add(TJupiterAction.Create('Salvar', @Internal_BtnClick));

      with TJupiterAction(FCurrentForm.Actions.GetLastObject) do
      begin
        Hint := 'Clique aqui para salvar';
        Icon := ICON_SAVE;
      end;
    end;

    Result := FCurrentForm.ShowModal = mrOK;
  finally
    FCurrentForm.Release;
    FreeAndNil(FCurrentForm);
  end;
end;

constructor TJupiterDialogForm.Create;
begin
  Self.FFields   := TJupiterVariableFormList.Create;

  Self.FTitle    := EmptyStr;
  Self.FHint     := EmptyStr;
  Self.FOnlyShow := False;
end;

destructor TJupiterDialogForm.Destroy;
begin
  FreeAndNil(Self.FFields);

  inherited Destroy;
end;

end.

