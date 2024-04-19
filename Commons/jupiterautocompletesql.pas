unit jupiterautocompletesql;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterConsts, JupiterObject;

type

  { TJupiterAutoCompleteSQL }

  TJupiterAutoCompleteSQLOnChangeAutoCompleteList = procedure(prNewList : TStrings) of object;

  TJupiterAutoCompleteSQL = class(TJupiterObject)
  private
    FTableList      : TStrings;
    FScript         : TStrings;
    FOnAutoComplete : TJupiterAutoCompleteSQLOnChangeAutoCompleteList;
  published
    property OnAutoComplete : TJupiterAutoCompleteSQLOnChangeAutoCompleteList read FOnAutoComplete write FOnAutoComplete;
  public
    procedure AddTable(prTableName : String);

    procedure SetScript(prScript : TStrings);

    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TJupiterAutoCompleteSQL }

procedure TJupiterAutoCompleteSQL.AddTable(prTableName: String);
begin
  Self.FTableList.Add(prTableName);
end;

procedure TJupiterAutoCompleteSQL.SetScript(prScript: TStrings);
begin
  Self.FScript.Clear;
  Self.FScript.AddStrings(prScript);
end;

constructor TJupiterAutoCompleteSQL.Create;
begin
  Self.FTableList := TStringList.Create;
  Self.FTableList.Clear;

  Self.FScript := TStringList.Create;
  Self.FScript.Clear;
end;

destructor TJupiterAutoCompleteSQL.Destroy;
begin
  Self.FTableList.Clear;
  FreeAndNil(Self.FTableList);

  Self.FScript.Clear;
  FreeAndNil(Self.FScript);

  inherited Destroy;
end;

end.

