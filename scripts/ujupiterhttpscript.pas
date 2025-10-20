unit uJupiterHTTPScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, jupiterScript, JupiterConsts, JupiterVariable, jupiterStringUtils,
  SysUtils, PascalScript, uPSComponent, SQLDB, FPHTTPClient;

type

  { TJupiterDateUtilsScript }

  { TJupiterHTTPScript }

  TJupiterHTTPScript = class(TJupiterScriptLibrary)
  protected
    function Internal_GetName : String; override;
  public
    procedure DoCompile(prSender: TPSScript); override;
    function AnalyseCode: TJupiterScriptAnalyserList; override;
  end;

  function JupiterHTTPScript_Get(prURL : String; var prResponseCode : Integer; var prResponseBody : String) : Boolean;

implementation

function JupiterHTTPScript_Get(prURL: String; var prResponseCode: Integer; var prResponseBody: String): Boolean;
var
  vrResponse : TStringStream;
begin
  vrResponse := TStringStream.Create(EmptyStr);
  try
    TFPHTTPClient.SimpleGet(prURL, vrResponse);

    prResponseCode := 200;

    prResponseBody := vrResponse.DataString;

    Result := True;
  finally
    vrResponse.Free;
  end;
end;

{ TJupiterHTTPScript }

function TJupiterHTTPScript.Internal_GetName: String;
begin
  Result := 'Jupiter.HTTPScript';
end;

procedure TJupiterHTTPScript.DoCompile(prSender: TPSScript);
begin
  inherited DoCompile(prSender);

  prSender.AddFunction(@JupiterHTTPScript_Get, 'function HTTPGet(prURL : String; var prResponseCode : Integer; var prResponseBody : String) : Boolean;');
end;

function TJupiterHTTPScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result := inherited AnalyseCode;

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function HTTPGet(prURL : String; var prResponseCode : Integer; var prResponseBody : String) : Boolean;'));
end;

end.

