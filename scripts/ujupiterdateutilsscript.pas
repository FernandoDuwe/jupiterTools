unit uJupiterDateUtilsScript;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, jupiterScript, JupiterConsts, JupiterVariable, jupiterStringUtils,
  SysUtils, PascalScript, uPSComponent, SQLDB;

type

  { TJupiterDateUtilsScript }

  TJupiterDateUtilsScript = class(TJupiterScriptLibrary)
  protected
    function Internal_GetName : String; override;
  public
    procedure DoCompile(prSender: TPSScript); override;
    function AnalyseCode: TJupiterScriptAnalyserList; override;
  end;

  function JupiterDateUtilsScript_GetCurrentDate : TDate;
  function JupiterDateUtilsScript_GetCurrentTime : TTime;
  function JupiterDateUtilsScript_GetElapsedCurrentTaskTime : TDateTime;
  function JupiterDateUtilsScript_GetCurrentDateAsString : String;
  function JupiterDateUtilsScript_GetCurrentTimeAsString : String;
  function JupiterDateUtilsScript_GetElapsedCurrentTaskTimeAsString : String;

implementation

uses JupiterApp;

function JupiterDateUtilsScript_GetCurrentDate: TDate;
begin
  Result := Now;
end;

function JupiterDateUtilsScript_GetCurrentTime: TTime;
begin
  Result := Now;
end;

function JupiterDateUtilsScript_GetElapsedCurrentTaskTime: TDateTime;
var
  vrQry : TSQLQuery;
begin
  Result := 0.0;

  if vrJupiterApp.Params.VariableById('Tools.Tasks.Current.ID').Value = EmptyStr then
    Exit;

  vrQry := vrJupiterApp.NewWizard.NewQuery;
  try
    vrQry.SQL.Add(' SELECT * FROM TEMPOS WHERE TAREFA = ' + vrJupiterApp.Params.VariableById('Tools.Tasks.Current.ID').Value);
    vrQry.Open;

    while not vrQry.EOF do
    begin
      if vrQry.FieldByName('FIM').IsNull then
        Result := Result + (Now - vrQry.FieldByName('INICIO').AsDateTime)
      else
        Result := Result + (vrQry.FieldByName('FIM').AsDateTime - vrQry.FieldByName('INICIO').AsDateTime);

      vrQry.Next;
    end;
  finally
    vrQry.Close;
    FreeAndNil(vrQry);
  end;
end;

function JupiterDateUtilsScript_GetCurrentDateAsString: String;
begin
  Result := FormatDateTime('dd/mm/yyyy', JupiterDateUtilsScript_GetCurrentDate);
end;

function JupiterDateUtilsScript_GetCurrentTimeAsString: String;
begin
  Result := FormatDateTime('hh:nn:ss', JupiterDateUtilsScript_GetCurrentTime);
end;

function JupiterDateUtilsScript_GetElapsedCurrentTaskTimeAsString: String;
begin
  Result := FormatDateTime('hh:nn:ss', JupiterDateUtilsScript_GetElapsedCurrentTaskTime);
end;

{ TJupiterDateUtilsScript }

function TJupiterDateUtilsScript.Internal_GetName: String;
begin
  Result := 'Jupiter.DateUtilsScript';
end;

procedure TJupiterDateUtilsScript.DoCompile(prSender: TPSScript);
begin
  inherited DoCompile(prSender);

//  prSender.AddFunction(@JupiterDateUtilsScript_GetCurrentDate, 'function GetCurrentDate : TDate;');
//  prSender.AddFunction(@JupiterDateUtilsScript_GetCurrentTime, 'function GetCurrentTime : TTime;');

  prSender.AddFunction(@JupiterDateUtilsScript_GetCurrentDateAsString, 'function GetCurrentDateAsString : String;');
  prSender.AddFunction(@JupiterDateUtilsScript_GetCurrentTimeAsString, 'function GetCurrentTimeAsString : String;');

  prSender.AddFunction(@JupiterDateUtilsScript_GetElapsedCurrentTaskTimeAsString, 'function GetElapsedCurrentTaskTimeAsString : String;');
end;

function TJupiterDateUtilsScript.AnalyseCode: TJupiterScriptAnalyserList;
begin
  Result := inherited AnalyseCode;

//  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function GetCurrentDate : TDate;'));
//  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function GetCurrentTime : TTime;'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function GetCurrentDateAsString : String;'));
  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function GetCurrentTimeAsString : String;'));

  Result.AddItem(TJupiterScriptAnalyserItem.Create(NULL_KEY, NULL_KEY, jsaFunction, 'function GetElapsedCurrentTaskTimeAsString : String;'));
end;

end.

