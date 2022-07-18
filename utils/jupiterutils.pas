unit jupiterUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, SysUtils;

  procedure NovaChecklist(prFile : String);
  procedure NovoScriptSQL(prFile : String);
  procedure NovoScriptBAT(prFile : String);

  procedure NovaChecklistInTask(prFile : String; prTaskPath : String);
  procedure NovoScriptSQLInTask(prFile : String; prTaskPath : String);
  procedure NovoScriptBATInTask(prFile : String; prTaskPath : String);

  function GetCSVColumn(prLine : String; prIndex : Integer) : String;

implementation

uses fileUtils, JupiterConsts;

procedure NovaChecklist(prFile : String);
var
  vrStr : TStrings;
begin
  prFile := prFile + '.ckl';
  prFile := TratarCaminho(ExtractFileDir(Application.ExeName) + '\modules\checklist\') + prFile;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add('Step;Checked;');

    vrStr.SaveToFile(prFile);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure NovoScriptSQL(prFile: String);
var
  vrStr : TStrings;
begin
  prFile := prFile + '.sql';
  prFile := TratarCaminho(ExtractFileDir(Application.ExeName) + '\modules\runner\') + prFile;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add(EmptyStr);

    vrStr.SaveToFile(prFile);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure NovoScriptBAT(prFile: String);
var
  vrStr : TStrings;
begin
  prFile := prFile + '.bat';
  prFile := TratarCaminho(ExtractFileDir(Application.ExeName) + '\modules\runner\') + prFile;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add(EmptyStr);

    vrStr.SaveToFile(prFile);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure NovaChecklistInTask(prFile: String; prTaskPath: String);
var
  vrStr : TStrings;
begin
  prFile := prFile + '.ckl';
  prFile := TratarCaminho(prTaskPath + '\Arquivos\') + prFile;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add('Step;Checked;');

    vrStr.SaveToFile(prFile);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure NovoScriptSQLInTask(prFile: String; prTaskPath: String);
var
  vrStr : TStrings;
begin
  prFile := prFile + '.sql';
  prFile := TratarCaminho(prTaskPath + '\Arquivos\') + prFile;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add(EmptyStr);

    vrStr.SaveToFile(prFile);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure NovoScriptBATInTask(prFile: String; prTaskPath: String);
var
  vrStr : TStrings;
begin
  prFile := prFile + '.bat';
  prFile := TratarCaminho(prTaskPath + '\Arquivos\') + prFile;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add(EmptyStr);

    vrStr.SaveToFile(prFile);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

function GetCSVColumn(prLine: String; prIndex: Integer): String;
var
  vrStr : TStrings;
begin
  Result := EmptyStr;

  if Trim(prLine) = EmptyStr then
    Exit;

  vrStr := TStringList.Create;
  try
    vrStr.Delimiter     := ';';
    vrStr.DelimitedText := StringReplace(prLine, ' ', EMPTY_SPACE_SEPARATOR, [rfIgnoreCase, rfReplaceAll]);

    if (vrStr.Count - 1) < prIndex then
      Exit;

    Result := vrStr[prIndex];
    Result := StringReplace(Result, EMPTY_SPACE_SEPARATOR, ' ', [rfIgnoreCase, rfReplaceAll]);
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

end.

