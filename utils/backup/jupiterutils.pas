unit jupiterUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, SysUtils;

  procedure NovaChecklist(prFile : String);
  procedure NovoScriptSQL(prFile : String);
  procedure NovoScriptBAT(prFile : String);

implementation

uses fileUtils;

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

end.

