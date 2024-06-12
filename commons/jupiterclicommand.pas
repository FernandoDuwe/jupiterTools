unit jupiterclicommand;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterConsts, JupiterObject, JupiterEnviroment,
  JupiterCSVDataProvider;

type

   { TJupiterCLICommandParam }

   TJupiterCLICommandParam = class(TJupiterObject)
   private
     FParamName : String;
     FRequired : Boolean;
     FValue : String;
   published
     property ParamName : String  read FParamName write FParamName;
     property Required  : Boolean read FRequired  write FRequired;
     property Value     : String  read FValue     write FValue;
   public
     function ToString() : String;
     procedure FromString(prLine : String);
   end;

   { TJupiterCLICommand }

   TJupiterCLICommand = class(TJupiterObject)
   private
     FCommandName : String;
     FCommand     : String;
     FCommandText : TStrings;
     FParamList   : TJupiterObjectList;
     FFileName    : String;

     procedure Internal_SetCommandText(Value : TStrings);
   published
     property CommandName : String   read FCommandName write FCommandName;
     property Command     : String   read FCommand     write FCommand;
     property CommandText : TStrings read FCommandText write Internal_SetCommandText;
     property FileName    : String   read FFileName    write FFileName;

     property ParamList : TJupiterObjectList read FParamList write FParamList;
   public
     procedure AddParam(prParamName : String; prRequired : Boolean);

     procedure LoadFromFile;
     procedure SaveToFile;

     constructor Create;
     destructor Destroy; override;
   end;

implementation

uses StrUtils;

{ TJupiterCLICommandParam }

function TJupiterCLICommandParam.ToString: String;
begin
  Result := EmptyStr;

  if Required then
    Result := Format('%0:s;%1:s;', [Self.ParamName, '1'])
  else
    Result := Format('%0:s;%1:s;', [Self.ParamName, '0']);
end;

procedure TJupiterCLICommandParam.FromString(prLine: String);
begin
  Self.ParamName := GetCSVColumn(prLine, 0);
  Self.Required  := GetCSVColumn(prLine, 1) = '1';

  Self.Value := EmptyStr;
end;

{ TJupiterCLICommand }

procedure TJupiterCLICommand.Internal_SetCommandText(Value: TStrings);
begin
  Self.FCommandText.Clear;
  Self.FCommandText.AddStrings(Value);
end;

procedure TJupiterCLICommand.AddParam(prParamName: String; prRequired: Boolean);
var
  vrParam : TJupiterCLICommandParam;
begin
  vrParam := TJupiterCLICommandParam.Create;
  vrParam.ParamName := prParamName;
  vrParam.Required  := prRequired;

  Self.FParamList.Add(vrParam);
end;

procedure TJupiterCLICommand.LoadFromFile;
var
  vrStr : TStrings;
  vrVez : Integer;
  vrCurrentTag : Integer;
  vrParam : TJupiterCLICommandParam;
begin
  vrCurrentTag := NULL_KEY;

  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.LoadFromFile(Self.FileName);

    for vrVez := 0 to vrStr.Count - 1 do
    begin
      if Trim(vrStr[vrVez]) = EmptyStr then
        Continue;

      if Trim(vrStr[vrVez]) = '@name' then
      begin
        vrCurrentTag := 0;
        Continue;
      end;

      if Trim(vrStr[vrVez]) = '@command' then
      begin
        vrCurrentTag := 1;
        Continue;
      end;

      if Trim(vrStr[vrVez]) = '@parameters' then
      begin
        vrCurrentTag := 2;
        Continue;
      end;

      if Trim(vrStr[vrVez]) = '@commandCode' then
      begin
        vrCurrentTag := 3;
        Continue;
      end;

      case vrCurrentTag of
         0: Self.CommandName := Trim(vrStr[vrVez]);
         1: Self.Command := Trim(vrStr[vrVez]);
         2: begin
              vrParam := TJupiterCLICommandParam.Create;
              vrParam.FromString(vrStr[vrVez]);

              Self.ParamList.Add(vrParam);
            end;
         3: Self.CommandText.Add(vrStr[vrVez]);
      end;
    end;
  finally
    vrStr.Clear;
    FreeAndNil(vrStr);
  end;
end;

procedure TJupiterCLICommand.SaveToFile;
var
  vrStr : TStrings;
  vrVez : Integer;
begin
  vrStr := TStringList.Create;
  try
    vrStr.Clear;
    vrStr.Add('@name');
    vrStr.Add(Self.CommandName);
    vrStr.Add(EmptyStr);

    vrStr.Add('@command');
    vrStr.Add(Self.Command);
    vrStr.Add(EmptyStr);

    vrStr.Add('@commandCode');
    vrStr.AddStrings(Self.CommandText);
    vrStr.Add(EmptyStr);

    vrStr.Add('@parameters');

    for vrVez := 0 to Self.ParamList.Count - 1 do
      with TJupiterCLICommandParam(Self.ParamList.GetAtIndex(vrVez)) do
        vrStr.Add(ToString());

    vrStr.Add(EmptyStr);
  finally
    vrStr.SaveToFile(Self.FileName);

    FreeAndNil(vrStr);
  end;
end;

constructor TJupiterCLICommand.Create;
var
  vrEnviroment : TJupiterEnviroment;
begin
  vrEnviroment := TJupiterEnviroment.Create;
  try
    Self.FFileName := vrEnviroment.FullPath('/modules/CLI/' + FormatDateTime('ddmmyyyyhms', Now) + '.jcc');

    Self.FCommandName := EmptyStr;

    Self.FCommandText := TStringList.Create;
    Self.FCommandText.Clear;

    Self.FParamList := TJupiterObjectList.Create;
  finally
    FreeAndNil(vrEnviroment);
  end;
end;

destructor TJupiterCLICommand.Destroy;
begin
  FreeAndNil(Self.FParamList);

  Self.FCommandText.Clear;
  FreeAndNil(Self.FCommandText);

  inherited Destroy;
end;

end.

