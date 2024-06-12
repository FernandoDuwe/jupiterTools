unit jupiterfiledownloader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TOnProgress = procedure(Sender: TObject; Percent: integer) of object;

  { TStreamAdapter }

  TStreamAdapter = class(TStream)
  strict private
    FOnProgress: TOnProgress;
    FPercent: integer;
    FStream: TStream;
  public
    constructor Create(AStream: TStream; ASize: int64);
    destructor  Destroy; override;
    function    Read(var Buffer; Count: longint): longint; override;
    function    Write(const Buffer; Count: longint): longint; override;
    function    Seek(Offset: longint; Origin: word): longint; override;
    procedure   DoProgress; virtual;
  published
    property OnProgress: TOnProgress read FOnProgress write FOnProgress;
  end;

  procedure DonwloadFile(OnProgress: TOnProgress; const cUrl, cFile: UTF8String);

implementation

uses
  fphttpclient;

procedure DonwloadFile(OnProgress: TOnProgress; const cUrl, cFile: UTF8String);
// cUrl  = 'http://www.imagemagick.org/download/binaries/ImageMagick-6.8.6-8-Q8-x86-static.exe';
// cFile = 'ImageMagick-6.8.6-8-Q8-x86-static.exe';
var
  vStream: TStreamAdapter;
  vHTTP: TFPHTTPClient;
  vSize: int64 = 0;
  i: integer;
  s: string;
begin
  WriteLn('48');

  try
    vHTTP := TFPHTTPClient.Create(nil);

    WriteLn('53');

    vHTTP.HTTPMethod('HEAD', cUrl, nil, [200]);

    WriteLn('54');

    for i := 0 to pred(vHTTP.ResponseHeaders.Count) do
    begin
      WriteLn('58');

      s := UpperCase(vHTTP.ResponseHeaders[I]);

      if Pos('CONTENT-LENGTH:', s) > 0 then
      begin
        WriteLn('64');

        vSize := StrToIntDef(Copy(s, Pos(':', s) + 1, Length(s)), 0);
        Break;
      end;

      WriteLn('70');
    end;

    WriteLn('73');

    vStream := TStreamAdapter.Create(TFileStream.Create(cFile, fmCreate), vSize);
    vStream.OnProgress := OnProgress;

    WriteLn('78');

    vHTTP.HTTPMethod('GET', cUrl, vStream, [200]);
  finally
    vHTTP.Free;
    vStream.Free;
  end;
end;

{ TStreamAdapter }

constructor TStreamAdapter.Create(AStream: TStream; ASize: int64);
begin
  inherited Create;

  FStream := AStream;
  FStream.Size := ASize;
  FStream.Position := 0;
end;

destructor TStreamAdapter.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TStreamAdapter.Read(var Buffer; Count: longint): longint;
begin
  Result := FStream.Read(Buffer, Count);

  DoProgress;
end;

function TStreamAdapter.Write(const Buffer; Count: longint): longint;
begin
  Result := FStream.Write(Buffer, Count);

  DoProgress;
end;

function TStreamAdapter.Seek(Offset: longint; Origin: word): longint;
begin
  Result := FStream.Seek(Offset, Origin);
end;

procedure TStreamAdapter.DoProgress;
begin
  FPercent := Trunc((FStream.Position) / (FStream.Size) * 100);

  if Assigned(OnProgress) then
     OnProgress(Self, FPercent);
end;

end.

