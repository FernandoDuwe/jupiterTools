unit JupiterFileDataProvider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterDataProvider, JupiterConsts;

type

  { TJupiterFileDataProvider }

  TJupiterFileDataProvider = class(TJupiterDataProvider)
  private
    FPath       : String;
    FSubFolders : Boolean;
  protected
    procedure Internal_Search(prPath : String); virtual;
  published
    property Path       : String  read FPath       write FPath;
    property SubFolders : Boolean read FSubFolders write FSubFolders;
  public
    procedure ProvideData; override;
  end;

implementation

{ TJupiterFileDataProvider }

procedure TJupiterFileDataProvider.Internal_Search(prPath: String);
var
  vrInfo : TSearchRec;
begin
  if FindFirst(prPath + '*', faAnyFile, vrInfo) = 0 then
    repeat
      if (((vrInfo.Name = '.') or (vrInfo.Name = '..')) or (vrInfo.Name = EmptyStr)) then
        Continue;

      if DirectoryExists(prPath + vrInfo.Name) then
        if Self.SubFolders then
          Self.Internal_Search(Format('%0:s%1:s%2:s', [prPath, vrInfo.Name, GetDirectorySeparator]))
        else
          Continue;

      if not FileExists(prPath + vrInfo.Name) then
        Continue;

      Self.AddRow;
      Self.GetLastRow.Fields.AddVariable('FieldName', vrInfo.Name, 'Nome do Arquivo');
      Self.GetLastRow.Fields.AddVariable('File', Format('%0:s%1:s', [prPath, vrInfo.Name, GetDirectorySeparator]), 'Arquivo');
      Self.GetLastRow.Fields.AddVariable('Extension', ExtractFileExt(vrInfo.Name), 'Extensão');
    until FindNext(vrInfo) <> 0;

  FindClose(vrInfo);
end;

procedure TJupiterFileDataProvider.ProvideData;
begin
  inherited ProvideData;

  if ((Trim(Self.Path) = EmptyStr) or (not DirectoryExists(Self.FPath))) then
     raise Exception.Create('Path must be valid');

  Self.Internal_Search(Self.Path);
end;

end.

