unit JupiterDirectoryDataProvider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterDataProvider, JupiterConsts;

type
    { TJupiterDirectoryDataProvider }

    TJupiterDirectoryDataProvider = class(TJupiterDataProvider)
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

{ TJupiterDirectoryDataProvider }

procedure TJupiterDirectoryDataProvider.Internal_Search(prPath: String);
var
  vrInfo : TSearchRec;
begin
  if FindFirst(prPath + '*', faDirectory, vrInfo) = 0 then
    repeat
      if (((vrInfo.Name = '.') or (vrInfo.Name = '..')) or (vrInfo.Name = EmptyStr)) then
        Continue;

      if not DirectoryExists(prPath + vrInfo.Name) then
        Continue;

      Self.AddRow;
      Self.GetLastRow.Fields.AddVariable('Folder', vrInfo.Name, 'Pasta');
      Self.GetLastRow.Fields.AddVariable('Path', Format('%0:s%1:s%2:s', [Self.Path, vrInfo.Name, GetDirectorySeparator]), 'Caminho');
    until FindNext(vrInfo) <> 0;

  FindClose(vrInfo);
end;

procedure TJupiterDirectoryDataProvider.ProvideData;
begin
  inherited ProvideData;

  if ((Trim(Self.Path) = EmptyStr) or (not DirectoryExists(Self.FPath))) then
     raise Exception.Create('Path must be valid');

  Self.Internal_Search(Self.Path);
end;

end.

