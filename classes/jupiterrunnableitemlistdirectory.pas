unit JupiterRunnableItemListDirectory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JupiterRunnableItem, jupiterconsts, Dos;

type

  { TJupiterRunnableItemListDirectory }

  TJupiterRunnableItemListDirectory = class(TJupiterRunnableItem)
  private
    procedure Internal_ReadDirectory(prPath : String);
  public
    procedure Internal_Execute; override;

    class function ListAction : String; override; static;
  end;

implementation

{ TJupiterRunnableItemListDirectory }

procedure TJupiterRunnableItemListDirectory.Internal_ReadDirectory(prPath: String);
var
  vrSearchResult : SearchRec;
  vrAttribute    : Word;
begin
    vrAttribute := readonly;

    if ((Pos('-DIRECTORIES', Self.Param.Flags) > 0) or (Pos('-RECURSIVE', Self.Param.Flags) > 0)) then
       vrAttribute := vrAttribute or directory;

    if Pos('-FILES', Self.Param.Flags) > 0 then
       vrAttribute := vrAttribute or archive;

    if Pos('-HIDDEN', Self.Param.Flags) > 0 then
       vrAttribute := vrAttribute or hidden;

    FindFirst(prPath + Self.Param.Filter, vrAttribute, vrSearchResult);

    while (DosError = 0) do
    begin
      if ((vrSearchResult.Name <> '.') and (vrSearchResult.Name <> '..')) then
      begin
        if ((DirectoryExists(prPath + vrSearchResult.Name)) and (Pos('-DIRECTORIES', Self.Param.Flags) > 0)) then
        begin
          if Self.Internal_HasFlag('-HASFILEINDIR') then
          begin
            if FileExists(prPath + vrSearchResult.Name + '/' + Self.Internal_GetFlagParam('-HASFILEINDIR')) then
              Self.Internal_AddItem(TJupiterListItem.Create(vrSearchResult.Name, prPath + vrSearchResult.Name, EmptyStr, JUPITER_ICON_DIRECTORY));
          end
          else
            Self.Internal_AddItem(TJupiterListItem.Create(vrSearchResult.Name, prPath + vrSearchResult.Name, EmptyStr, JUPITER_ICON_DIRECTORY));
        end;

        if ((FileExists(prPath + vrSearchResult.Name)) and (Pos('-FILES', Self.Param.Flags) > 0)) then
          Self.Internal_AddItem(TJupiterListItem.Create(vrSearchResult.Name, prPath + vrSearchResult.Name, EmptyStr, JUPITER_ICON_FILE));

        if ((DirectoryExists(prPath + vrSearchResult.Name)) and (Pos('-RECURSIVE', Self.Param.Flags) > 0)) then
          Self.Internal_ReadDirectory(prPath + vrSearchResult.Name + '/');
      end;

      FindNext(vrSearchResult);
    end;

    FindClose(vrSearchResult);
end;

procedure TJupiterRunnableItemListDirectory.Internal_Execute;
begin
  inherited Internal_Execute;

  Self.Internal_ReadDirectory(Self.Param.Params);
end;

class function TJupiterRunnableItemListDirectory.ListAction: String;
begin
  Result := 'ListDirectory';
end;

end.

