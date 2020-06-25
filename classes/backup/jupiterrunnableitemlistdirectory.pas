unit JupiterRunnableItemListDirectory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JupiterRunnableItem, jupiterconsts, Dos;

type

  { TJupiterRunnableItemListDirectory }

  TJupiterRunnableItemListDirectory = class(TJupiterRunnableItem)
  public
    procedure Internal_Execute; override;

    class function ListAction : String; override; static;
  end;

implementation

{ TJupiterRunnableItemListDirectory }

procedure TJupiterRunnableItemListDirectory.Internal_Execute;
var
  vrSearchResult : SearchRec;
  vrAttribute    : Word;
begin
  inherited Internal_Execute;

  vrAttribute := readonly;

  if Pos('DIRECTORIES', Self.Param.Flags) > 0 then
     vrAttribute := vrAttribute or directory;

  if Pos('FILES', Self.Param.Flags) > 0 then
     vrAttribute := vrAttribute or archive;

  if Pos('HIDDENFILES', Self.Param.Flags) > 0 then
     vrAttribute := vrAttribute or hidden;

  FindFirst(Self.Param.Params + Self.Param.Filter, vrAttribute, vrSearchResult);

  while (DosError = 0) do
  begin
    if ((vrSearchResult.Name <> '.') and (vrSearchResult.Name <> '..')) then
    begin
      if DirectoryExists(Self.Param.OptionPath + vrSearchResult.Name) then
          Self.Internal_AddItem(TJupiterListItem.Create(vrSearchResult.Name, Self.Param.Params + vrSearchResult.Name, EmptyStr, JUPITER_ICON_DIRECTORY))
      else
          Self.Internal_AddItem(TJupiterListItem.Create(vrSearchResult.Name, Self.Param.Params + vrSearchResult.Name, EmptyStr, JUPITER_ICON_FILE));
    end;

    FindNext(vrSearchResult);
  end;

  FindClose(vrSearchResult);
end;

class function TJupiterRunnableItemListDirectory.ListAction: String;
begin
  Result := 'ListDirectory';
end;

end.

