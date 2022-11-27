unit JupiterXMLDataProvider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, JupiterDataProvider, JupiterConsts, JupiterVariable,
  DOM, XMLRead, LazUTF8;

type

  { TJupiterXMLDataProvider }

  TJupiterXMLDataProvider = class(TJupiterDataProvider)
  private
    FFilename   : String;
    FSearchNode : String;
  protected
    procedure Internal_DoFill(prNode : TDOMNode; prSaveAsRow : Boolean = False);
  published
    property Filename   : String read FFilename   write FFilename;
    property SearchNode : String read FSearchNode write FSearchNode;
  public
    procedure ProvideData; override;
  end;

implementation

{ TJupiterXMLDataProvider }

procedure TJupiterXMLDataProvider.Internal_DoFill(prNode : TDOMNode; prSaveAsRow : Boolean = False);
var
  vrVez       : Integer;
  vrSaveAsRow : Boolean;
begin
  for vrVez := 0 to prNode.ChildNodes.Count - 1 do
  begin
    vrSaveAsRow := False;

    if ((not prSaveAsRow) and (Trim(Self.SearchNode) <> EmptyStr)) then
       vrSaveAsRow := AnsiUpperCase(prNode.ChildNodes[vrVez].NodeName) = AnsiUpperCase(Self.SearchNode);

    if vrSaveAsRow then
       Self.AddRow;

    if prSaveAsRow then
      with Self.GetLastRow do
        Fields.AddVariable(prNode.ChildNodes[vrVez].NodeName,
                           prNode.ChildNodes[vrVez].NodeValue,
                           prNode.ChildNodes[vrVez].NodeName);

    Self.Internal_DoFill(prNode.ChildNodes[vrVez], vrSaveAsRow);
  end;
end;

procedure TJupiterXMLDataProvider.ProvideData;
var
  vrDocumentXML : TXMLDocument;
begin
  if ((Trim(Self.Filename) = EmptyStr) or (not FileExists(Self.Filename))) then
     raise Exception.Create('Filename must be valid');

  ReadXMLFile(vrDocumentXML, UTF8ToSys(Self.Filename));
  try
    Self.Internal_DoFill(vrDocumentXML);
  finally
    FreeAndNil(vrDocumentXML);
  end;
end;

end.

