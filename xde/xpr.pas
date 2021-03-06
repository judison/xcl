(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
unit xpr;

{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}
{$H+}

interface

uses Classes, SysUtils, DOM;

type
  TXPRCustom = class
  private
    FParent: TXPRCustom;
  protected
    Node: TDOMElement;
  public
    constructor Create(AParent: TXPRCustom; ANode: TDOMElement); virtual;
    property Parent: TXPRCustom read FParent;
  end;

  TXPRSource = class(TXPRCustom)
  private
    function GetFile: String;
    procedure SetFile(AValue: String);
  protected
  public
    property FileName: String read GetFile write SetFile;
  end;

  TXPRUnit = class(TXPRSource)
  private
    function GetName: String;
    procedure SetName(AValue: String);
    function GetForm: String;
    procedure SetForm(AValue: String);
  protected
  public
    function HasForm: Boolean;
    //--
    property Name: String read GetName write SetName;
    property Form: String read GetForm write SetForm;
  end;

  TXPRInclude = class(TXPRSource)
  private
  protected
  public
  end;

  TXPRSources = class(TXPRCustom)
  private
    UnitList: TList;
    IncludeList: TList;
  protected
  public
    constructor Create(AParent: TXPRCustom; ANode: TDOMElement); override;
    destructor Destroy; override;
    //--
    procedure AddSource(ASource: TXPRSource);
    procedure RemoveSource(ASource: TXPRSource);
    function UnitByName(AName: String): TXPRUnit;
    function UnitByIdx(AIdx: Integer): TXPRUnit;
    function IncludeByIdx(AIdx: Integer): TXPRInclude;
    //--
    function UnitCount: Integer;
    function IncludeCount: Integer;
    //--
    property Units[AIdx: Integer]: TXPRUnit read UnitByIdx;
    property Includes[AIdx: Integer]: TXPRInclude read IncludeByIdx;
  end;

  TXPRResource = class(TXPRCustom)
  private
    function GetType: String;
    procedure SetType(AValue : String);
    function GetName: String;
    procedure SetName(AValue : String);
    function GetFile: String;
    procedure SetFile(AValue : String);
  protected
  public
    property RType: String read GetType write SetType;
    property RName: String read GetName write SetName;
    property FileName: String read GetFile write SetFile;
  end;

  TXPRResources = class(TXPRCustom)
  private
    ResourceList: TList;
    //--
    function GetFile: String;
    procedure SetFile(AValue: String);
  protected
  public
    constructor Create(AParent: TXPRCustom; ANode: TDOMElement); override;
    destructor Destroy; override;
    //--
    procedure AddResource(AResource: TXPRResource);
    procedure RemoveResource(AResource: TXPRResource);
    function GetResource(AType, AName: String): TXPRResource;
    function ResourceByIdx(AIdx: Integer): TXPRResource;
    //--
    function ResourceCount: Integer;
    //--
    function NeedsToRebuild: Boolean;
    //--
    property Count: Integer read ResourceCount;
    property Resources[AIdx: Integer]: TXPRResource read ResourceByIdx; default;
    //--
    property FileName: String read GetFile write SetFile;
  end;

  TXPROptions = class(TXPRCustom)
  private
  protected
    function GetNode(AName: String; var AN: TDOMElement; ACanCreate: Boolean): Boolean;
  public
    constructor Create(AParent: TXPRCustom; ANode: TDOMElement); override;
    function Has(AName: String): Boolean;
    function GetS(AName: String): String;
    function GetI(AName: String): Integer;
    function GetB(AName: String): Boolean;
    procedure SetS(AName: String; AValue: String);
    procedure SetI(AName: String; AValue: Integer);
    procedure SetB(AName: String; AValue: Boolean);
    procedure Unset(AName: String);
  end;

  TXPRProject = class(TXPRCustom)
  private
    FXMLDoc: TXMLDocument;
    FSources: TXPRSources;
    FResources: TXPRResources;
    FOptions: TXPROptions;
    //--
    FFileName: String;
    //--
    function GetName: String;
    function GetType: String;
    function GetFile: String;
    function GetExeFile: String;
    procedure SetName(AValue: String);
    procedure SetType(AValue: String);
    procedure SetFile(AValue: String);
    //==
    procedure PostLoad;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //===
    procedure New(AName: String);
    procedure Load(AFileName: String);
    procedure SaveAs(AFileName: String);
    procedure Save;
    procedure Close;
    //--
    function NeedsToCompile: Boolean;
    //===
    property Name: String read GetName write SetName;
    property ProjectType: String read GetType write SetType;
    property ProjectFile: String read GetFile write SetFile;
    property ProjectExeFile: String read GetExeFile;
    //==
    //property Options: TXPROptions read FOptions;
    property Sources: TXPRSources read FSources;
    property Resources: TXPRResources read FResources;
    property Options: TXPROptions read FOptions;
    //--
    property FileName: String read FFileName;
  end;

implementation

uses XMLRead, XMLWrite;

const
{$IFDEF WIN32}
  ExeExt = '.exe';
{$ELSE}
  ExeExt = '';
{$ENDIF}

{ TXPRCustom }

constructor TXPRCustom.Create(AParent: TXPRCustom; ANode: TDOMElement);
begin
  Node := ANode;
  FParent := AParent;
end;

{ TXPRSource }

function TXPRSource.GetFile: String;
begin
  Result := Node.GetAttribute('file');
end;

procedure TXPRSource.SetFile(AValue: String);
begin
  Node.SetAttribute('file', AValue);
end;

{ TXPRUnit }

function TXPRUnit.GetName: String;
begin
  Result := Node.GetAttribute('name');
end;

procedure TXPRUnit.SetName(AValue: String);
begin
  Node.SetAttribute('name', AValue);
end;

function TXPRUnit.GetForm: String;
begin
  Result := Node.GetAttribute('form');
end;

procedure TXPRUnit.SetForm(AValue: String);
begin
  Node.SetAttribute('form', AValue);
end;

function TXPRUnit.HasForm: Boolean;
begin
  Result := Node.GetAttribute('form') <> '';
end;

{ TXPRSources }

constructor TXPRSources.Create(AParent: TXPRCustom; ANode: TDOMElement);
var
  List: TDOMNodeList;
  I: Integer;
begin
  inherited;
  UnitList := TList.Create;
  IncludeList := TList.Create;
  //--
  List := Node.GetElementsByTagName('unit');
  try
    for I := 0 to List.Count -1 do
      AddSource(TXPRUnit.Create(Self, TDOMElement(List.Item[I])));
  finally
    List.Release;
  end;
  //--
  List := Node.GetElementsByTagName('include');
  try
    for I := 0 to List.Count -1 do
      AddSource(TXPRInclude.Create(Self, TDOMElement(List.Item[I])));
  finally
    List.Release;
  end;
end;

destructor TXPRSources.Destroy;
var
  I: Integer;
begin
  for I := 0 to UnitList.Count - 1 do
    TXPRUnit(UnitList[I]).Free;
  UnitList.Free;
  //--
  for I := 0 to IncludeList.Count - 1 do
    TXPRInclude(IncludeList[I]).Free;
  IncludeList.Free;
  //--
  inherited;
end;

procedure TXPRSources.AddSource(ASource: TXPRSource);
begin
  if ASource is TXPRUnit then
    UnitList.Add(ASource)
  else if ASource is TXPRInclude then
    IncludeList.Add(ASource);
end;

procedure TXPRSources.RemoveSource(ASource: TXPRSource);
begin
  if ASource is TXPRUnit then
    UnitList.Remove(ASource)
  else if ASource is TXPRInclude then
    IncludeList.Remove(ASource);
end;

function TXPRSources.UnitByName(AName: String): TXPRUnit;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to UnitList.Count -1 do
    if TXPRUnit(UnitList[I]).Name = AName then
      exit(TXPRUnit(UnitList[I]));
end;

function TXPRSources.UnitByIdx(AIdx: Integer): TXPRUnit;
begin
  Result := TXPRUnit(UnitList[AIdx]);
end;

function TXPRSources.IncludeByIdx(AIdx: Integer): TXPRInclude;
begin
  Result := TXPRInclude(IncludeList[AIdx]);
end;

function TXPRSources.UnitCount: Integer;
begin
  Result := UnitList.Count;
end;

function TXPRSources.IncludeCount: Integer;
begin
  Result := IncludeList.Count;
end;

{ TXPRResource }

function TXPRResource.GetType: String;
begin
  Result := Node.GetAttribute('type');
end;

procedure TXPRResource.SetType(AValue : String);
begin
  Node.SetAttribute('type', AValue);
end;

function TXPRResource.GetName: String;
begin
  Result := Node.GetAttribute('name');
end;

procedure TXPRResource.SetName(AValue : String);
begin
  Node.SetAttribute('name', AValue);
end;

function TXPRResource.GetFile: String;
begin
  Result := Node.GetAttribute('file');
end;

procedure TXPRResource.SetFile(AValue : String);
begin
  Node.SetAttribute('file', AValue);
end;

{ TXPRResources }

constructor TXPRResources.Create(AParent: TXPRCustom; ANode: TDOMElement);
var
  List: TDOMNodeList;
  I: Integer;
begin
  inherited;
  ResourceList := TList.Create;
  //--
  List := Node.GetElementsByTagName('resource');
  try
    for I := 0 to List.Count -1 do
      AddResource(TXPRResource.Create(Self, TDOMElement(List.Item[I])));
  finally
    List.Release;
  end;
end;

destructor TXPRResources.Destroy;
var
  I: Integer;
begin
  for I := 0 to ResourceList.Count - 1 do
    TXPRResource(ResourceList[I]).Free;
  ResourceList.Free;
  //--
  inherited;
end;

function TXPRResources.GetFile: String;
begin
  Result := Node.GetAttribute('file');
end;

procedure TXPRResources.SetFile(AValue: String);
begin
  Node.SetAttribute('file', AValue);
end;

procedure TXPRResources.AddResource(AResource: TXPRResource);
begin
  ResourceList.Add(AResource);
end;

procedure TXPRResources.RemoveResource(AResource: TXPRResource);
begin
  ResourceList.Remove(AResource);
end;

function TXPRResources.GetResource(AType, AName: String): TXPRResource;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ResourceList.Count -1 do
    if (TXPRResource(ResourceList[I]).RType = AType) and (TXPRResource(ResourceList[I]).RName = AName) then
      exit(TXPRResource(ResourceList[I]));
end;

function TXPRResources.ResourceByIdx(AIdx: Integer): TXPRResource;
begin
  Result := TXPRResource(ResourceList[AIdx]);
end;

function TXPRResources.ResourceCount: Integer;
begin
  Result := ResourceList.Count;
end;

function TXPRResources.NeedsToRebuild: Boolean;
var
  dtRes: TDateTime;

  function CheckFile(AFN: String): Boolean;
  begin
    Result := FileDateToDateTime(FileAge(AFN)) > dtRes;
  end;
var
  I: Integer;
  Prj: TXPRProject;
begin
  if not FileExists(GetFile) then
    exit(True);

  dtRes := FileDateToDateTime(FileAge(GetFile));

  for I := 0 to Count -1 do
    if CheckFile(ResourceByIdx(I).GetFile) then
      exit(True);

  if Assigned(Parent) and (Parent is TXPRProject) then
  begin
    Prj := TXPRProject(Parent);
    for I := 0 to Prj.Sources.UnitCount -1 do
      if (Prj.Sources.Units[I].Form <> '') and CheckFile(Prj.Sources.Units[I].Form) then
        exit(True);
  end;

  Result := False;
end;

{ TXPROptions }

constructor TXPROptions.Create(AParent: TXPRCustom; ANode: TDOMElement);
begin
  inherited;
end;
{
procedure PrintNode(L: Integer; ANode: TDOMNode);
var
  I: Word;
begin
  if Assigned(ANode) then
  begin
    for I := 0 to L do
      Write('  ');
    WriteLn(ANode.NodeName);
    PrintNode(L+1,ANode.FirstChild);
    PrintNode(L,ANode.NextSibling);
  end;
end;
}

function TXPROptions.GetNode(AName: String; var AN: TDOMElement; ACanCreate: Boolean): Boolean;
var
  List: TDOMNodeList;
  I: Integer;
begin
  Result := False;
  AN := nil;
  //--
  List := Node.GetElementsByTagName('option');
  try
    for I := 0 to List.Count -1 do
      if TDOMElement(List.Item[I]).GetAttribute('name') = AName then
      begin
        Result := True;
        AN := TDOMElement(List.Item[I]);
        Break;
      end;

    if (not Assigned(AN)) and ACanCreate then
    begin
      Result := True;
      AN := TXPRProject(Parent).FXMLDoc.CreateElement('option');
      AN.SetAttribute('name', AName);
      AN := TDOMElement(Node.AppendChild(AN));
    end;
  finally
    List.Release;
  end;
end;

function TXPROptions.Has(AName: String): Boolean;
var
  N: TDOMElement;
begin
  Result := GetNode(AName, N, False);
end;

function TXPROptions.GetS(AName: String): String;
var
  N: TDOMElement;
begin
  if GetNode(AName, N, False) then
    Result := N.GetAttribute('value')
  else
    Result := '';
end;

function TXPROptions.GetI(AName: String): Integer;
var
  N: TDOMElement;
begin
  if GetNode(AName, N, False) then
    Result := StrToInt(N.GetAttribute('value'))
  else
    Result := 0;
end;

function TXPROptions.GetB(AName: String): Boolean;
var
  N: TDOMElement;
  S: String;
begin
  if GetNode(AName, N, False) then
  begin
    S := LowerCase(N.GetAttribute('value'));
    Result := (S = '1') or (S = 't') or (S = 'true') or (S = 'y') or (S = 'yes');
  end
  else
    Result := False;
end;

procedure TXPROptions.SetS(AName: String; AValue: String);
var
  N: TDOMElement;
begin
  if AValue = '' then
    Unset(AName)
  else
  begin
    GetNode(AName, N, True);
    N.SetAttribute('value', AValue);
  end;
end;

procedure TXPROptions.SetI(AName: String; AValue: Integer);
var
  N: TDOMElement;
begin
  GetNode(AName, N, True);
  N.SetAttribute('value', IntToStr(AValue));
end;

procedure TXPROptions.SetB(AName: String; AValue: Boolean);
var
  N: TDOMElement;
begin
  GetNode(AName, N, True);
  if AValue then
    N.SetAttribute('value', 'true')
  else
    N.SetAttribute('value', 'false');
end;

procedure TXPROptions.Unset(AName: String);
var
  N: TDOMElement;
begin
  if GetNode(AName, N, False) then
  begin
    Node.RemoveChild(N).Free;
  end;
end;

{ TXPRProject }

constructor TXPRProject.Create;
begin
  inherited Create(nil, nil);
  FXMLDoc := nil;
  FSources := nil;
  FResources := nil;
  FOptions := nil;
end;

destructor TXPRProject.Destroy;
begin
  inherited;
end;

function TXPRProject.GetName: String;
begin
  if Assigned(Node) then
    Result := Node.GetAttribute('name')
  else
    Result := '';
end;

function TXPRProject.GetType: String;
begin
  if Assigned(Node) then
    Result := Node.GetAttribute('type')
  else
    Result := '';
end;

function TXPRProject.GetFile: String;
begin
  if Assigned(Node) then
    Result := Node.GetAttribute('file')
  else
    Result := '';
end;

function TXPRProject.GetExeFile: String;
begin
  Result := ChangeFileExt(GetFile, ExeExt);
end;

procedure TXPRProject.SetName(AValue: String);
begin
  if Assigned(Node) then
    Node.SetAttribute('name', AValue);
end;

procedure TXPRProject.SetType(AValue: String);
begin
  if Assigned(Node) then
    Node.SetAttribute('type', AValue);
end;

procedure TXPRProject.SetFile(AValue: String);
begin
  if Assigned(Node) then
    Node.SetAttribute('file', AValue);
end;

procedure TXPRProject.New(AName: String);
var
  S: TStream;
begin
  Close;

  FFileName := AName + '.xpr';
  S := TStringStream.Create(
  '<?xml version="1.0"?>'+
  '<project type="program" name="'+AName+'" file="'+FFileName+'">'+
  '  <options>'+
  '  </options>'+
  '  <sources>'+
  '  </sources>'+
  '  <resources file="'+AName+'_xrc.pp">'+
  '  </resources>'+
  '</project>');
  try
    ReadXMLFile(FXMLDoc, S);
  finally
    S.Free;
  end;

  PostLoad;
end;

procedure TXPRProject.Load(AFileName: String);
begin
  Close;

  ChDir(ExtractFileDir(AFileName));

  FFileName := ExtractFileName(AFileName);
  ReadXMLFile(FXMLDoc, FFileName);

  PostLoad;
end;

procedure TXPRProject.PostLoad;
begin
  if not Assigned(FXMLDoc) then
    raise Exception.CreateFmt('Error loading Project "%s"', [FFileName]);

  Node := TDOMElement(FXMLDoc.FindNode('project'));

  FOptions := TXPROptions.Create(Self, TDOMElement(Node.FindNode('options')));
  FSources := TXPRSources.Create(Self, TDOMElement(Node.FindNode('sources')));
  FResources := TXPRResources.Create(Self, TDOMElement(Node.FindNode('resources')));
end;

procedure TXPRProject.SaveAs(AFileName: String);
begin
  FFileName := AFileName;
  Save;
end;

procedure TXPRProject.Save;
begin
  WriteXMLFile(FXMLDoc, FFileName);
end;

procedure TXPRProject.Close;
begin
  if Assigned(FXMLDoc) then
  begin
    FreeAndNil(FOptions);
    FreeAndNil(FSources);
    FreeAndNil(FResources);
    FreeAndNil(FXMLDoc);
  end;
  FFileName := '';
end;

function TXPRProject.NeedsToCompile: Boolean;
var
  dtExe: TDateTime;

  function CheckFile(AFN: String): Boolean;
  begin
    Result := FileDateToDateTime(FileAge(AFN)) > dtExe;
  end;
var
  I: Integer;
begin
  if not FileExists(ProjectExeFile) then
    exit(True);

  dtExe := FileDateToDateTime(FileAge(ProjectExeFile));

  if CheckFile(GetFile) then
    exit(True);

  for I := 0 to Sources.UnitCount -1 do
    if CheckFile(Sources.Units[I].FileName) then
      exit(True);

  for I := 0 to Sources.IncludeCount -1 do
    if CheckFile(Sources.Includes[I].FileName) then
      exit(True);

  if CheckFile(Resources.FileName) or Resources.NeedsToRebuild then
    exit(True);

  Result := False;
end;

end.
