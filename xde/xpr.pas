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

{$H+}
{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}

interface

uses Classes, SysUtils, DOM;

type
  TXPRCustom = class
  private
  protected
    Node: TDOMElement;
  public
    constructor Create(ANode: TDOMElement); virtual;
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
    constructor Create(ANode: TDOMElement); override;
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
  protected
  public
    property RType: String read GetType write SetType;
    property RName: String read GetName write SetName;
  end;

  TXPRResources = class(TXPRCustom)
  private
    ResourceList: TList;
    //--
    function GetFile: String;
    procedure SetFile(AValue: String);
  protected
  public
    constructor Create(ANode: TDOMElement); override;
    destructor Destroy; override;
    //--
    procedure AddResource(AResource: TXPRResource);
    procedure RemoveResource(AResource: TXPRResource);
    function GetResource(AType, AName: String): TXPRResource;
    function ResourceByIdx(AIdx: Integer): TXPRResource;
    //--
    function ResourceCount: Integer;
    //--
    property Count: Integer read ResourceCount;
    property Resources[AIdx: Integer]: TXPRResource read ResourceByIdx; default;
    //--
    property FileName: String read GetFile write SetFile;
  end;

  TXPRProject = class(TXPRCustom)
  private
    FXMLDoc: TXMLDocument;
    FSources: TXPRSources;
    FResources: TXPRResources;
    //--
    FFileName: String;
    //--
    function GetName: String;
    function GetType: String;
    function GetFile: String;
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
    //===
    property Name: String read GetName write SetName;
    property ProjectType: String read GetType write SetType;
    property ProjectFile: String read GetFile write SetFile;
    //==
    //property Options: TXPROptions read FOptions;
    property Sources: TXPRSources read FSources;
    property Resources: TXPRResources read FResources;
    //--
    property FileName: String read FFileName;
  end;

implementation

uses XMLRead, XMLWrite;

{ TXPRCustom }

constructor TXPRCustom.Create(ANode: TDOMElement);
begin
  Node := ANode;
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

constructor TXPRSources.Create(ANode: TDOMElement);
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
      AddSource(TXPRUnit.Create(TDOMElement(List.Item[I])));
  finally
    List.Release;
  end;
  //--
  List := Node.GetElementsByTagName('include');
  try
    for I := 0 to List.Count -1 do
      AddSource(TXPRInclude.Create(TDOMElement(List.Item[I])));
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

{ TXPRResources }

constructor TXPRResources.Create(ANode: TDOMElement);
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
      AddResource(TXPRResource.Create(TDOMElement(List.Item[I])));
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

{ TXPRProject }

constructor TXPRProject.Create;
begin
  inherited Create(nil);
  FXMLDoc := nil;
  FSources := nil;
  FResources := nil;
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
  '<?xml version="1.0" encoding="utf-8"?>'+
  '<project type="program" name="'+AName+'" file="'+FFileName+'">'+
    '<options></options>'+
    '<sources></sources>'+
    '<resources file="'+AName+'_xrc.pp"></resources>'+
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

  FFileName := AFileName;
  ReadXMLFile(FXMLDoc, AFileName);

  PostLoad;
end;

procedure TXPRProject.PostLoad;
begin
  if not Assigned(FXMLDoc) then
    raise Exception.CreateFmt('Error loading Project "%s"', [FFileName]);

  Node := TDOMElement(FXMLDoc.FindNode('project'));

  FSources := TXPRSources.Create(TDOMElement(Node.FindNode('sources')));
  FResources := TXPRResources.Create(TDOMElement(Node.FindNode('resources')));
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
    FreeAndNil(FSources);
    FreeAndNil(FResources);
    FreeAndNil(FXMLDoc);
  end;
end;

end.