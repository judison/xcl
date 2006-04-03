(*
   XCL's Skeleton XML description file generator

   Copyright (C) 2000 - 2003 by
     Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org
   Copyright (C) 2006 by
     Judison Oliveria Gil Filho, judison@gmail.com

   See the file COPYING.GPL, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
program xmkskel;

{$H+}
{$MODE ObjFpc}

uses
  SysUtils, Classes, PasTree, PParser, PScanner, DOM, XMLWrite, XMLRead;

type
  TCmdLineAction = (actionHelp, actionConvert);

  TSkelEngine = class(TPasTreeContainer)
  private
    CurModule: TPasModule;
    CurPackage: TPasPackage;

    FXMLDoc: TXMLDocument;
    FDesc: TDOMElement;
      FPack: TDOMElement;
        FModule: TDOMElement;
    FLastEL: TDOMElement;
  public
    constructor Create;
    procedure SetPackageName(const APackageName: String);
    function FindElement(const AName: String): TPasElement; override;
    function FindModule(const AName: String): TPasModule; override;

    function FindXMLElement(AName: String): TDOMElement;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility :TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement; override;
  end;

const
  CmdLineAction: TCmdLineAction = actionConvert;
  OSTarget: String = {$I %FPCTARGETOS%};
  CPUTarget: String = {$I %FPCTARGETCPU%};

var
  Engine: TSkelEngine;
  UpdateMode,
  DisableErrors,
  DisableSeealso,
  DisableArguments,
  DisableProtected,
  DisablePrivate,
  DisableFunctionResults: Boolean;

  PackageName, InputName, OutputName: String;

constructor TSkelEngine.Create;
begin
  FLastEL := nil;
  if UpdateMode then
  begin
    ReadXMLFile(FXMLDoc, OutputName);
    FDesc := TDOMElement(FXMLDoc.FindNode('fpdoc-descriptions'));
    FPack := TDOMElement(FDesc.FindNode('package'));
    FModule := TDOMElement(FPack.FindNode('module'));
  end
  else
  begin
    FXMLDoc := TXMLDocument.Create;
    FXMLDoc.XMLVersion := '1.0';
    FDesc := FXMLDoc.CreateElement('fpdoc-descriptions');
    FXMLDoc.AppendChild(FDesc);
    FPack := FXMLDoc.CreateElement('package');
    FPack['name'] := PackageName;
    FDesc.AppendChild(FPack);
    FModule := FXMLDoc.CreateElement('module');
    FPack.AppendChild(FModule);
  end;
end;

procedure TSkelEngine.SetPackageName(const APackageName: String);
begin
  CurPackage := TPasPackage(inherited CreateElement(TPasPackage, '#' + APackageName, nil, '', 0));
end;

function TSkelEngine.FindElement(const AName: String): TPasElement;
  function FindInModule(AModule: TPasModule; const LocalName: String): TPasElement;
  var
    l: TList;
    i: Integer;
  begin
    l := AModule.InterfaceSection.Declarations;
    for i := 0 to l.Count - 1 do
    begin
      Result := TPasElement(l[i]);
      if CompareText(Result.Name, LocalName) = 0 then
        exit;
    end;
    Result := nil;
 end;
var
  i: Integer;
  Module: TPasElement;
begin
  Result := FindInModule(CurModule, AName);
  if not Assigned(Result) then
    for i := CurModule.InterfaceSection.UsesList.Count - 1 downto 0 do
    begin
      Module := TPasElement(CurModule.InterfaceSection.UsesList[i]);
      if Module.ClassType = TPasModule then
      begin
        Result := FindInModule(TPasModule(Module), AName);
        if Assigned(Result) then
          exit;
      end;
    end;
end;

function TSkelEngine.FindModule(const AName: String): TPasModule;
var
  i: Integer;
begin
  for i := 0 to CurPackage.Modules.Count - 1 do
  begin
    Result := TPasModule(FPackage.Modules[i]);
    if CompareText(Result.Name, AName) = 0 then
      exit;
  end;
  Result := nil;
end;

function TSkelEngine.FindXMLElement(AName: String): TDOMElement;
begin
  Result := TDOMElement(FModule.FirstChild);
  while Assigned(Result) and (Result['name'] <> AName) do
    Result := TDOMElement(Result.NextSibling);
end;

function TSkelEngine.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility : TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;

  Function WriteThisNode(APasElement : TPasElement)  : Boolean;
  var
   lel: TDOMElement;
  begin
    Result:=Assigned(AParent) and (Length(AName) > 0) and
            (not DisableArguments or (APasElement.ClassType <> TPasArgument)) and
            (not DisableFunctionResults or (APasElement.ClassType <> TPasResultElement)) and
            (not DisablePrivate or (AVisibility<>visPrivate)) and
            (not DisableProtected or (AVisibility<>visProtected));
    if Result and UpdateMode then
    begin
      lel := FindXMLElement(APasElement.FullName);
      Result := lel = nil;
      If Result then
      begin
        Writeln(stderr,'Creating documentation for new node ',APasElement.PathName);
      end
      else
        FLastEL := lel;
    end;
  end;

  Function WriteOnlyShort(APasElement : TPasElement) : Boolean;
  begin
    Result:=(APasElement.ClassType=TPasArgument) or
            (APasElement.ClassType=TPasResultElement) or
            (APasElement.ClassType=TPasEnumValue);
  end;

  Function IsTypeVarConst(APasElement : TPasElement) : Boolean;
  begin
    With APasElement do
      Result:=(InheritsFrom(TPasType) and not InheritsFrom(TPasClassType)) or
              (InheritsFrom(TPasResString)) or
              (InheritsFrom(TPasVariable));

  end;

  procedure AddNullNode(AName: String; P: TDOMElement);
  var
    EL: TDOMElement;
    T: TDOMText;
  begin
    EL := FXMLDoc.CreateElement(AName);
//    if P = FModule then
//      T := FXMLDoc.CreateTextNode(#10'      ')
//    else
//      T := FXMLDoc.CreateTextNode(#10'        ');
    T := FXMLDoc.CreateTextNode('');
    EL.AppendChild(T);
    P.AppendChild(EL);
  end;

var
  EL: TDOMElement;
begin
  Result := AClass.Create(AName, AParent);
  if AClass.InheritsFrom(TPasModule) then
    CurModule := TPasModule(Result);

  if Result.ClassType = TPasModule then
  begin
    FModule['name'] := Result.Name;

    if not UpdateMode then
    begin
      AddNullNode('short', FModule);
      AddNullNode('descr', FModule);
    end;
  end
  else if WriteThisNode(Result) then
  begin
    EL := FXMLDoc.CreateElement('element');
    EL['name'] := Result.FullName;
    if Assigned(FLastEL) then
      FModule.InsertBefore(EL, FLastEL.NextSibling)
    else
      FModule.AppendChild(EL);
    AddNullNode('short', EL);
    if not WriteOnlyShort(Result) then
    begin
      AddNullNode('descr', EL);
      if not (DisableErrors or IsTypeVarConst(Result)) then
        AddNullNode('errors', EL);
      if not DisableSeealso then
        AddNullNode('seealso', EL);
    end;
    FLastEL := EL;
  end;
end;

procedure Usage;
begin
  Writeln('Usage : ',ExtractFileName(Paramstr(0)),' [options]');
  Writeln('Where [options] is one or more of :');
  Writeln(' --disable-arguments Do not create nodes for function arguments.');
  Writeln(' --disable-errors    Do not create errors node.');
  Writeln(' --disable-function-results');
  Writeln('                     Do not create nodes for function arguments.');
  Writeln(' --disable-private   Do not create nodes for class private fields.');
  Writeln(' --disable-protected Do not create nodes for class protected fields.');
  Writeln(' --disable-seealso   Do not create seealso node.');
  Writeln(' --help              Emit help.');
  Writeln(' --input=cmdline     Input file to create skeleton for.');
  Writeln('                     Use options are as for compiler.');
  Writeln(' --output=filename   Send output to file.');
  Writeln(' --package=name      Specify package name (mandatory).');
  Writeln(' --update            Update mode. Updates Output file.');
end;

procedure ParseOption(const s: String);
var
  i: Integer;
  Cmd, Arg: String;
begin
  if (s = '-h') or (s = '--help') then
    CmdLineAction := actionHelp
  else if s = '--update' then
    UpdateMode := True
  else if s = '--disable-arguments' then
    DisableArguments := True
  else if s = '--disable-errors' then
    DisableErrors := True
  else if s = '--disable-function-results' then
    DisableFunctionResults := True
  else if s = '--disable-seealso' then
    DisableSeealso := True
  else if s = '--disable-private' then
    DisablePrivate := True
  else if s = '--disable-protected' then
  begin
    DisableProtected := True;
    DisablePrivate := True;
  end
  else
  begin
    i := Pos('=', s);
    if i > 0 then
    begin
      Cmd := Copy(s, 1, i - 1);
      Arg := Copy(s, i + 1, Length(s));
    end else
    begin
      Cmd := s;
      SetLength(Arg, 0);
    end;
    if (Cmd = '-i') or (Cmd = '--input') then
      InputName := Arg
    else if (Cmd = '-o') or (Cmd = '--output') then
      OutputName := Arg
    else if Cmd = '--package' then
      PackageName := Arg
    else
      WriteLn(StdErr, Format('Ignoring unknown option "%s"', [s]));
  end;
end;

procedure ParseCommandLine;
var
  i: Integer;
begin
  for i := 1 to ParamCount do
    ParseOption(ParamStr(i));
end;

begin
  ParseCommandLine;
  if CmdLineAction = actionHelp then
  begin
    Usage;
    exit;
  end;

  if Length(PackageName) = 0 then
  begin
    WriteLn('Please specify a package name with --package=<name>');
    Halt(2);
  end;

  if UpdateMode then
    WriteLn('In Update Mode, please be patient...');

  // Process source file
  Engine := TSkelEngine.Create;
  try
    try
      Engine.SetPackageName(PackageName);
      ParseSource(Engine, InputName, OSTarget, CPUTarget);
      //--
      WriteXMLFile(Engine.FXMLDoc, OutputName);
    except
      on E: EFileNotFoundError do
      begin
        Writeln(StdErr,' file ', E.Message, ' not found');
        Halt(1);
      end;
    end;
  finally
    Engine.Free;
  end;
  WriteLn('Done.');
end.
