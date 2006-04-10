{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
 
  Author: Mattias Gaertner

  Abstract:
    This unit defines a list of forms descendents. The forms are normal TForm
    descendents with one exception: Every form has its own class. These classes
    are changeable at runtime, so that IDEs can add, remove or rename methods
    and such stuff. Also these forms can be loaded from streams and missing
    components and methods are added just-in-time to the class definition.
    Hence the name for the class: TJITForms.
    Subcomponents are looked up in the list of registered components
    (TJITForms.RegCompList).

  ToDo:
    -Add recursion needed for frames.
}
unit JITForms;

{$mode objfpc}{$H+}

{DEFINE VerboseJITForms}

interface

uses
  Classes, SysUtils, TypInfo, XCL, JITForm;


//=========================
// JUDISON
//=========================

type
  TModalResult = integer;

const
  mrCancel = 0;
  mrIgnore = 1;

//===========================================

type
  //----------------------------------------------------------------------------
  TJITFormError = (
      jfeNone,
      jfeUnknown,
      jfeUnknownProperty,
      jfeUnknownComponentClass,
      jfeReaderError
    );
  TJITFormErrors = set of TJITFormError;
  
  TJITReaderErrorEvent = procedure(Sender: TObject; ErrorType: TJITFormError;
    var Action: TModalResult) of object;
  TJITPropertyNotFoundEvent = procedure(Sender: TObject; Reader: TReader;
    Instance: TPersistent; var PropName: string; IsPath: boolean;
    var Handled, Skip: Boolean) of object;


  { TJITComponentList }
  
  TJITCompListFlag = (
    jclAutoRenameComponents
    );
  TJITCompListFlags = set of TJITCompListFlag;

  TJITComponentList = class(TPersistentWithTemplates)
  private
    FComponentPrefix: string;
    FCurUnknownClass: string;
    FCurUnknownProperty: string;
    FOnPropertyNotFound: TJITPropertyNotFoundEvent;
    procedure SetComponentPrefix(const AValue: string);
  protected
    FCurReadErrorMsg: string;
    FCurReadJITComponent:TComponent;
    FCurReadClass:TClass;
    FCurReadChild: TComponent;
    FCurReadChildClass: TComponentClass;
    FOnReaderError: TJITReaderErrorEvent;
    FJITComponents: TList;
    FFlags: TJITCompListFlags;
    // jit procedures
    function CreateNewJITClass(ParentClass: TClass;
                          const NewClassName, NewUnitName: ShortString): TClass;
    procedure FreeJITClass(var AClass: TClass);
    procedure DoAddNewMethod(JITClass: TClass; const AName: ShortString;
      ACode: Pointer); // Note: AddNewMethod does not check if method already exists
    procedure DoRemoveMethod(JITClass: TClass; AName: ShortString;
      var OldCode: Pointer); // Note: RemoveMethod does not free code memory
    procedure DoRenameMethod(JITClass: TClass; OldName, NewName: ShortString);
    procedure DoRenameClass(JITClass: TClass; const NewName: ShortString);
    // TReader events
    procedure ReaderFindMethod(Reader: TReader; const FindMethodName: Ansistring;
      var Address: Pointer; var Error: Boolean);
    {$IFNDEF VER1_0}
    procedure ReaderSetMethodProperty(Reader: TReader; Instance: TPersistent;
      PropInfo: PPropInfo; const TheMethodName: string; var Handled: boolean);
    procedure ReaderPropertyNotFound(Reader: TReader; Instance: TPersistent;
      var PropName: string; IsPath: Boolean; var Handled, Skip: Boolean);
    {$ENDIF}
    procedure ReaderSetName(Reader: TReader; Component: TComponent;
      var NewName: Ansistring);
    procedure ReaderReferenceName(Reader: TReader; var RefName: Ansistring);
    procedure ReaderAncestorNotFound(Reader: TReader;
      const ComponentName: Ansistring; ComponentClass: TPersistentClass;
      var Component: TComponent);
    procedure ReaderError(Reader: TReader; const ErrorMsg: Ansistring;
      var Handled: Boolean);
    procedure ReaderFindComponentClass(Reader: TReader;
      const FindClassName: Ansistring; var ComponentClass: TComponentClass);
    procedure ReaderCreateComponent(Reader: TReader;
      ComponentClass: TComponentClass; var Component: TComponent);
    procedure ReaderReadComponent(Component: TComponent);
    // some useful functions
    function GetItem(Index:integer):TComponent;
    function GetClassNameFromStream(s:TStream):shortstring;
    function OnFindGlobalComponent(const AName:AnsiString):TComponent;
    procedure InitReading(BinStream: TStream; var Reader: TReader;
                          DestroyDriver: Boolean); virtual;
    function DoCreateJITComponent(const NewComponentName, NewClassName,
                         NewUnitName: shortstring; ParentClass: TClass):integer;
    procedure DoFinishReading; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property Items[Index: integer]: TComponent read GetItem; default;
    function Count: integer;
    function AddNewJITComponent(const NewUnitName: shortstring;
                                ParentClass: TClass): integer;
    function AddJITComponentFromStream(BinStream: TStream; ParentClass: TClass;
                                       const NewUnitName: ShortString;
                                       Interactive: Boolean):integer;
    procedure DestroyJITComponent(JITComponent: TComponent);
    procedure DestroyJITComponent(Index: integer);
    function IndexOf(JITComponent: TComponent): integer;
    function Contains(JITComponent: TComponent): boolean;
    function FindComponentByClassName(const AClassName: shortstring):integer;
    function FindComponentByName(const AName: shortstring): integer;
    procedure GetUnusedNames(var ComponentName, ComponentClassName: shortstring);
    procedure AddNewMethod(JITComponent: TComponent; const AName: ShortString);
    function CreateNewMethod(JITComponent: TComponent;
                             const AName: ShortString): TMethod;
    procedure RemoveMethod(JITComponent: TComponent; const AName: ShortString);
    procedure RenameMethod(JITComponent: TComponent;
                           const OldName, NewName: ShortString);
    procedure RenameComponentClass(JITComponent: TComponent;
                                   const NewName: ShortString);
    // child components
    function AddJITChildComponentFromStream(JITOwnerComponent: TComponent;
      BinStream: TStream; ComponentClass: TComponentClass;
      ParentControl: TControl): TComponent;
  public
    property OnReaderError: TJITReaderErrorEvent
                                       read FOnReaderError write FOnReaderError;
    property OnPropertyNotFound: TJITPropertyNotFoundEvent
                             read FOnPropertyNotFound write FOnPropertyNotFound;
    property CurReadJITComponent:TComponent read FCurReadJITComponent;
    property CurReadClass:TClass read FCurReadClass;
    property CurReadChild: TComponent read FCurReadChild;
    property CurReadChildClass: TComponentClass read FCurReadChildClass;
    property CurReadErrorMsg: string read FCurReadErrorMsg;
    property CurUnknownProperty: string read FCurUnknownProperty;
    property CurUnknownClass: string read FCurUnknownClass;
    property ComponentPrefix: string read FComponentPrefix
                                     write SetComponentPrefix;
  end;


  { TJITForms }
  
  TJITForms = class(TJITComponentList)
  private
    function GetItem(Index: integer): TForm;
  public
    constructor Create;
    function IsJITForm(AComponent: TComponent): boolean;
    property Items[Index:integer]: TForm read GetItem; default;
  end;
  
  
  { TJITNonFormComponents }
  
  TJITNonFormComponents = class(TJITComponentList)
  public
    constructor Create;
    function IsJITNonForm(AComponent: TComponent): boolean;
  end;
  

function ClassAsString(AClass: TClass): string;
function ClassMethodTableAsString(AClass: TClass): string;
function ClassTypeInfoAsString(AClass: TClass): string;
function ClassFieldTableAsString(AClass: TClass): string;

const
  DefaultJITUnitName = 'VirtualUnitForJITClasses';


implementation

//=============
//JUDISON

procedure DebugLn(const s: string);
begin
  if TextRec(Output).Mode<>fmClosed then
    writeln(s);
end;

procedure DebugLn;
begin
  DebugLn('');
end;

procedure DebugLn(const s1, s2: string);
begin
  DebugLn(s1+s2);
end;

procedure DebugLn(const s1, s2, s3: string);
begin
  DebugLn(s1+s2+s3);
end;

procedure DebugLn(const s1, s2, s3, s4: string);
begin
  DebugLn(s1+s2+s3+s4);
end;

procedure DebugLn(const s1, s2, s3, s4, s5: string);
begin
  DebugLn(s1+s2+s3+s4+s5);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12);
end;

procedure DBGOut(const s: string);
begin
  if TextRec(Output).Mode<>fmClosed then
    write(s);
end;

procedure DBGOut(const s1, s2: string);
begin
  DbgOut(s1+s2);
end;

procedure DbgOut(const s1, s2, s3: string);
begin
  DbgOut(s1+s2+s3);
end;

procedure DbgOut(const s1, s2, s3, s4: string);
begin
  DbgOut(s1+s2+s3+s4);
end;

procedure DbgOut(const s1, s2, s3, s4, s5: string);
begin
  DbgOut(s1+s2+s3+s4+s5);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6);
end;

function DbgS(const c: cardinal): string;
begin
  Result:=IntToStr(c);
end;

function DbgS(const i: integer): string;
begin
  Result:=IntToStr(i);
end;

function DbgS(const r: TRect): string;
begin
  Result:=' l='+IntToStr(r.Left)+',t='+IntToStr(r.Top)
         +',r='+IntToStr(r.Right)+',b='+IntToStr(r.Bottom);
end;

function DbgS(const p: TPoint): string;
begin
  Result:=' x='+IntToStr(p.x)+',y='+IntToStr(p.y);
end;

function DbgS(const p: pointer): string;
begin
  Result:=HexStr(PtrInt(p),2*sizeof(PtrInt));
end;

function DbgS(const e: extended): string;
begin
  Result:=FloatToStr(e);
end;

function DbgS(const b: boolean): string;
begin
  if b then Result:='True' else Result:='False';
end;

function DbgS(const i1, i2, i3, i4: integer): string;
begin
  Result:=dbgs(i1)+','+dbgs(i2)+','+dbgs(i3)+','+dbgs(i4);
end;

function DbgSName(const p: TObject): string;
begin
  if p=nil then
    Result:='nil'
  else if p is TComponent then
    Result:=TComponent(p).Name+':'+p.ClassName
  else
    Result:=p.ClassName;
end;

function DbgStr(const StringWithSpecialChars: string): string;
var
  i: Integer;
  s: String;
begin
  Result:=StringWithSpecialChars;
  i:=1;
  while (i<=length(Result)) do begin
    case Result[i] of
    ' '..#126: inc(i);
    else
      s:='#'+IntToStr(ord(Result[i]));
      Result:=copy(Result,1,i-1)+s+copy(Result,i+1,length(Result)-i);
      inc(i,length(s));
    end;
  end;
end;



function GetDesignerForm(AComponent: TComponent): TForm;
var
  OwnerComponent: TComponent;
begin
  Result := nil;
  if AComponent = nil then
    exit;
//  if Assigned(OnGetDesignerForm) then
//    Result:=OnGetDesignerForm(AComponent)
//  else
//  begin
    OwnerComponent := AComponent;
    while OwnerComponent.Owner <> nil do
      OwnerComponent := OwnerComponent.Owner;
    if OwnerComponent is TForm then
      Result:=TForm(OwnerComponent);
//  end;
end;

function FindRootDesigner(AComponent: TComponent): IDesigner;
var
  Form: TForm;
begin
  Result:=nil;
  Form := GetDesignerForm(AComponent);
  if Form <> nil then
    Result := Form.Designer;
end;


procedure RaiseException(const AMsg: String);
begin
  raise Exception.Create(AMsg);
end;

function CreateXRCReader(s: TStream; var DestroyDriver: boolean): TReader;
begin
  Result:=TReader.Create(s, 4096);
  DestroyDriver:=True;
end;


//=============

{$IFOPT R+}{$DEFINE RangeCheckOn}{$ENDIF}

//------------------------------------------------------------------------------
// adding, removing and renaming of classes and methods at runtime

const
  vmtInstanceSizeNeg = vmtInstanceSize+sizeof(ptrint);

type
  // these definitions are copied from objpas.inc

  TMethodNameRec = packed record
    Name : PShortString;
    Addr : Pointer;
  end;

  TMethodNameTable = packed record
    Count : DWord;
    // for runtime range checking it is important to give a range
    Entries : packed array[0..1000000] of TMethodNameRec;
  end;
  PMethodNameTable =  ^TMethodNameTable;

  PFieldClassTable = ^TFieldClassTable;
  TFieldClassTable = 
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record  
    Count: Word;
    Entries: array[Word] of TPersistentClass;
  end;

  PFieldInfo = ^TFieldInfo;
  TFieldInfo = 
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record  
    FieldOffset: LongWord;
    ClassTypeIndex: Word;
    Name: ShortString;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = 
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record  
    FieldCount: Word;
    ClassTable: PFieldClassTable;
    { should be array[Word] of TFieldinfo;  but Elements have variant size! force at least proper alignment }
    Fields: array[0..0] of TFieldInfo;
  end;

function GetVMTSize(AClass: TClass): integer;
var
  p: PPointer;
begin
  Result:=10000;
  if AClass=nil then exit;
  p:=PPointer(pointer(AClass)+vmtMethodStart);
  Result:=vmtMethodStart;
  while (p^<>nil) and (Result<10000) do begin
    inc(p);
    inc(Result,SizeOf(Pointer));
  end;
end;

function FindVMTMethodOffset(AClass: TClass; MethodPointer: Pointer): integer;
var
  i: Integer;
  p: Pointer;
begin
  i:=vmtMethodStart div SizeOf(Pointer);
  while i<=10000 do begin
    p:=PPointer(pointer(AClass))[i];
    if p=nil then break;
    if p=MethodPointer then begin
      Result:=i*SizeOf(Pointer);
      exit;
    end;
    inc(i);
  end;
  Result:=0;
end;

function GetVMTVirtualMethodOffset(
  ParentClassWithVirtualMethod: TClass; MethodOfParentClass: Pointer;
  ClassWithOverrideMethod: TClass; OverrideMethodOfClass: Pointer
  ): integer;
var
  ParentMethodOffset: LongInt;
  OverrideMethodOffset: LongInt;
begin
  ParentMethodOffset:=FindVMTMethodOffset(
                              ParentClassWithVirtualMethod,MethodOfParentClass);
  if ParentMethodOffset<=0 then
    raise Exception.Create('GetVMTVirtualMethodOffset Parent Virtual Method not found');
  OverrideMethodOffset:=FindVMTMethodOffset(
                                 ClassWithOverrideMethod,OverrideMethodOfClass);
  if OverrideMethodOffset<=0 then
    raise Exception.Create('GetVMTVirtualMethodOffset Override Method not found');
  if ParentMethodOffset<>OverrideMethodOffset then
    raise Exception.Create('GetVMTVirtualMethodOffset Virtual Method Offset <> Override Method Offset');
  Result:=OverrideMethodOffset;
end;

{ TComponentWithOverrideValidateRename }
type
  TComponentWithOverrideValidateRename = class(TComponent)
  public
    procedure ValidateRename(AComponent: TComponent;
      const CurName, NewName: string); override;
  end;

var
  TComponentValidateRenameOffset: LongInt;

procedure TComponentWithOverrideValidateRename.ValidateRename(
  AComponent: TComponent; const CurName, NewName: string);
var
  Designer: IDesigner;
begin
  //debugln('TComponentWithOverrideValidateRename.ValidateRename ',DbgSName(Self));
  inherited ValidateRename(AComponent, CurName, NewName);
  Designer:=FindRootDesigner(Self);
  if Designer <> nil then
    Designer.ValidateRename(AComponent, CurName, NewName);
end;

{$IFDEF VER1_0}
type
  TMyComponent = class(TComponent)
    function GetValidateRenameAddress: pointer;
  end;

{ TMyComponent }

class function TMyComponent.GetValidateRenameAddress: pointer;
begin
  Result := @TComponent.ValidateRename;
end;
{$ENDIF}

function GetTComponentValidateRenameVMTOffset: integer;
begin
  Result:=GetVMTVirtualMethodOffset(TComponent,
    {$IFNDEF VER1_0}@TComponent.ValidateRename
    {$ELSE}TMyComponent.GetValidateRenameAddress{$ENDIF},
    TComponentWithOverrideValidateRename,
    @TComponentWithOverrideValidateRename.ValidateRename);
end;

var
  MyFindGlobalComponentProc: function(const AName: AnsiString): TComponent of object;

function MyFindGlobalComponent(const AName: AnsiString): TComponent;
begin
  Result:=MyFindGlobalComponentProc(AName);
end;

function ClassAsString(AClass: TClass): string;
var
  ParentClass: TClass;
begin
  Result:='Class='+DbgS(AClass);
  if AClass=nil then exit;
  Result:=Result+' Name="'+AClass.ClassName+'"';
  ParentClass:=AClass.ClassParent;
  if ParentClass<>nil then
    Result:=Result+' Parent='+DbgS(ParentClass)+'-"'+ParentClass.ClassName+'"';
  Result:=Result+LineEnding;
  Result:=Result+' vmtInstanceSize='+IntToStr(PLongInt(pointer(AClass)+vmtInstanceSize)^);
  Result:=Result+' vmtInstanceSizeNeg='+IntToStr(PLongInt(pointer(AClass)+vmtInstanceSizeNeg)^);
  Result:=Result+' vmtParent='+DbgS(pcardinal(pointer(AClass)+vmtParent)^);
  Result:=Result+' vmtClassName="'+PShortString((Pointer(AClass)+vmtClassName)^)^+'"';
  Result:=Result+' vmtDynamicTable='+DbgS(pcardinal(pointer(AClass)+vmtDynamicTable)^);
  Result:=Result+' vmtMethodTable='+DbgS(pcardinal(pointer(AClass)+vmtMethodTable)^);
  Result:=Result+' vmtFieldTable='+DbgS(pcardinal(pointer(AClass)+vmtFieldTable)^);
  Result:=Result+' vmtTypeInfo='+DbgS(pcardinal(pointer(AClass)+vmtTypeInfo)^);
  Result:=Result+' vmtInitTable='+DbgS(pcardinal(pointer(AClass)+vmtInitTable)^);
  Result:=Result+' vmtAutoTable='+DbgS(pcardinal(pointer(AClass)+vmtAutoTable)^);
  Result:=Result+' vmtIntfTable='+DbgS(pcardinal(pointer(AClass)+vmtIntfTable)^);
  Result:=Result+' vmtMsgStrPtr='+DbgS(pcardinal(pointer(AClass)+vmtMsgStrPtr)^);
  Result:=Result+LineEnding;
  Result:=Result+' MethodTable=['+ClassMethodTableAsString(AClass)+']';
  Result:=Result+LineEnding;
  Result:=Result+' TypeInfo=['+ClassTypeInfoAsString(AClass)+']';
  Result:=Result+LineEnding;
  Result:=Result+' FieldTable=['+ClassFieldTableAsString(AClass)+']';
end;

function ClassMethodTableAsString(AClass: TClass): string;
var
  MethodTable: PMethodNameTable;
  i: Integer;
begin
  Result:='';
  if AClass=nil then exit;
  MethodTable:=PMethodNameTable((Pointer(AClass)+vmtMethodTable)^);
  if MethodTable=nil then exit;
  for i:=0 to MethodTable^.Count-1 do begin
    if i>0 then Result:=Result+',';
    Result:=Result+IntToStr(i)+':"'+(MethodTable^.Entries[i].Name^)+'"'
      +':'+DbgS(MethodTable^.Entries[i].Addr);
  end;
end;

function ClassTypeInfoAsString(AClass: TClass): string;
var
  TypeInfo: PTypeInfo;
  TypeData: PTypeData;
  PropInfo: PPropInfo;
  CurCount: integer;
  i: Integer;
begin
  Result:='';
  if AClass=nil then exit;
  TypeInfo:=AClass.ClassInfo;
  if TypeInfo=nil then exit;
  Result:=Result+'ClassInfo^.Name="'+TypeInfo^.Name+'"';
  // read all property infos of current class
  TypeData:=GetTypeData(TypeInfo);
  if TypeData=nil then exit;
  Result:=Result+' ClassType='+DbgS(TypeData^.ClassType);
  if TypeData^.ClassType<>AClass then
    Result:=Result+LineEnding
      +' WARNING: ClassType<>AClass('+DbgS(AClass)+')'+LineEnding;
  Result:=Result+' ParentInfo='+DbgS(TypeData^.ParentInfo);
  if (AClass.ClassParent<>nil)
  and (TypeData^.ParentInfo<>AClass.ClassParent.ClassInfo) then
    Result:=Result+LineEnding
      +' WARNING: TypeData^.ParentInfo<>AClass.ClassParent.ClassInfo('
      +DbgS(TypeData^.ParentInfo)+'<>'
      +DbgS(AClass.ClassParent.ClassInfo)+'<>'+')'+LineEnding;
  Result:=Result+' PropCount='+IntToStr(TypeData^.PropCount);
  Result:=Result+' UnitName="'+TypeData^.UnitName+'"';

  // skip unitname
  PropInfo:=(@TypeData^.UnitName+Length(TypeData^.UnitName)+1);
  // read property count
  CurCount:=PWord(PropInfo)^;
  Result:=Result+' CurPropCnt='+IntToStr(CurCount);
  inc(PropInfo,SizeOf(Word));

  // read properties
  Result:=Result+' Properties={';
  for i:=0 to CurCount-1 do begin
    if i>0 then Result:=Result+',';
    // point PropInfo to next propinfo record.
    // Located at Name[Length(Name)+1] !
    Result:=Result+IntToStr(i)+':PropName="'+PropInfo^.Name+'"'
                  +':Type="'+PropInfo^.PropType^.Name+'"';
    PropInfo:=PPropInfo(pointer(@PropInfo^.Name)+PByte(@PropInfo^.Name)^+1);
  end;
  Result:=Result+'}';
end;

function ClassFieldTableAsString(AClass: TClass): string;
var
  FieldTable: PFieldTable;
  FieldInfo: PFieldInfo;
  i: Integer;
  ClassTable: PFieldClassTable;
begin
  Result:='';
  if AClass=nil then exit;
  FieldTable:=PFieldTable((Pointer(AClass)+vmtFieldTable)^);
  if FieldTable=nil then exit;
  Result:=Result+'FieldCount='+IntToStr(FieldTable^.FieldCount);
  ClassTable:=FieldTable^.ClassTable;
  Result:=Result+' ClassTable='+DbgS(ClassTable);
  if ClassTable<>nil then begin
    Result:=Result+'={';
    for i:=0 to ClassTable^.Count-1 do begin
      if i>0 then Result:=Result+',';
      Result:=Result+IntToStr(i)+':Name="'+ClassTable^.Entries[i].ClassName+'"';
    end;
  end;
  Result:=Result+'}';
  FieldInfo := @FieldTable^.Fields;
  Result := Result + ' Fields={';
  for i := 0 to FieldTable^.FieldCount-1 do begin
    if i > 0 then Result:=Result+',';
    Result := Result + IntToStr(i) 
      + ':Name="' + FieldInfo^.Name + '"'
      + ':Offset=' +IntToStr(FieldInfo^.FieldOffset);
    FieldInfo := @FieldInfo^.Name + 1 + Length(FieldInfo^.Name);
    {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
    FieldInfo := Align(FieldInfo, SizeOf(Pointer));
    {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  end;
  Result := Result+'}';
end;

//------------------------------------------------------------------------------


{ TJITComponentList }

constructor TJITComponentList.Create;
begin
  inherited Create;
  FComponentPrefix:='Form';
  FJITComponents:=TList.Create;
end;

destructor TJITComponentList.Destroy;
begin
  while FJITComponents.Count>0 do DestroyJITComponent(FJITComponents.Count-1);
  FJITComponents.Free;
  inherited Destroy;
end;

function TJITComponentList.GetItem(Index:integer):TComponent;
begin
  Result:=TComponent(FJITComponents[Index]);
end;

function TJITComponentList.Count:integer;
begin
  Result:=FJITComponents.Count;
end;

function TJITComponentList.IndexOf(JITComponent:TComponent):integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result]<>JITComponent) do dec(Result);
end;

function TJITComponentList.Contains(JITComponent: TComponent): boolean;
begin
  Result:=IndexOf(JITComponent)>=0;
end;

procedure TJITComponentList.DestroyJITComponent(JITComponent:TComponent);
var a:integer;
begin
  if JITComponent=nil then
    RaiseException('TJITComponentList.DestroyJITForm JITComponent=nil');
  a:=IndexOf(JITComponent);
  if a<0 then
    RaiseException('TJITComponentList.DestroyJITForm JITComponent.ClassName='+
      JITComponent.ClassName);
  if a>=0 then DestroyJITComponent(a);
end;

procedure TJITComponentList.DestroyJITComponent(Index:integer);
var
  OldClass: TClass;
begin
  OldClass:=Items[Index].ClassType;
  Items[Index].Free;
  FreeJITClass(OldClass);
  FJITComponents.Delete(Index);
end;

function TJITComponentList.FindComponentByClassName(
  const AClassName:shortstring):integer;
begin
  Result:=FJITComponents.Count-1;
  while (Result>=0)
  and (AnsiCompareText(Items[Result].ClassName,AClassName)<>0) do
    dec(Result);
end;

function TJITComponentList.FindComponentByName(const AName:shortstring):integer;
begin
  Result:=FJITComponents.Count-1;
  while (Result>=0)
  and (AnsiCompareText(Items[Result].Name,AName)<>0) do
    dec(Result);
end;

procedure TJITComponentList.GetUnusedNames(
  var ComponentName,ComponentClassName:shortstring);
var a:integer;
begin
  a:=1;
  repeat
    ComponentName:=ComponentPrefix+IntToStr(a);
    ComponentClassName:='T'+ComponentPrefix+IntToStr(a);
    inc(a);
  until (FindComponentByName(ComponentName)<0)
        and (FindComponentByClassName(ComponentClassName)<0);
end;

function TJITComponentList.GetClassNameFromStream(s:TStream):shortstring;
var Signature:shortstring;
  NameLen:byte;
begin
  Result:='';
  // read signature
  Signature:='1234';
  s.Read(Signature[1],length(Signature));
  if Signature<>'TPF0' then exit;
  // read classname length
  NameLen:=0;
  s.Read(NameLen,1);
  if (NameLen and $f0) = $f0 then begin
    { Skip Flag Byte }
    s.Read(NameLen,1);
  end;
  // read classname
  if NameLen>0 then begin
    SetLength(Result,NameLen);
    s.Read(Result[1],NameLen);
  end;
  s.Position:=0;
end;

function TJITComponentList.AddNewJITComponent(const NewUnitName: shortstring;
  ParentClass: TClass): integer;
var
  NewComponentName, NewClassName: shortstring;
begin
  {$IFDEF VerboseJITForms}
  debugln('[TJITComponentList] AddNewJITComponent');
  {$ENDIF}
  GetUnusedNames(NewComponentName,NewClassName);
  {$IFDEF VerboseJITForms}
  debugln('TJITComponentList.AddNewJITComponent NewComponentName=',NewComponentName,' NewClassName=',NewClassName,
    ' NewUnitName=',NewUnitName,' ParentClass=',ParentClass.ClassName);
  {$ENDIF}
  Result:=DoCreateJITComponent(NewComponentName,NewClassName,NewUnitName,
                               ParentClass);
end;

function TJITComponentList.AddJITComponentFromStream(BinStream: TStream;
  ParentClass: TClass; const NewUnitName: ShortString; Interactive: Boolean
  ):integer;
//  returns new index
// -1 = invalid stream
var
  Reader:TReader;
  NewClassName: shortstring;
  NewName: string;
  DestroyDriver: Boolean;
begin
  Result:=-1;
  NewClassName:=GetClassNameFromStream(BinStream);
  if NewClassName='' then begin
    //MessageDlg('No classname in stream found.',mtError,[mbOK],0);
    WriteLN('JUDISON: No classname in stream found.');
    exit;
  end;
  {$IFDEF VerboseJITForms}
  writeln('[TJITComponentList.AddJITComponentFromStream] Create ...');
  {$ENDIF}
  try
    Result:=DoCreateJITComponent('',NewClassName,NewUnitName,ParentClass);
    {$IFDEF VerboseJITForms}
    writeln('[TJITComponentList.AddJITComponentFromStream] InitReading ...');
    {$ENDIF}

    DestroyDriver:=false;
    InitReading(BinStream,Reader,DestroyDriver);
    {$IFDEF VerboseJITForms}
    writeln('[TJITComponentList.AddJITComponentFromStream] Read ...');
    {$ENDIF}
    try
      Reader.ReadRootComponent(FCurReadJITComponent);
      if FCurReadJITComponent.Name='' then begin
        NewName:=FCurReadJITComponent.ClassName;
        if NewName[1] in ['T','t'] then
          System.Delete(NewName,1,1);
        FCurReadJITComponent.Name:=NewName;
      end;

      {$IFDEF VerboseJITForms}
      writeln('[TJITComponentList.AddJITComponentFromStream] Finish Reading ...');
      {$ENDIF}
      DoFinishReading;
    finally
      UnregisterFindGlobalComponentProc(@MyFindGlobalComponent);
      if DestroyDriver then Reader.Driver.Free;
      Reader.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('[TJITComponentList.AddJITChildComponentFromStream] ERROR reading form stream'
         +' of Class ''',NewClassName,''' Error: ',E.Message);
      Result:=-1;
    end;
  end;
end;

function TJITComponentList.OnFindGlobalComponent(
  const AName: AnsiString): TComponent;
begin
  Result:=Application.FindComponent(AName);
end;

procedure TJITComponentList.InitReading(BinStream: TStream;
  var Reader: TReader; DestroyDriver: Boolean);
begin
  FFlags:=FFlags-[jclAutoRenameComponents];
  
  DestroyDriver:=false;
  Reader:=CreateXRCReader(BinStream,DestroyDriver);
  MyFindGlobalComponentProc:=@OnFindGlobalComponent;
  RegisterFindGlobalComponentProc(@MyFindGlobalComponent);

  {$IFDEF VerboseJITForms}
  writeln('[TJITComponentList.InitReading] A');
  {$ENDIF}
  // connect TReader events
  Reader.OnError:=@ReaderError;
  Reader.OnFindMethod:=@ReaderFindMethod;
  {$IFNDEF VER1_0}
  Reader.OnPropertyNotFound:=@ReaderPropertyNotFound;
  Reader.OnSetMethodProperty:=@ReaderSetMethodProperty;
  {$ENDIF}
  Reader.OnSetName:=@ReaderSetName;
  Reader.OnReferenceName:=@ReaderReferenceName;
  Reader.OnAncestorNotFound:=@ReaderAncestorNotFound;
  Reader.OnCreateComponent:=@ReaderCreateComponent;
  Reader.OnFindComponentClass:=@ReaderFindComponentClass;

  {$IFDEF VerboseJITForms}
  writeln('[TJITComponentList.InitReading] B');
  {$ENDIF}

  FCurReadChildClass:=nil;
  FCurReadChild:=nil;
  FCurReadErrorMsg:='';
end;

function TJITComponentList.DoCreateJITComponent(
  const NewComponentName, NewClassName, NewUnitName: shortstring;
  ParentClass: TClass):integer;
var
  Instance:TComponent;
  ok: boolean;
begin
  Result:=-1;
  // create new class and an instance
  //debugln('[TJITForms.DoCreateJITComponent] Creating new JIT class '''+NewClassName+''' ...');
  Pointer(FCurReadClass):=CreateNewJITClass(ParentClass,NewClassName,
                                            NewUnitName);
  //debugln('[TJITForms.DoCreateJITComponent] Creating an instance of JIT class "'+NewClassName+'" = class('+ParentClass.ClassName+') ...');
  Instance:=TComponent(FCurReadClass.NewInstance);
  //debugln('[TJITForms.DoCreateJITComponent] Initializing new instance ... ',DbgS(Instance));
  TComponent(FCurReadJITComponent):=Instance;
  ok:=false;
  try
    // set into design mode
    SetComponentDesignMode(Instance,true);
    // finish 'create' component
    Instance.Create(nil);
    if NewComponentName<>'' then
      Instance.Name:=NewComponentName;
    DoRenameClass(FCurReadClass,NewClassName);
    ok:=true;
  //debugln('[TJITForms.DoCreateJITComponent] Initialization was successful! FormName="',NewFormName,'"');
  finally
    if not ok then begin
      TComponent(FCurReadJITComponent):=nil;
      DebugLn('[TJITForms.DoCreateJITComponent] Error while creating instance');
    end;
  end;
  Result:=FJITComponents.Add(FCurReadJITComponent);
end;

procedure TJITComponentList.DoFinishReading;
begin

end;

procedure TJITComponentList.AddNewMethod(JITComponent:TComponent;
  const AName:ShortString);
begin
  CreateNewmethod(JITComponent,AName);
end;

procedure TJITComponentList.RemoveMethod(JITComponent:TComponent;
  const AName:ShortString);
var OldCode:Pointer;
begin
  {$IFDEF VerboseJITForms}
  writeln('TJITComponentList.RemoveMethod ',JITComponent.Name,':',JITComponent.Name,' Method=',AName);
  {$ENDIF}
  if JITComponent=nil then
    raise Exception.Create('TJITComponentList.RemoveMethod JITComponent=nil');
  if IndexOf(JITComponent)<0 then
    raise Exception.Create('TJITComponentList.RemoveMethod JITComponent.ClassName='+
      JITComponent.ClassName);
  if (AName='') or (not IsValidIdent(AName)) then
    raise Exception.Create('TJITComponentList.RemoveMethod invalid name: "'+AName+'"');
  OldCode:=nil;
  DoRemoveMethod(JITComponent.ClassType,AName,OldCode);
  FreeMem(OldCode);
end;

procedure TJITComponentList.RenameMethod(JITComponent:TComponent;
  const OldName,NewName:ShortString);
begin
  {$IFDEF VerboseJITForms}
  writeln('TJITComponentList.RenameMethod ',JITComponent.Name,':',JITComponent.Name,' Old=',OldName,' NewName=',NewName);
  {$ENDIF}
  if JITComponent=nil then
    raise Exception.Create('TJITComponentList.RenameMethod JITComponent=nil');
  if IndexOf(JITComponent)<0 then
    raise Exception.Create('TJITComponentList.RenameMethod JITComponent.ClassName='+
      JITComponent.ClassName);
  if (NewName='') or (not IsValidIdent(NewName)) then
    raise Exception.Create('TJITComponentList.RenameMethod invalid name: "'+NewName+'"');
  DoRenameMethod(JITComponent.ClassType,OldName,NewName);
end;

procedure TJITComponentList.RenameComponentClass(JITComponent:TComponent;
  const NewName:ShortString);
begin
  {$IFDEF VerboseJITForms}
  writeln('TJITComponentList.RenameComponentClass ',JITComponent.Name,':',JITComponent.Name,' New=',NewName);
  {$ENDIF}
  if JITComponent=nil then
    raise Exception.Create('TJITComponentList.RenameComponentClass JITComponent=nil');
  if IndexOf(JITComponent)<0 then
    raise Exception.Create('TJITComponentList.RenameComponentClass JITComponent.ClassName='+
      JITComponent.ClassName);
  if (NewName='') or (not IsValidIdent(NewName)) then
    raise Exception.Create('TJITComponentList.RenameComponentClass invalid name: "'+NewName+'"');
  DoRenameClass(JITComponent.ClassType,NewName);
end;

function TJITComponentList.AddJITChildComponentFromStream(
  JITOwnerComponent: TComponent; BinStream: TStream;
  ComponentClass: TComponentClass; ParentControl: TControl): TComponent;
var
  Reader: TReader;
  NewComponent: TComponent;
  DestroyDriver: Boolean;
begin
  Result:=nil;
  NewComponent:=nil;
  if IndexOf(JITOwnerComponent)<0 then
    RaiseException('TJITComponentList.AddJITChildComponentFromStream');
  {$IFDEF VerboseJITForms}
  writeln('[TJITComponentList.AddJITChildComponentFromStream] A');
  {$ENDIF}
  try
    DestroyDriver:=false;
    InitReading(BinStream,Reader,DestroyDriver);
    {$IFDEF VerboseJITForms}
    writeln('[TJITComponentList.AddJITChildComponentFromStream] B');
    {$ENDIF}
    try
      FCurReadJITComponent:=JITOwnerComponent;
      FCurReadClass:=JITOwnerComponent.ClassType;

      FFlags:=FFlags+[jclAutoRenameComponents];
      {$IFDEF VerboseJITForms}
      writeln('[TJITComponentList.AddJITChildComponentFromStream] C1 ',ComponentClass.ClassName);
      {$ENDIF}
      Reader.Root := FCurReadJITComponent;
      Reader.Owner := FCurReadJITComponent;
      Reader.Parent := ParentControl;
      Reader.BeginReferences;
      try
        Reader.Driver.BeginRootComponent;
        NewComponent:=Reader.ReadComponent(nil);
        Reader.FixupReferences;
      finally
        Reader.EndReferences;
      end;
      DebugLn('[TJITComponentList.AddJITChildComponentFromStream] C6 ');

      {$IFDEF VerboseJITForms}
      writeln('[TJITComponentList.AddJITChildComponentFromStream] D');
      {$ENDIF}
      DoFinishReading;
    finally
      UnregisterFindGlobalComponentProc(@MyFindGlobalComponent);
      if DestroyDriver then Reader.Driver.Free;
      Reader.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('[TJITComponentList.AddJITChildComponentFromStream] ERROR reading form stream'
         +' of Class ''',ComponentClass.ClassName,''' Error: ',E.Message);
    end;
  end;
  Result:=NewComponent;
end;

function TJITComponentList.CreateNewMethod(JITComponent: TComponent;
  const AName: ShortString): TMethod;
var CodeTemplate,NewCode:Pointer;
  CodeSize:integer;
  OldCode: Pointer;
begin
  {$IFDEF VerboseJITForms}
  writeln('TJITComponentList.CreateNewMethod ',JITComponent.Name,':',JITComponent.Name,' Method=',AName);
  {$ENDIF}
  if JITComponent=nil then
    raise Exception.Create('TJITComponentList.CreateNewMethod JITComponent=nil');
  if IndexOf(JITComponent)<0 then
    raise Exception.Create('TJITComponentList.CreateNewMethod JITComponent.ClassName='+
      JITComponent.ClassName);
  if (AName='') or (not IsValidIdent(AName)) then
    raise Exception.Create('TJITComponentList.CreateNewMethod invalid name: "'+AName+'"');
  OldCode:=JITComponent.MethodAddress(AName);
  if OldCode<>nil then begin
    Result.Data:=JITComponent;
    Result.Code:=OldCode;
    exit;
  end;
  CodeTemplate:=MethodAddress('DoNothing');
  CodeSize:=100; // !!! what is the real codesize of DoNothing? !!!
  GetMem(NewCode,CodeSize);
  Move(CodeTemplate^,NewCode^,CodeSize);
  DoAddNewMethod(JITComponent.ClassType,AName,NewCode);
  Result.Data:=JITComponent;
  Result.Code:=NewCode;
end;

procedure TJITComponentList.SetComponentPrefix(const AValue: string);
begin
  if FComponentPrefix=AValue then exit;
  FComponentPrefix:=AValue;
end;

function TJITComponentList.CreateNewJITClass(ParentClass: TClass;
  const NewClassName, NewUnitName: ShortString): TClass;
// Create a new class (vmt, virtual method table, field table and typeinfo)
// that descends from ParentClass.
// The new class will have no new variables, now new methods and no new fields.
var
  NewVMT: Pointer;
  ClassNamePShortString: Pointer;
  NewFieldTable: PFieldTable;
  NewClassTable: PFieldClassTable;
  NewTypeInfo: PTypeInfo;
  NewTypeData: PTypeData;
  TypeInfoSize: Integer;
  TypeDataSize: Integer;
  AddedPropCount: PWord;
  vmtSize: Integer;
  vmtTailSize: Integer;
begin
  if ParentClass=nil then
    raise Exception.Create('CreateNewClass ParentClass=nil');
  if NewClassName='' then
    raise Exception.Create('CreateNewClass NewClassName empty');
  if not IsValidIdent(NewClassName) then
    raise Exception.Create('CreateNewClass NewClassName is not a valid identifier');
  if NewUnitName='' then
    raise Exception.Create('CreateNewClass NewUnitName empty');
  if not IsValidIdent(NewUnitName) then
    raise Exception.Create('CreateNewClass NewUnitName is not a valid identifier');
  Result:=nil;

  // create vmt
  vmtSize:=GetVMTSize(ParentClass);
  vmtTailSize:=vmtSize-vmtMethodStart;
  GetMem(NewVMT,vmtSize);
  FillChar(NewVMT^,vmtSize,0);

  // set vmtInstanceSize
  PPtrInt(NewVMT+vmtInstanceSize)^:=ParentClass.InstanceSize;
  PPtrInt(NewVMT+vmtInstanceSizeNeg)^:=-ParentClass.InstanceSize;

  // set vmtParent
  TClass(Pointer(NewVMT+vmtParent)^):=ParentClass;

  // set vmtClassName: create pointer to classname (PShortString)
  GetMem(ClassNamePShortString,SizeOf(ShortString));
  System.Move(NewClassName[0],ClassNamePShortString^,SizeOf(ShortString));
  Pointer(Pointer(NewVMT+vmtClassName)^):=ClassNamePShortString;// don't use
                 // PShortString, so that the compiler does not get silly ideas

  // set vmtFieldTable
  GetMem(NewFieldTable,SizeOf(TFieldTable));
  FillChar(NewFieldTable^,SizeOf(TFieldTable),0);
  PFieldTable(Pointer(NewVMT+vmtFieldTable)^):=NewFieldTable;

  // ClassTable
  GetMem(NewClassTable,SizeOf(Word));
  FillChar(NewClassTable^,SizeOf(Word),0);
  NewFieldTable^.ClassTable:=NewClassTable;

  // set vmtTypeInfo
  TypeDataSize := SizeOf(TTypeData) + 2; // TTypeData + one word for new prop count
  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  if TypeDataSize and (SizeOf(Pointer) - 1) <> 0
  then Inc(TypeDataSize, SizeOf(Pointer)); // a few bytes to much, but atleast enough 
  {$endif}
  TypeInfoSize := SizeOf(TTypeKind) + 1 + length(NewClassName) + TypeDataSize;
  {$warnings off}
  if SizeOf(TTypeKind)<>1 then
    raise Exception.Create('CreateNewClass SizeOf(TTypeInfo^.Kind)<>1');
  {$warnings on}
  GetMem(NewTypeInfo,TypeInfoSize);
  FillChar(NewTypeInfo^,TypeInfoSize,0);
  Pointer(Pointer(NewVMT+vmtTypeInfo)^):=NewTypeInfo;

  // set TypeInfo Kind and Name
  NewTypeInfo^.Kind:=tkClass;
  System.Move(NewClassName[0],NewTypeInfo^.Name[0],length(NewClassName)+1);
  NewTypeData:=GetTypeData(NewTypeInfo);
  if NewTypeData<>Pointer(Pointer(@NewTypeInfo^.Name[0])+1+length(NewClassName))
  then
    raise Exception.Create('CreateNewClass new aligned TypeData');

  // set TypeData (PropCount is the total number of properties)
  NewTypeData^.ClassType:=TClass(NewVMT);
  NewTypeData^.ParentInfo:=ParentClass.ClassInfo;
  NewTypeData^.PropCount:=GetTypeData(NewTypeData^.ParentInfo)^.PropCount;
  NewTypeData^.UnitName:=NewUnitName;
  AddedPropCount:=PWord(@(NewTypeData^.UnitName)+Length(NewTypeData^.UnitName)+1);
  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  AddedPropCount := Align(AddedPropCount, SizeOf(Pointer));
  {$endif}
  AddedPropCount^:=0;

  // copy the standard methods
  System.Move(Pointer(Pointer(ParentClass)+vmtMethodStart)^,
              Pointer(NewVMT+vmtMethodStart)^,
              vmtTailSize);

  // override 'ValidateRename' for TComponent descendents
  if ParentClass.InheritsFrom(TComponent) then begin
    Pointer(Pointer(NewVMT+TComponentValidateRenameOffset)^):=
                           @TComponentWithOverrideValidateRename.ValidateRename;
  end;

  Result:=TClass(NewVMT);
end;

procedure TJITComponentList.FreeJITClass(var AClass: TClass);

  procedure FreeMethodTableEntries(MethodTable: PMethodNameTable);
  var
    CurCount, i: integer;
    CurMethod: TMethodNameRec;
  begin
    if MethodTable=nil then exit;
    CurCount:=MethodTable^.Count;
    i:=CurCount;
    while i>0 do begin
      CurMethod:=MethodTable^.Entries[i-1];
      if CurMethod.Name<>nil then
        FreeMem(CurMethod.Name);
      if CurMethod.Addr<>nil then
        FreeMem(CurMethod.Addr);
      dec(i);
    end;
  end;

var
  OldVMT: Pointer;
  ClassNamePShortString: Pointer;
  OldFieldTable: PFieldTable;
  OldTypeInfo: PTypeInfo;
  OldMethodTable: PMethodNameTable;
begin
  OldVMT:=Pointer(AClass);
  // free methodtable
  OldMethodTable:=PMethodNameTable((OldVMT+vmtMethodTable)^);
  if Assigned(OldMethodTable) then begin
    FreeMethodTableEntries(OldMethodTable);
    FreeMem(OldMethodTable);
  end;
  // free classname
  ClassNamePShortString:=Pointer(Pointer(OldVMT+vmtClassName)^);
  FreeMem(ClassNamePShortString);
  // free field table
  OldFieldTable:=PFieldTable(Pointer(OldVMT+vmtFieldTable)^);
  ReallocMem(OldFieldTable^.ClassTable,0);
  FreeMem(OldFieldTable);
  // free typeinfo
  OldTypeInfo:=PTypeInfo(Pointer(OldVMT+vmtTypeInfo)^);
  FreeMem(OldTypeInfo);
  // free vmt
  FreeMem(OldVMT);
  AClass:=nil;
end;

procedure TJITComponentList.DoAddNewMethod(JITClass:TClass;
  const AName:ShortString;  ACode:Pointer);
var OldMethodTable, NewMethodTable: PMethodNameTable;
  NewMethodTableSize:integer;
begin
  //writeln('[TJITComponentList.AddNewMethod] '''+JITClass.ClassName+'.'+AName+'''');
  OldMethodTable:=PMethodNameTable((Pointer(JITClass)+vmtMethodTable)^);
  if Assigned(OldMethodTable) then begin
    NewMethodTableSize:=SizeOf(DWord)+
                     (OldMethodTable^.Count + 1)*SizeOf(TMethodNameRec);
  end else begin
    NewMethodTableSize:=SizeOf(DWord)+SizeOf(TMethodNameRec);
  end;
  GetMem(NewMethodTable,NewMethodTableSize);
  if Assigned(OldMethodTable) then begin
    Move(OldMethodTable^,NewMethodTable^,
      NewMethodTableSize-SizeOf(TMethodNameRec));
    NewMethodTable^.Count:=NewMethodTable^.Count+1;
  end else begin
    NewMethodTable^.Count:=1;
  end;
  {$R-}
  //for a:=0 to NewMethodTable^.Count-2 do
  //  writeln(a,'=',NewMethodTable^.Entries[a].Name^,' $'
  //    ,DbgS(PtrInt(NewMethodTable^.Entries[a].Name),8));
  with NewMethodTable^.Entries[NewMethodTable^.Count-1] do begin
    GetMem(Name,256);
    Name^:=AName;
    Addr:=ACode;
  end;
  //for a:=0 to NewMethodTable^.Count-1 do
  //  writeln(a,'=',NewMethodTable^.Entries[a].Name^,' $'
  //    ,DbgS(PtrInt(NewMethodTable^.Entries[a].Name),8));
  {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
  PMethodNameTable((Pointer(JITClass)+vmtMethodTable)^):=NewMethodTable;
  if Assigned(OldMethodTable) then
    FreeMem(OldMethodTable);
end;

procedure TJITComponentList.DoRemoveMethod(JITClass:TClass;
  AName:ShortString; var OldCode:Pointer);
// Note: does not free OldCode
var OldMethodTable, NewMethodTable: PMethodNameTable;
  NewMethodTableSize:integer;
  a:cardinal;
begin
  {$IFDEF VerboseJITForms}
  writeln('[TJITComponentList.DoRemoveMethod] '''+JITClass.ClassName+'.'+AName+'''');
  {$ENDIF}
  AName:=uppercase(AName);
  OldMethodTable:=PMethodNameTable((Pointer(JITClass)+vmtMethodTable)^);
  OldCode:=nil;
  if Assigned(OldMethodTable) then begin
    a:=0;
    while a<OldMethodTable^.Count do begin
      {$R-}
      if uppercase(OldMethodTable^.Entries[a].Name^)=AName then begin
        OldCode:=OldMethodTable^.Entries[a].Addr;
        FreeMem(OldMethodTable^.Entries[a].Name);
        if OldMethodTable^.Count>0 then begin
          NewMethodTableSize:=SizeOf(DWord)+
                              OldMethodTable^.Count*SizeOf(TMethodNameRec);
          GetMem(NewMethodTable,NewMethodTableSize);
          NewMethodTable^.Count:=OldMethodTable^.Count-1;
          Move(OldMethodTable^,NewMethodTable^,
               SizeOf(DWord)+a*SizeOf(TMethodNameRec));
          Move(OldMethodTable^.Entries[a],NewMethodTable^.Entries[a+1],
               SizeOf(DWord)+a*SizeOf(TMethodNameRec));
        end else begin
          NewMethodTable:=nil;
        end;
        PMethodNameTable((Pointer(JITClass)+vmtMethodTable)^):=NewMethodTable;
        FreeMem(OldMethodTable);
        break;
      end;
      {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
      inc(a);
    end;
  end;
end;

procedure TJITComponentList.DoRenameMethod(JITClass:TClass;
  OldName,NewName:ShortString);
var MethodTable: PMethodNameTable;
  a:integer;
begin
  {$IFDEF VerboseJITForms}
  writeln('[TJITComponentList.DoRenameMethod] ClassName='''+JITClass.ClassName+''''
    +' OldName='''+OldName+''' NewName='''+OldName+'''');
  {$ENDIF}
  OldName:=uppercase(OldName);
  MethodTable:=PMethodNameTable((Pointer(JITClass)+vmtMethodTable)^);
  if Assigned(MethodTable) then begin
    for a:=0 to MethodTable^.Count-1 do begin
      if uppercase(MethodTable^.Entries[a].Name^)=OldName then
        MethodTable^.Entries[a].Name^:=NewName;
    end;
  end;
end;

procedure TJITComponentList.DoRenameClass(JITClass:TClass;
  const NewName:ShortString);
begin
  {$IFDEF VerboseJITForms}
  writeln('[TJITComponentList.DoRenameClass] OldName='''+JITClass.ClassName
    +''' NewName='''+NewName+''' ');
  {$ENDIF}
  PShortString((Pointer(JITClass)+vmtClassName)^)^:=NewName;
end;

//------------------------------------------------------------------------------

{
  TReader events.
  Normally at runtime a LFM is streamed back into the corresponding TForm
  descendent, all methods and components are published members and TReader can
  set these values.
  But at design time we do not have the corresponding TForm descendent. And
  there is no compiled code, thus it must be produced it at runtime
  (just-in-time).
}
procedure TJITComponentList.ReaderFindMethod(Reader: TReader;
  const FindMethodName: Ansistring;  var Address: Pointer; var Error: Boolean);
var NewMethod: TMethod;
begin
  {$IFDEF IDE_DEBUG}
  writeln('[TJITComponentList.ReaderFindMethod] A "'+FindMethodName+'" Address=',DbgS(Address));
  {$ENDIF}
  if Address=nil then begin
    // there is no method in the ancestor class with this name
    // => add a JIT method with this name to the JITForm
    NewMethod:=CreateNewMethod(FCurReadJITComponent,FindMethodName);
    Address:=NewMethod.Code;
    Error:=false;
  end;
end;

{$IFNDEF VER1_0}
procedure TJITComponentList.ReaderPropertyNotFound(Reader: TReader;
  Instance: TPersistent; var PropName: string; IsPath: Boolean;
  var Handled, Skip: Boolean);
begin
  DebugLn('TJITComponentList.ReaderPropertyNotFound ',Instance.ClassName,'.',PropName);
  if Assigned(OnPropertyNotFound) then
    OnPropertyNotFound(Self,Reader,Instance,PropName,IsPath,Handled,Skip);
end;

procedure TJITComponentList.ReaderSetMethodProperty(Reader: TReader;
  Instance: TPersistent; PropInfo: PPropInfo; const TheMethodName: string;
  var Handled: boolean);
begin
  //writeln('TJITComponentList.ReaderSetMethodProperty ',PropInfo^.Name,':=',TheMethodName);
end;
{$ENDIF}

procedure TJITComponentList.ReaderSetName(Reader: TReader;
  Component: TComponent; var NewName: Ansistring);
var
  CurName: String;
  i: Integer;
begin
//  writeln('[TJITComponentList.ReaderSetName] OldName="'+Component.Name+'" NewName="'+NewName+'"');
  if jclAutoRenameComponents in FFlags then begin
    while (NewName<>'') and (NewName[length(NewName)] in ['0'..'9']) do
      System.Delete(NewName,length(NewName),1);
    if NewName='' then
      NewName:=Component.ClassName;
    i:=0;
    repeat
      inc(i);
      CurName:=NewName+IntToStr(i);
    until FCurReadJITComponent.FindComponent(CurName)=nil;
    NewName:=CurName;
  end;
end;

procedure TJITComponentList.ReaderReferenceName(Reader: TReader; var RefName: Ansistring);
begin
//  writeln('[TJITComponentList.ReaderReferenceName] Name='''+RefName+'''');
end;

procedure TJITComponentList.ReaderAncestorNotFound(Reader: TReader;
  const ComponentName: Ansistring;  ComponentClass: TPersistentClass;
  var Component: TComponent);
begin
// ToDo: this is for custom form templates
//  writeln('[TJITComponentList.ReaderAncestorNotFound] ComponentName='''+ComponentName
//    +''' Component='''+Component.Name+'''');
end;

procedure TJITComponentList.ReaderError(Reader: TReader;
  const ErrorMsg: Ansistring; var Handled: Boolean);
// ToDo: use SUnknownProperty when it is published by the fpc team
const
  SUnknownProperty = 'Unknown property';
var
  ErrorType: TJITFormError;
  Action: TModalResult;
begin
  ErrorType:=jfeReaderError;
  Action:=mrCancel;
  FCurReadErrorMsg:=ErrorMsg;
  FCurUnknownProperty:=''; // ToDo find name property
  // find out, what error occured
  if RightStr(ErrorMsg,length(SUnknownProperty))=SUnknownProperty then begin
    ErrorType:=jfeUnknownProperty;
    Action:=mrIgnore;
  end;
  if Assigned(OnReaderError) then
    OnReaderError(Self,ErrorType,Action);
  Handled:=Action in [mrIgnore];
  FCurUnknownProperty:='';
  DebugLn('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>');
  DebugLn('[TJITComponentList.ReaderError] "'+ErrorMsg+'" ignoring=',BoolToStr(Handled));
  DebugLn('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<');
end;

procedure TJITComponentList.ReaderFindComponentClass(Reader: TReader;
  const FindClassName: Ansistring; var ComponentClass: TComponentClass);

var
//JUDISON
//  RegComp: TRegisteredComponent;
  RegComp: Tcomponent;
begin
  fCurReadChild:=nil;
  fCurReadChildClass:=ComponentClass;
  FCurUnknownClass:=FindClassName;
  if ComponentClass=nil then begin

    //JUDISON
    //RegComp:=IDEComponentPalette.FindComponent(FindClassName);
    RegComp := nil;

    if RegComp<>nil then begin
      //writeln('[TJITComponentList.ReaderFindComponentClass] '''+FindClassName
      //   +''' is registered');  original comment
      //JUDISON
      //ComponentClass:=RegComp.ComponentClass;

    end else begin
      DebugLn('[TJITComponentList.ReaderFindComponentClass] '''+FindClassName
         +''' is unregistered');
      // The reader will create a ReaderError automatically
    end;
  end;
  FCurUnknownClass:='';
end;

procedure TJITComponentList.ReaderCreateComponent(Reader: TReader;
  ComponentClass: TComponentClass; var Component: TComponent);
begin
  fCurReadChild:=Component;
  fCurReadChildClass:=ComponentClass;
//  writeln('[TJITComponentList.ReaderCreateComponent] Class='''+ComponentClass.ClassName+'''');
end;

procedure TJITComponentList.ReaderReadComponent(Component: TComponent);
begin
  DebugLn('TJITComponentList.ReaderReadComponent A ',Component.Name,':',Component.ClassName);
end;

//==============================================================================


{ TJITForms }

constructor TJITForms.Create;
begin
  inherited Create;
  FComponentPrefix:='Form';
end;

function TJITForms.IsJITForm(AComponent: TComponent): boolean;
begin
  Result:=(AComponent<>nil) and (AComponent is TForm)
      and (TForm(AComponent).Parent=nil) and (IndexOf(AComponent)>=0);
end;

function TJITForms.GetItem(Index: integer): TForm;
begin
  Result:=TForm(inherited Items[Index]);
end;

{ TJITNonFormComponents }

constructor TJITNonFormComponents.Create;
begin
  inherited Create;
  FComponentPrefix:='DataModule';
end;

function TJITNonFormComponents.IsJITNonForm(AComponent: TComponent): boolean;
begin
  Result:=(AComponent<>nil) and (not (AComponent is TForm))
          and (IndexOf(AComponent)>=0);
end;

initialization
  TComponentValidateRenameOffset:=GetTComponentValidateRenameVMTOffset;

end.

