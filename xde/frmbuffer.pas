(*
   XDE - XCL's Development Environment
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit frmbuffer;

{$H+}
{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}

interface

uses Classes, SysUtils, xcl, Buffer, jitforms, typinfo, propeditor, ComponentPalette;

type
  TDesignForm = class(TFixed)
  private
    FTitle: String;
    FWidth: Integer;
    FHeight: Integer;
    FResizable: Boolean;
    FDecorated: Boolean;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Title: string read FTitle write FTitle;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Resizable: Boolean read FResizable write FResizable default True;
    property Decorated: Boolean read FDecorated write FDecorated default True;
  end;

  TFrmBuffer = class(TBuffer)
    ClientArea: TViewPort;
    ComponentTS: TTreeStore;
    ComponentTV: TTreeView;
    PropTable: TTable;
    EventTable: TTable;
    ClientBox: TVBox;
    procedure CompChanged(Sender: TObject);
    procedure RemoveComp(Sender: TObject);
  private
    JITForms: TJITForms;
    MyForm: TDesignForm;
    CompEd: TComponentEditor;

    CompPalette: TComponentPalette;
    procedure LoadComponents;
    procedure PaletteClassSelected(Sender: TObject; AClass: TComponentClass);
    function AddTree(C: TComponent; P: TTreeIter): TTreeIter;
    procedure SelectComp(C: TComponent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //==
    procedure Open(AFileName: String); override;
    procedure Save; override;
  end;

implementation

{ TDesignForm }

constructor TDesignForm.Create(AOwner: TComponent);
begin
  inherited;
  FResizable := True;
  FDecorated := True;
  SetDesigning(True);
end;

procedure TDesignForm.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  if Root = Self then
    for I := 0 to ComponentCount - 1 do
      if not Components[I].HasParent
        then Proc(Components[I]);
  inherited GetChildren(Proc, Root);
end;

{ TFrmBuffer }

constructor TFrmBuffer.Create(AOwner: TComponent);
var
  Stream: TStream;
begin
  inherited;

  CompEd := nil;

  JITForms := TJitForms.Create;

  Stream := TResourceStream.Create('FORMDATA', 'TFrmBuffer');
  try
    Stream.ReadComponent(Self);
  finally
    Stream.Free;
  end;

  Name := Name + IntToStr(Integer(Pointer(Self)));

  CompPalette := TComponentPalette.Create(Self);
  CompPalette.Parent := ClientBox;
  CompPalette.BoxPosition := 0;
  CompPalette.BoxExpand := False;
  CompPalette.OnClassSelected := @PaletteClassSelected;

  ShowCtrls;
end;

destructor TFrmBuffer.Destroy;
begin
  JITForms.Free;
  inherited;
end;

procedure TFrmBuffer.PaletteClassSelected(Sender: TObject; AClass: TComponentClass);
var
  C: TComponent;
  It: TTreeIter;
begin
  if Assigned(CompEd) and Assigned(CompEd.Component) then
  begin
    C := AClass.Create(MyForm);
    try
      if (C is TControl) then
        if (CompEd.Component is TContainerControl) then
        begin
          TControl(C).Parent := TControl(CompEd.Component);
          ComponentTV.GetSelected(It);
        end
        else
          raise Exception.Create('A Control can only be place inside a Container Contol')
      else
        ComponentTS.GetIterFirst(It);
    except
      C.Free;
      raise;
    end;

    It := AddTree(C, It);
    ComponentTV.ExpandTo(It);
    ComponentTV.SelectIter(It);
  end;
end;

procedure TFrmBuffer.Open(AFileName: String);
var
  Stream, BStream: TStream;
begin
  inherited;
  Stream := TFileStream.Create(AFileName, fmOpenRead);
  try
    BStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(Stream, BStream);
      BStream.Position := 0;
      JITForms.AddJITComponentFromStream(BStream, TDesignForm, 'jit_unit', false);
    finally
      BStream.Free;
    end;
  finally
    Stream.Free;
  end;
  MyForm := TDesignForm(JITForms[0]);
  MyForm.Parent := ClientArea;
  LoadComponents;
end;

function TFrmBuffer.AddTree(C: TComponent; P: TTreeIter): TTreeIter;
var
  It: TTreeIter;
  I: Integer;
  S: String;
begin
  if C.Name <> '' then
    S := C.Name + ': ' + C.ClassName
  else
    S := '['+C.ClassName+']';

  ComponentTS.Append(It, P);
  ComponentTS.SetStringValue(It, 0, S);
  ComponentTS.SetPointerValue(It, 1, C);

  if C is TControl then
    for I := 0 to TControl(C).ControlCount -1 do
      AddTree(TControl(C).Controls[I], It);

  Result := It;
end;


procedure TFrmBuffer.LoadComponents;
var
  It: TTreeIter;
  I: Integer;
begin
  ComponentTS.Append(It);
  ComponentTS.SetStringValue(It, 0, MyForm.ClassName);
  ComponentTS.SetPointerValue(It, 1, MyForm);
  for I := 0 to MyForm.ComponentCount -1 do
    if (not (MyForm.Components[I] is TControl)) and (not (MyForm.Components[I] is TCustomAction))  then
      AddTree(MyForm.Components[I], It);
  for I := 0 to ControlCount -1 do
      AddTree(MyForm.Controls[I], It);

  SelectComp(MyForm);
end;

procedure TFrmBuffer.CompChanged(Sender: TObject);
var
  It: TTreeIter;
  C: TComponent;
begin
  if ComponentTV.GetSelected(It) then
  begin
    C := TComponent(ComponentTS.GetPointerValue(It, 1));

    if not Assigned(CompEd) then
    begin
      CompEd := TComponentEditor.Create(Self);
      CompEd.PropTable := PropTable;
      CompEd.EventTable := EventTable;
    end;

    CompEd.Component := C;
  end;
end;

procedure TFrmBuffer.SelectComp(C: TComponent);

  function FindIter(var A: TTreeIter): boolean;
  var
    B: TTreeIter;
  begin
    Result := False;
    if TComponent(ComponentTS.GetPointerValue(A, 1)) = C then
      exit(True);
    if ComponentTS.IterChildren(B, A) then
      if FindIter(B) then
      begin
        A := B;
        exit(True);
      end;
    if ComponentTS.IterNext(A) then
      Result := FindIter(A);
  end;

var
  It: TTreeIter;
begin
  if (C <> nil) and ComponentTS.GetIterFirst(It) and FindIter(It) then
  begin
    ComponentTV.ExpandTo(It);
    ComponentTV.SelectIter(It);

    if not Assigned(CompEd) then
    begin
      CompEd := TComponentEditor.Create(Self);
      CompEd.PropTable := PropTable;
      CompEd.EventTable := EventTable;
    end;

    CompEd.Component := C;
  end;
end;

procedure TFrmBuffer.RemoveComp(Sender: TObject);
var
  It: TTreeIter;
  C,P: TComponent;
begin
  if ComponentTV.GetSelected(It) then
  begin
    C := TComponent(ComponentTS.GetPointerValue(It, 1));
    if C <> MyForm then
    begin
      if C is TControl then
      begin
        P := TControl(C).Parent;
        TControl(C).Parent := nil;
      end
      else
        P := MyForm;
  
      ComponentTS.Remove(It);
      SelectComp(P);
      C.Free;
    end;
  end;
end;


procedure TFrmBuffer.Save;
var
  AStream, BStream: TStream;
  SL: TStringList;
begin
  inherited;
  //
  if FileExists(FileName) then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile(FileName);
      SL.SaveToFile(FileName+'~');
    finally
      SL.Free;
    end;
  end;

  AStream := TFileStream.Create(FileName, fmCreate);
  BStream := TMemoryStream.Create;
  try
    BStream.WriteComponent(MyForm);
    BStream.Position := 0;
    ObjectBinaryToText(BStream, AStream);
  finally
    BStream.Free;
    AStream.Free;
  end;
end;


end.
