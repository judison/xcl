(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

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

uses Classes, SysUtils, xcl, Buffer, jitforms, typinfo, propeditor, DesignForm, pasbuffer;

type
  TFrmBuffer = class(TPasBuffer)
    ClientArea: TViewPort;
    ComponentTS: TTreeStore;
    FormBox: TVBox;
  private
    JITForms: TJITForms;
    procedure LoadComponents;
    function AddTree(C: TComponent; P: TTreeIter): TTreeIter;
  public
    MyForm: TDesignForm;
    //--
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //==
    procedure New; override;
    procedure Open(AFileName: String); override;
    procedure Save; override;
  end;

implementation

{ TFrmBuffer }

constructor TFrmBuffer.Create(AOwner: TComponent);
var
  Stream: TStream;
begin
  inherited;

  JITForms := TJitForms.Create;

  Stream := TResourceStream.Create('FORMDATA', 'TFrmBuffer');
  try
    Stream.ReadComponent(Self);
  finally
    Stream.Free;
  end;

  Name := Name + IntToStr(Integer(Pointer(Self)));

  ShowCtrls;

  FormBox.Visible := False;
end;

destructor TFrmBuffer.Destroy;
begin
  JITForms.Free;
  inherited;
end;

procedure TFrmBuffer.New;
begin
  JITForms.AddNewJITComponent('jit_unit', TDesignForm);
  MyForm := TDesignForm(JITForms[0]);
  MyForm.Parent := ClientArea;
  LoadComponents;
end;

procedure TFrmBuffer.Open(AFileName: String);
var
  Stream, BStream: TStream;
begin
  inherited Open(ChangeFileExt(AFileName, '.pas'));
  if FileExists(AFileName) then
  begin
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

  if C is TActionList then
    for I := 0 to TActionList(C).ActionCount -1 do
      AddTree(TActionList(C).Actions[I], It);

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
