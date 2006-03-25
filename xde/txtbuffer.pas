(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit txtbuffer;

{$H+}
{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}

interface

uses Classes, SysUtils, xcl, Buffer, xclsourceview;

type
  TTxtBuffer = class(TBuffer)
    Buf: TSourceBuffer;
    Edt: TSourceView;
    MainBox: TVBox;
    TextBox: TVBox;
    procedure BufChanged(Sender: TObject);
  private
  protected
    procedure InitEdt; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open(AFileName: String); override;
    procedure Save; override;
  end;

implementation

{ TTxtBuffer }

constructor TTxtBuffer.Create(AOwner: TComponent);
var
  Stream: TStream;
begin
  inherited;
  Stream := TResourceStream.Create('FORMDATA', 'TTxtBuffer');
  try
    Stream.ReadComponent(Self);
  finally
    Stream.Free;
  end;

  ShowCtrls;
  InitEdt;
end;

procedure TTxtBuffer.InitEdt;
begin
end;

destructor TTxtBuffer.Destroy;
begin
  inherited;
end;

procedure TTxtBuffer.Open(AFileName: String);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(AFileName);
    Buf.BeginNotUndoableAction;
    Buf.Text := SL.Text;
    Buf.Modified := False;
    Buf.EndNotUndoableAction;
  finally
    SL.Free;
  end;
  inherited;
end;

procedure TTxtBuffer.Save;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := Buf.Text;
    SL.SaveToFile(FileName);
  finally
    SL.Free;
  end;
  inherited;
end;

procedure TTxtBuffer.BufChanged(Sender: TObject);
begin
  Modified := True;
end;

end.
