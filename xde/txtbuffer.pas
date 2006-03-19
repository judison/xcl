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
    actEditUndo: TAction;
    actEditRedo: TAction;
    actEditCut: TAction;
    actEditCopy: TAction;
    actEditPaste: TAction;
    actEditDelete: TAction;
    MainBox: TVBox;
    TextBox: TVBox;
    procedure EditUndo(Sender: TObject);
    procedure EditUndoUpd(Sender: TObject);
    procedure EditRedo(Sender: TObject);
    procedure EditRedoUpd(Sender: TObject);
    procedure EditCut(Sender: TObject);
    procedure EditCutUpd(Sender: TObject);
    procedure EditCopy(Sender: TObject);
    procedure EditCopyUpd(Sender: TObject);
    procedure EditPaste(Sender: TObject);
    procedure EditPasteUpd(Sender: TObject);
    procedure EditDelete(Sender: TObject);
    procedure EditDeleteUpd(Sender: TObject);
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
  Modified := Buf.Modified;
end;

procedure TTxtBuffer.EditUndo(Sender: TObject);
begin
  Buf.Undo;
end;

procedure TTxtBuffer.EditUndoUpd(Sender: TObject);
begin
  actEditUndo.Sensitive := Buf.CanUndo;
end;

procedure TTxtBuffer.EditRedo(Sender: TObject);
begin
  Buf.Redo;
end;

procedure TTxtBuffer.EditRedoUpd(Sender: TObject);
begin
  actEditRedo.Sensitive := Buf.CanRedo;
end;

procedure TTxtBuffer.EditCut(Sender: TObject);
begin
  Buf.CutClipboard(Clipboard, True);
end;

procedure TTxtBuffer.EditCutUpd(Sender: TObject);
begin
  actEditCut.Sensitive := Buf.SelText <> '';
end;

procedure TTxtBuffer.EditCopy(Sender: TObject);
begin
  Buf.CopyClipboard(Clipboard);
end;

procedure TTxtBuffer.EditCopyUpd(Sender: TObject);
begin
  actEditCopy.Sensitive := Buf.SelText <> '';
end;

procedure TTxtBuffer.EditPaste(Sender: TObject);
begin
  Buf.PasteClipboard(Clipboard, True);
end;

procedure TTxtBuffer.EditPasteUpd(Sender: TObject);
begin
  actEditPaste.Sensitive := True; // How to know?? see TClipboard
end;

procedure TTxtBuffer.EditDelete(Sender: TObject);
begin
  Buf.DeleteSelection(True, True);
end;

procedure TTxtBuffer.EditDeleteUpd(Sender: TObject);
begin
  actEditDelete.Sensitive := Buf.SelText <> '';
end;

end.
