(*
   $Id: txtbuffer.pas,v 1.5 2006/02/28 16:26:29 judison Exp $

   XDE - XCL's Development Environment
   Copyright (C) 2005 Judison Oliveira Gil Filho

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
    actEditClear: TAction;
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
    procedure EditClear(Sender: TObject);
    procedure EditClearUpd(Sender: TObject);
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

procedure TTxtBuffer.EditClear(Sender: TObject);
begin
  Buf.DeleteSelection(True, True);
end;

procedure TTxtBuffer.EditClearUpd(Sender: TObject);
begin
  actEditClear.Sensitive := Buf.SelText <> '';
end;

end.
{
$Log: txtbuffer.pas,v $
Revision 1.5  2006/02/28 16:26:29  judison
* New Resource System
* Better Editing Functions

Revision 1.4  2006/02/25 19:19:16  judison
Added GtkSourceView/XCLSourceView
Removed all deps on Scintilla

Revision 1.3  2005/12/18 05:05:59  judison
copyleft stuff

Revision 1.2  2005/12/17 21:48:30  judison
*** empty log message ***

Revision 1.1.1.1  2005/12/17 17:29:45  judison
Initial Import

Revision 1.1  2005/12/09 02:47:17  judison
Added TTxtBuffer, now TPasBuffer derives from TTxtBuffer


}
