(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
unit main;

{$H+}
{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}

interface

uses Classes, SysUtils, xcl, Buffer, xclsourceview;

type
  TMainForm = class(TForm)
    LangMan: TSourceLanguagesManager;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileClose: TAction;
    actFileQuit: TAction;
    MenuBar: TMenuBar;
    NB: TNoteBook;
    FS: TFileChooserDialog;
    procedure FileOpen(Sender: TObject);
    procedure FileNew(Sender: TObject);
    procedure FileSave(Sender: TObject);
    procedure FileSaveUpd(Sender: TObject);
    procedure FileSaveAs(Sender: TObject);
    procedure FileSaveAsUpd(Sender: TObject);
    procedure FileClose(Sender: TObject);
    procedure FileCloseUpd(Sender: TObject);
    procedure FileQuit(Sender: TObject);
    procedure ShowCompilerOptions(Sender: TObject);
    procedure ShowEditorOptions(Sender: TObject);
  private
    function CurrentBuffer: TBuffer;
  protected
    procedure DoCloseQuery(CanClose: Boolean); override;
  end;

var
  MainForm: TMainForm;

implementation

uses compiler_opts, editor_opts, TxtBuffer, PasBuffer, FrmBuffer, frm_NewFile;

{ TMainForm }

function TMainForm.CurrentBuffer: TBuffer;
begin
  Result := TBuffer(NB.Pages[NB.CurrentPage]);
end;

procedure TMainForm.FileNew(Sender: TObject);
var
  B: TBuffer;
begin
  B := nil;
  frmNewFile := TFrmNewFile.Create(nil);
  try
    frmNewFile.Tag := 0;
    frmNewFile.ShowModal;
    case frmNewFile.Tag of
      1: B := TPasBuffer.Create(Self);
      2: B := TFrmBuffer.Create(Self);
    end;
  finally
    frmNewFile.Free;
  end;
  if Assigned(B) then
    B.Parent := NB;
end;

procedure TMainForm.FileOpen(Sender: TObject);
var
  B: TBuffer;
  ext: String;
begin
  FS.FileAction := fcaOpen;
  FS.Title := 'Open...';
  if FS.Execute = -3 then
  begin
    ext := LowerCase(ExtractFileExt(FS.FileName));
    if (ext = '.pas') or (ext = '.pp') or (ext = '.inc') or (ext = '.dpr') or (ext = '.p') then
      B := TPasBuffer.Create(Self)
    else if (ext = '.frm') then
      B := TFrmBuffer.Create(Self)
    else
      B := TTxtBuffer.Create(Self);
    B.Parent := NB;
    B.Open(FS.FileName);
  end;
end;

procedure TMainForm.FileSave(Sender: TObject);
begin
  CurrentBuffer.Save;
end;

procedure TMainForm.FileSaveUpd(Sender: TObject);
begin
  actFileSave.Sensitive := CurrentBuffer <> nil;
end;

procedure TMainForm.FileSaveAs(Sender: TObject);
begin
  FS.FileAction := fcaSave;
  FS.Title := 'Save As...';
  if FS.Execute = -3 then
    CurrentBuffer.SaveAs(FS.FileName);
end;

procedure TMainForm.FileSaveAsUpd(Sender: TObject);
begin
  actFileSaveAs.Sensitive := CurrentBuffer <> nil;
end;

procedure TMainForm.FileClose(Sender: TObject);
begin
  CurrentBuffer.Free;
end;

procedure TMainForm.FileCloseUpd(Sender: TObject);
begin
  actFileClose.Sensitive := CurrentBuffer <> nil;
end;


procedure TMainForm.DoCloseQuery(CanClose: Boolean);
var
  Buffer: TBuffer;
  I: Integer;
begin
  inherited;
  for I := 0 to NB.PageCount -1 do
  begin
    Buffer := TBuffer(NB.Pages[I]);
    if Buffer.Modified then
      Abort;
      //CanClose := False;
  end;
end;

procedure TMainForm.FileQuit(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ShowCompilerOptions(Sender: TObject);
begin
  FrmCompilerOpts := TFrmCompilerOpts.Create(nil);
  try
    FrmCompilerOpts.ShowModal;
  finally
    FrmCompilerOpts.Free;
  end;
end;

procedure TMainForm.ShowEditorOptions(Sender: TObject);
begin
  FrmEditorOpts := TFrmEditorOpts.Create(nil);
  try
    FrmEditorOpts.ShowModal;
  finally
    FrmEditorOpts.Free;
  end;
end;

end.
