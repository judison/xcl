(*
   XCL - XDE's Component Library
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING.XCL, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
unit main;

{$H+}
{$MODE ObjFpc}

interface

uses Classes, SysUtils, xcl, syntax;

type
  TMainForm = class(TForm)
    MenuBar: TMenuBar;
    TV: TTextView;
    TB: TPasTextBuffer;
    OpenDialog: TFileChooserDialog;
    SaveAsDialog: TFileChooserDialog;
    procedure FileNew(Sender: TObject);
    procedure FileOpen(Sender: TObject);
    procedure FileSave(Sender: TObject);
    procedure FileSaveAs(Sender: TObject);
    procedure FileQuit(Sender: TObject);
    procedure EditUndo(Sender: TObject);
    procedure EditRedo(Sender: TObject);
    procedure EditCut(Sender: TObject);
    procedure EditCopy(Sender: TObject);
    procedure EditPaste(Sender: TObject);
    procedure EditDelete(Sender: TObject);
    procedure TBChanged(Sender: TObject);
  private
    FFileName: String;
    FModified: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  MainForm: TMainForm;

implementation

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  FFileName := '';
  FModified := False;
end;

procedure TMainForm.TBChanged(Sender: TObject);
begin
  FModified := True;
end;

procedure TMainForm.FileNew(Sender: TObject);
begin
  FFileName := '';
  FModified := False;
  TB.Text := '';
end;

procedure TMainForm.FileOpen(Sender: TObject);
begin
  if OpenDialog.Execute = -3 then
  begin
    TB.LoadFromFile(OpenDialog.FileName);
    FFileName := OpenDialog.FileName;
    FModified := False;
  end;
end;

procedure TMainForm.FileSave(Sender: TObject);
begin
  if FFileName = '' then
    FileSaveAs(Sender)
  else
  begin
    TB.SaveToFile(FFileName);
    FModified := False;
  end;
end;

procedure TMainForm.FileSaveAs(Sender: TObject);
begin
  if SaveAsDialog.Execute = -3 then
  begin
    TB.SaveToFile(SaveAsDialog.FileName);
    FFileName := SaveAsDialog.FileName;
    FModified := False;
  end;
end;

procedure TMainForm.FileQuit(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.EditUndo(Sender: TObject);
begin
end;

procedure TMainForm.EditRedo(Sender: TObject);
begin
end;

procedure TMainForm.EditCut(Sender: TObject);
begin
end;

procedure TMainForm.EditCopy(Sender: TObject);
begin
end;

procedure TMainForm.EditPaste(Sender: TObject);
begin
end;

procedure TMainForm.EditDelete(Sender: TObject);
begin
end;

end.
