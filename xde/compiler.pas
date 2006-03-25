(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit Compiler;

{$H+}
{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}

interface

uses Classes, SysUtils, XCL, Executor, XPR, CompilingDlg;

type
  TCompiler = class(TExecutor)
  private
    Frm: TFrmCompilingDlg;
    FHints: Integer;
    FWarnings: Integer;
    FErrors: Integer;
    FAborted: Boolean;
    procedure BtnStop(Sender: TObject);
    procedure BtnOK(Sender: TObject);
  protected
    procedure ReadLine(ALine: String); override;
  public
    procedure Compile(AProject: TXPRProject; BuildAll: Boolean);
  end;

implementation

uses gtk2, Main;

{ TCompiler }

function Starts(const AStr, ASubStr: String): Boolean; inline;
begin
  Result := Copy(AStr, 1, Length(ASubStr)) = ASubStr
end;

procedure TCompiler.ReadLine(ALine: String);
begin
  if Starts(ALine, 'Compiling ') or
     Starts(ALine, 'Assembling ') or
     Starts(ALine, 'Linking ') or
     (Pos(' Lines compiled, ', ALine) > 0) then
    Frm.lblStatus.Caption := ALine
  else if Starts(ALine, 'Panic') or Starts(ALine, 'Fatal: ') or Starts(ALine, 'Error: ') then
  begin
    Inc(FErrors);
    Frm.lblErrors.Caption := Format('<b>Errors:</b> %d', [FErrors]);
  end
  else if Starts(ALine, 'Warning: ') then
  begin
    Inc(FWarnings);
    Frm.lblWarnings.Caption := Format('<b>Warnings:</b> %d', [FWarnings]);
  end
  else if Starts(ALine, 'Hint: ') then
  begin
    Inc(FHints);
    Frm.lblHints.Caption := Format('<b>Hints:</b> %d', [FHints]);
  end;
  //WriteLn('>>> ', ALine);
  inherited;
end;

procedure TCompiler.Compile(AProject: TXPRProject; BuildAll: Boolean);
var
  CmdLine: String;
  ProjectFilename: String;
  OldCurDir, ProjectDir: String;
begin
  OldCurDir := GetCurrentDir;

  ProjectFilename := AProject.ProjectFile;
  ProjectDir := ExtractFilePath(ProjectFilename);

  if not SetCurrentDir(ProjectDir) then
    exit;
  try
    CmdLine := 'fpc ' + '-Fu../xcl -Fusourceview -Fu../xcl/extra -Fuicons';
    
    if BuildAll then
      CmdLine := CmdLine + ' -B';

    CmdLine := CmdLine+ ' '+ ProjectFilename;

    FHints := 0;
    FWarnings := 0;
    FErrors := 0;
    FAborted := False;

    Frm := TFrmCompilingDlg.Create(nil);
    try
      Frm.lblProject.Caption := Format('<b>Project:</b> %s', [AProject.Name]);
      Frm.lblStatus.Caption := 'Initializing Compiler...';
      Frm.Btn.Caption := '_Stop';
      Frm.Btn.IconName := 'gtk-stop';
      Frm.Btn.OnClicked := @BtnStop;

      gtk_window_set_modal(Frm.GetHandle, True);
      Frm.Show(MainForm);

      Execute(CmdLine);
      if FAborted then
      begin
        Frm.Title := 'Compilation Aborted';
        Frm.lblStatus.Caption := 'Aborted by User';
      end
      else
        Frm.Title := 'Compiled';


      Frm.Btn.Caption := '_OK';
      Frm.Btn.IconName := 'gtk-ok';
      Frm.Btn.OnClicked := @BtnOK;
      Frm.ShowModal(MainForm);
    finally
      FreeAndNil(Frm);
    end;
  finally
    SetCurrentDir(OldCurDir);
  end;
end;

procedure TCompiler.BtnStop(Sender: TObject);
begin
  Stop;
  FAborted := True;
end;

procedure TCompiler.BtnOK(Sender: TObject);
begin
  Frm.Close;
end;

end.