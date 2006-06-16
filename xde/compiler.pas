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

uses Classes, SysUtils, XCL, Executor, XPR, Frm_Compiling;

type
  TCompiler = class(TExecutor)
  private
    Frm: TFrmCompiling;
    FHints: Integer;
    FWarnings: Integer;
    FErrors: Integer;
    FAborted: Boolean;
    procedure BtnStop(Sender: TObject);
    procedure BtnOK(Sender: TObject);
  protected
    procedure ReadLine(ALine: String); override;
  public
    function Compile(AProject: TXPRProject; BuildAll: Boolean; AutoClose: Boolean): Boolean;
  end;

implementation

uses gtk2, frm_Main;

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
  inherited;
end;

function TCompiler.Compile(AProject: TXPRProject; BuildAll: Boolean; AutoClose: Boolean): Boolean;
var
  CmdLine: String;
  ProjectFilename: String;
  OldCurDir, ProjectDir: String;
  Opts: String;
  S: String;
begin
  OldCurDir := GetCurrentDir;

  ProjectFilename := AProject.ProjectFile;
  ProjectDir := ExtractFilePath(ProjectFilename);

  if not SetCurrentDir(ProjectDir) then
    exit(False);
  try
    // Read Project Options ===========
    Opts := '';
    //--
    S := AProject.Options.GetS('OutputPath');
    if S <> '' then
      Opts := Opts + ' -FE'+S;
    //--
    S := AProject.Options.GetS('UnitOutputPath');
    if S <> '' then
      Opts := Opts + ' -FU'+S;
    //--
    S := AProject.Options.GetS('UnitSearchPath');
    if S <> '' then
    begin
      S := '-Fu'+S;
      StringReplace(S, ';', ' -Fu', [rfReplaceAll]);
      Opts := Opts + ' ' + S
    end;
    //--
    S := AProject.Options.GetS('Conditionals');
    if S <> '' then
    begin
      S := '-d'+S;
      StringReplace(S, ';', ' -d', [rfReplaceAll]);
      Opts := Opts + ' ' + S
    end;
    //=================================
    CmdLine := 'fpc' + Opts;
    
    if BuildAll then
      CmdLine := CmdLine + ' -B';

    CmdLine := CmdLine+ ' '+ ProjectFilename;

    FHints := 0;
    FWarnings := 0;
    FErrors := 0;
    FAborted := False;

    Frm := TFrmCompiling.Create(nil);
    try
      Frm.lblProject.Caption := Format('<b>Project:</b> %s', [AProject.Name]);
      Frm.lblStatus.Caption := 'Initializing Compiler...';
      Frm.Btn.Caption := '_Stop';
      Frm.Btn.IconName := 'gtk-stop';
      Frm.Btn.OnClicked := @BtnStop;

      gtk_window_set_modal(Frm.GetHandle, True);
      Frm.Show(FrmMain);

      Execute(CmdLine);
      if FAborted then
      begin
        Frm.Title := 'Compilation Aborted';
        Frm.lblStatus.Caption := 'Aborted by User';
      end
      else
        Frm.Title := 'Done';

      if (not AutoClose) or FAborted then
      begin
        Frm.Btn.Caption := '_OK';
        Frm.Btn.IconName := 'gtk-ok';
        Frm.Btn.OnClicked := @BtnOK;
        Frm.ShowModal(FrmMain);
      end
      else
        Frm.Close;

      Result := (FErrors = 0) and (not FAborted);
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
