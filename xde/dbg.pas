(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit dbg;

{$H+}
{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}

interface

uses
  Classes, SysUtils, Process, XCL;

type
  PBreakpoint = ^TBreakpoint;
  TBreakpoint = record
    ID: Integer;
    SourceFile: String;
    SourceLine: Integer;
  end;

  TDebugger = class(TComponent)
  private
    FIOChannel: Pointer;
    //==
    FDbgProcess: TProcess;
    //==
    FTargetRunning: Boolean;
    FTargetPID: LongWord;
    FPrompt: Integer; //-1 Nothing, 0 pre-prompt, 1 prompt, 2 post-prompt
    //==
    FSendCmdPassed: Boolean;
  protected
    procedure CreateDebugProcess;
    procedure FinalizeDebugProcess;
    //--
    procedure DoDbgOutput(const AText: String);
    procedure SendCmd(const ACommand: String);
    procedure SendCmd(const ACommand: String; Values: array of const);
    //--
    procedure WaitForPrompt(AKill: Boolean = True);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //===========================
    procedure Load(AExeFile: String);
    procedure RunTarget;
    //===========================
    procedure InterruptTarget;
  public
    property TargetRunning: Boolean read FTargetRunning;
    // property TargetExists: Boolean;
    property DebugProcess: TProcess read FDbgProcess;
  end;

implementation

uses
{$IFDEF UNIX}
  Unix, BaseUnix,
{$ENDIF}
  glib2, formatread;

function hnd_io_std(source: PGIOChannel; condition: TGIOCondition; data: gpointer): gboolean; cdecl;
var
  L: PChar;
  S: String;
  R: LongWord;
  Err: PGError;
begin
  try
    Err := nil;
    g_io_channel_read_line(source, @L, nil, @R, @Err);
    RaiseGError(Err);
    //--
    S := L;
    S := Copy(S, 1, R);
    TDebugger(data).DoDbgOutput(S);
  except
    on E: Exception do
      Application.ShowException(E);
  end;
  Result := True;
end;

{ TDebugger }

constructor TDebugger.Create(AOwner: TComponent);
begin
  inherited;
  FSendCmdPassed := True;
  FPrompt := -1;
  FTargetRunning := False;
  FDbgProcess := nil;
end;

destructor TDebugger.Destroy;
begin
  if Assigned(FDbgProcess) then
    FinalizeDebugProcess;

  inherited;
end;

procedure TDebugger.CreateDebugProcess;
begin
  FDbgProcess := TProcess.Create(Self);
  FDbgProcess.CommandLine := 'gdb -q --annotate=3';
  // TODO: under win9x and winMe should be created with console, otherwise no break can be sent.
  FDbgProcess.Options:= [poUsePipes, poNoConsole, poStdErrToOutPut, poNewProcessGroup];
  FDbgProcess.ShowWindow := swoNone;

  FDbgProcess.Execute;

  FIOChannel := g_io_channel_unix_new(FDbgProcess.Output.Handle);
  g_io_add_watch(FIOChannel, G_IO_IN, @hnd_io_std, Pointer(Self));

  //== Configuration
  WaitForPrompt(False); SendCmd('set height 0');
  WaitForPrompt(False); SendCmd('set width 0');
end;

procedure TDebugger.FinalizeDebugProcess;
begin
  if (FDbgProcess.Running) then
    FDbgProcess.Terminate(0);

  FDbgProcess.Free;
  FDbgProcess := nil;
end;

procedure TDebugger.WaitForPrompt(AKill: Boolean = True);
begin
  if not Assigned(FDbgProcess) then
    CreateDebugProcess;
  //--
  while not FSendCmdPassed do
    Application.ProcessMessages;
  //--
  if AKill and (FPrompt <> 1) and TargetRunning then
    InterruptTarget;
  //--
  while FPrompt <> 1 do
    Application.ProcessMessages;
end;

procedure TDebugger.SendCmd(const ACommand: String);
begin
  if ACommand = '' then
    exit;

  if FPrompt <> 1 then
    raise Exception.Create('No gdb prompt');

  FDbgProcess.Input.Write(ACommand[1], Length(ACommand));
{$IFDEF WIN32}
  FDbgProcess.Input.Write(LineEnding[1], Length(LineEnding));
{$ELSE}
  FDbgProcess.Input.Write(LineEnding, Length(LineEnding));
{$ENDIF}
  FSendCmdPassed := False;

  WriteLn('gdb <<< ', ACommand);
end;

procedure TDebugger.SendCmd(const ACommand: String; Values: array of const);
begin
  SendCmd(Format(ACommand, Values));
end;

procedure TDebugger.DoDbgOutput(const AText: String);
var
  An: String;
  I1, I2: Integer;
  S1: String;
  IsGDB: Boolean;
begin
  IsGDB := True;
  if (Length(AText) > 2) and (AText[1] = #26) and (AText[2] = #26) then
  begin
    An := Copy(AText, 3, Length(AText) - 2);
    if An = 'pre-prompt' then
      FPrompt := 0
    else if An = 'prompt' then
      FPrompt := 1
    else if An = 'post-prompt' then
    begin
      FPrompt := 2;
      FSendCmdPassed := True;
    end
    else if An = 'starting' then
      FTargetRunning := True
    else if (An = 'stopped') or (An = 'signal') then
      FTargetRunning := False
  end
  else if FTargetRunning then
    if FmtRead('[New Thread %d (LWP %d)]', AText, [@I1, @I2]) then
      FTargetPID := I2
    else if FmtRead('[Thread debugging using libthread_db %s]', AText, [@S1]) then
    begin end
    else
      IsGDB := False;

  if not IsGDB then
    WriteLn('tgt >>> ', AText)
  else if Length(AText) > 0 then
    WriteLn('gdb >>> ', AText);
end;

procedure TDebugger.InterruptTarget;
begin
{$IFDEF UNIX}
  fpKill(FTargetPID, SIGINT);
{$ENDIF}
{$IFDEF WIN32}
  SuspendThread(FTargetPID); // Nao deve ser isso :(
{$ENDIF}
end;

procedure TDebugger.Load(AExeFile: String);
begin
  WaitForPrompt;
  SendCmd('file ' + AExeFile);
end;

procedure TDebugger.RunTarget;
begin
  WaitForPrompt;
  SendCmd('run');
end;

end.
