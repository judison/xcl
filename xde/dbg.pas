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
  TDebugger = class;

  PBreakpoint = ^TBreakpoint;
  TBreakpoint = record
    ID: Integer; // -1 = unset at gdb
    SourceFile: String;
    SourceLine: Integer;
    // Condition????
  end;

  TBreakpointList = class
  private
    FDebugger: TDebugger;
    FList: TList;
    function GetItem(AIndex: Integer): PBreakpoint;
    function GetCount: Integer;
    //--
    procedure SyncItem(AItem: PBreakpoint);
  public
    constructor Create(ADebugger: TDebugger);
    destructor Destroy; override;
    //--
    procedure Add(AFile: String; ALine: Integer);
    procedure Delete(AIndex: Integer);
    procedure Clear;
    //--
    function ItemByID(AID: Integer): PBreakpoint;
    //==
    procedure Invalidate;
    //==
    property Items[AIndex: Integer]: PBreakpoint read GetItem; default;
    property Count: Integer read GetCount;
  end;

  TDebugger = class(TComponent)
  private
    FIOChannel: Pointer;
    //==
    FDbgProcess: TProcess;
    //==
    FTargetRunning: Boolean;
    FTargetExists: Boolean;
    FTargetPID: LongWord;
    FPrompt: Integer; //-1 Nothing, 0 pre-prompt, 1 prompt, 2 post-prompt
    //==
    FSendCmdPassed: Boolean;
    FExecCmdOut: TStrings;
    FInExecCmd: Boolean;
    //==========
    FBreakpoints: TBreakpointList;
  protected
    procedure CreateDebugProcess;
    procedure FinalizeDebugProcess;
    //--
    procedure DoReadLine;
    procedure DoDbgOutput(const AText: String);

    function  ExecCmd(const ACommand: String): String;
    function  ExecCmd(const ACommand: String; const Args: array of const): String;
    procedure ExecCmd(const ACommand: String; AResult: TStrings);

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
    property TargetExists: Boolean read FTargetExists;
    property DebugProcess: TProcess read FDbgProcess;
    property Breakpoints: TBreakpointList read FBreakpoints;
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

{ TBreakpointList }

constructor TBreakpointList.Create(ADebugger: TDebugger);
begin
  FDebugger := ADebugger;
  FList := TList.Create;
end;

destructor TBreakpointList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TBreakpointList.GetItem(AIndex: Integer): PBreakpoint;
begin
  Result := PBreakpoint(FList.Items[AIndex]);
end;

function TBreakpointList.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TBreakpointList.Add(AFile: String; ALine: Integer);
var
  It: PBreakpoint;
begin
  New(It);
  try
    It^.ID := -1;
    It^.SourceFile := AFile;
    It^.SourceLine := ALine;
    FList.Add(It);
  except
    Dispose(It);
    raise;
  end;
  if FDebugger.TargetExists then
    SyncItem(It);
end;

procedure TBreakpointList.Delete(AIndex: Integer);
var
  It: PBreakpoint;
begin
  It := PBreakpoint(FList.Items[AIndex]);
  FList.Delete(AIndex);
  Dispose(It);
end;

procedure TBreakpointList.Clear;
begin
  while Count > 0 do
    Delete(0);
end;

function TBreakpointList.ItemByID(AID: Integer): PBreakpoint;
var
  I: Integer;
  It: PBreakpoint;
begin
  Result := nil;
  for I := 0 to Count-1 do
  begin
    It := GetItem(I);
    if It^.ID = AID then
    begin
      Result := It;
      Break;
    end;
  end;
end;

procedure TBreakpointList.Invalidate;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    GetItem(I)^.ID := -1;
end;

procedure TBreakpointList.SyncItem(AItem: PBreakpoint);
begin
  FDebugger.ExecCmd('break %s:%d', [AItem^.SourceFile, AItem^.SourceLine]);
  AItem^.ID := 0;
end;

{ TDebugger }

constructor TDebugger.Create(AOwner: TComponent);
begin
  inherited;
  FBreakpoints := TBreakpointList.Create(Self);

  FSendCmdPassed := True;
  FExecCmdOut := nil;
  FInExecCmd := False;
  FPrompt := -1;
  FTargetRunning := False;
  FTargetExists := False;
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

function TDebugger.ExecCmd(const ACommand: String): String;
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    ExecCmd(ACommand, SL);

    if SL.Count > 0 then
    begin
      Result := SL[0];
      for I := 1 to SL.Count -1 do
        Result := Result + LineEnding + SL.Text;
    end
    else
      Result := '';
  finally
    SL.Free;
  end;
end;

function TDebugger.ExecCmd(const ACommand: String; const Args: array of const): String;
begin
  Result := ExecCmd(Format(ACommand, Args));
end;

procedure TDebugger.ExecCmd(const ACommand: String; AResult: TStrings);
var
  R: Boolean;
begin
  FExecCmdOut := AResult;
  try
    R := TargetRunning;
    try
      WaitForPrompt(True);

      FInExecCmd := True;
      SendCmd(ACommand);
      while FInExecCmd do
        DoReadLine;
    finally
      if R and (FPrompt = 1) then
        SendCmd('continue');
    end;
  finally
    FExecCmdOut := nil;
    FInExecCmd := False;
  end;
end;

procedure TDebugger.DoReadLine;
var
  L: PChar;
  S: String;
  R: LongWord;
  Err: PGError;
begin
  Err := nil;
  g_io_channel_read_line(FIOChannel, @L, nil, @R, @Err);
  RaiseGError(Err);
  //--
  S := L;
  S := Copy(S, 1, R);
  DoDbgOutput(S);
end;

procedure TDebugger.DoDbgOutput(const AText: String);
var
  An: String;
  I1, I2: Integer;
  S1: String;
  IsGDB: Boolean;
  procedure ExecOut(Str: String);
  begin
    if Assigned(FExecCmdOut) and (Length(Str) > 0) and (FPrompt = 2) then
      FExecCmdOut.Add(Str);
  end;
begin
  // TODO:
  // - Setar TargetExists := False

  IsGDB := True;
  if (Length(AText) > 2) and (AText[1] = #26) and (AText[2] = #26) then
  begin
    An := Copy(AText, 3, Length(AText) - 2);
    if An = 'pre-prompt' then
      FPrompt := 0
    else if An = 'prompt' then
    begin
      FPrompt := 1;
      FInExecCmd := False;
    end
    else if An = 'post-prompt' then
    begin
      FPrompt := 2;
      FSendCmdPassed := True;
    end
    else if An = 'starting' then
      FTargetRunning := True
    else if (An = 'stopped') or (An = 'signal') then
      FTargetRunning := False
    else
      ExecOut(AText);
  end
  else if FTargetRunning then
    if FmtRead('[New Thread %d (LWP %d)]', AText, [@I1, @I2]) then
      FTargetPID := I2
    else if FmtRead('[Thread debugging using libthread_db %s]', AText, [@S1]) then
    begin end
    else
      IsGDB := False
  else
    ExecOut(AText);

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
  ExecCmd('file ' + AExeFile);
  FTargetExists := True;
end;

procedure TDebugger.RunTarget;
begin
  WaitForPrompt;
  SendCmd('run');
end;

end.
