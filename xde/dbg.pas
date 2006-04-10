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
  TDebugger = class(TComponent)
  private
    FIOChannel: Pointer;
    //==
    FDbgProcess: TProcess;
    FTargetProcess: TProcess;
    FDbgOutputBuf: String;
    //==
    FAcceptCmd: Boolean;
  protected
    procedure CreateDebugProcess;
    procedure SendCmd(const ACommand: String);
    procedure SendCmd(const ACommand: String; Values: array of const);
    procedure DoDbgOutputChar(C: Char);
    procedure DoDbgOutput(const AText: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //==
    function CreateTargetProcess(const ACommand: String): Boolean;
    function TargetProcessRunning: Boolean;
    //====================================
  public
    property DebugProcess: TProcess read FDbgProcess;
  end;

implementation

uses
{$IFDEF UNIX}
  Unix, BaseUnix,
{$ENDIF}
  glib2;

function hnd_io_std(source: PGIOChannel; condition: TGIOCondition; data: gpointer): gboolean; cdecl;
var
  C: Char;
  R: LongWord;
  Err: PGError;
begin
  try
    Err := nil;
    g_io_channel_read_chars(source, @C, 1, @R, @Err);
    RaiseGError(Err);
    //--
    TDebugger(data).DoDbgOutputChar(C);
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
  FTargetProcess := nil;
  //==
  CreateDebugProcess;
end;

destructor TDebugger.Destroy;
begin
  if Assigned(FDbgProcess) and (FDbgProcess.Running) then
    FDbgProcess.Terminate(0);

  try
    FreeAndNil(FDbgProcess);
  except
    on E: Exception do
      Application.ShowMessage(mtError, 'Exeption while freeing debugger: ' + E.Message);
  end;

  inherited;
end;

procedure TDebugger.CreateDebugProcess;
begin
  FAcceptCmd := False;

  FDbgProcess := TProcess.Create(Self);
  FDbgProcess.CommandLine := 'gdb -q';
  // TODO: under win9x and winMe should be created with console, otherwise no break can be sent.
  FDbgProcess.Options:= [poUsePipes, poNoConsole, poStdErrToOutPut, poNewProcessGroup];
  FDbgProcess.ShowWindow := swoNone;

  FDbgProcess.Execute;

  FIOChannel := g_io_channel_unix_new(FDbgProcess.Output.Handle);
  g_io_add_watch(FIOChannel, G_IO_IN, @hnd_io_std, Pointer(Self));
end;

function TDebugger.CreateTargetProcess(const ACommand: String): Boolean;
begin
  if Assigned(FTargetProcess) then
    FTargetProcess.Free;

  FTargetProcess := TProcess.Create(Self);
  FTargetProcess.CommandLine := ACommand;
  FTargetProcess.Options:= [poUsePipes, poNoConsole, poStdErrToOutPut];//, poRunSuspended];
  //FTargetProcess.ShowWindow := swoNone; ??
  FTargetProcess.Execute;

  Result := FTargetProcess.Running;

  if Result then
  begin
    SendCmd('attach %d', [FTargetProcess.Handle]);
    SendCmd('c');
  end;
end;

function TDebugger.TargetProcessRunning: Boolean;
begin
  Result := Assigned(FTargetProcess) and FTargetProcess.Running;
end;

procedure TDebugger.SendCmd(const ACommand: String);
begin
  if (not FAcceptCmd) and TargetProcessRunning then
  begin
{$IFDEF UNIX}
    fpKill(FTargetProcess.Handle, SIGINT);
{$ELSE}
    FTargetProcess.Suspend; // It's Right?
{$ENDIF}
    Application.ProcessMessages;
    if not FAcceptCmd then
      raise Exception.Create('Can''t interrupt target process.'+LineEnding+'Unable to send gdb command.');
  end;

  if ACommand <> '' then
    FDbgProcess.Input.Write(ACommand[1], Length(ACommand));

{$IFDEF WIN32}
  FDbgProcess.Input.Write(LineEnding[1], Length(LineEnding));
{$ELSE}
  FDbgProcess.Input.Write(LineEnding, Length(LineEnding));
{$ENDIF}
end;

procedure TDebugger.SendCmd(const ACommand: String; Values: array of const);
begin
  SendCmd(Format(ACommand, Values));
end;

procedure TDebugger.DoDbgOutputChar(C: Char);
begin
  if (C = #10) or (C = #13) then
  begin
    if Length(FDbgOutputBuf) > 0 then
      DoDbgOutput(FDbgOutputBuf);
    FDbgOutputBuf := '';
  end
  else
    FDbgOutputBuf := FDbgOutputBuf + C;
  //--
  if FDbgOutputBuf = '(gdb) ' then
  begin
    DoDbgOutput(FDbgOutputBuf);
    FDbgOutputBuf := '';
    FAcceptCmd := True;
  end
end;

procedure TDebugger.DoDbgOutput(const AText: String);
begin                          
  WriteLn('>>> ', AText);
end;


end.
