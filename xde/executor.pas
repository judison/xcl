(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit Executor;

{$H+}
{$MODE ObjFpc}

interface

uses Classes, SysUtils, Process, XCL;

type
  TReadLineEvent = procedure(Sender: TObject; ALine: String) of object;

  TExecutor = class
  private
    FOutput: TStringList;
    FProcess: TProcess;
    //--
    FForceStop: Boolean;
    //--
    FOnReadLine: TReadLineEvent;
    FOnFinish: TNotifyEvent;
  protected
    procedure ReadLine(ALine: String); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Execute(ACommandLine: String);
    procedure Stop;
    //==
    property Process: TProcess read FProcess;
    property Output: TStringList read FOutput;
    property OnReadLine: TReadLineEvent read FOnReadLine write FOnReadLine;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

implementation

{ TExecutor }

constructor TExecutor.Create;
begin
  FOutput := TStringList.Create();
  FProcess := TProcess.Create(nil);
  FProcess.Options:= [poUsePipes, poStdErrToOutput];
  FProcess.ShowWindow := swoHide;
  inherited;
end;

destructor TExecutor.Destroy;
begin
  FProcess.Free;
  FOutput.Free;
  inherited;
end;

procedure TExecutor.ReadLine(ALine: String);
begin
  FOutput.Add(ALine);
  if Assigned(FOnReadLine) then
    FOnReadLine(Self, ALine);
end;

// Portions of this code are from Lazarus project.
procedure TExecutor.Execute(ACommandLine: String);
const
  BufSize = 1000;
var
  I, Count, LineStart : longint;
  OutputLine, Buf : String;
begin
  FOutput.Clear;
  FForceStop := False;

  FProcess.CommandLine := ACommandLine;
  FProcess.Execute;

  SetLength(Buf, BufSize);
  OutputLine := '';
  repeat
    Application.ProcessMessages;
    //--
    if FForceStop then
    begin
      FProcess.Terminate(0);
      Break;
    end;

    if Assigned(FProcess.Output) then
      Count := FProcess.Output.Read(Buf[1], Length(Buf))
    else
      Count := 0;

    LineStart := 1;
    i := 1;
    while i <= Count do
    begin
      if Buf[i] in [#10,#13] then
      begin
        OutputLine := OutputLine + Copy(Buf, LineStart, i-LineStart);
        ReadLine(OutputLine);
        OutputLine := '';
        if (i < Count) and (Buf[i+1] in [#10,#13]) and (Buf[i] <> Buf[i+1]) then
          Inc(i);
        LineStart := i+1;
      end;
      Inc(i);
    end;
    OutputLine := OutputLine + Copy(Buf, LineStart, Count-LineStart+1);
  until Count = 0;

  FProcess.WaitOnExit;
  if Assigned(FOnFinish) then
    FOnFinish(Self);
end;

procedure TExecutor.Stop;
begin
  FForceStop := True;
end;

end.
