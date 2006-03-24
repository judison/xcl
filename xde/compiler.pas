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

uses Classes, SysUtils, xcl, Process, xpr;

type
  TOnCmdLineCreate = procedure(var CmdLine: String; var Abort: Boolean) of object;
  
  TCompilerResult = (crOK, crCancel, crAbort);

  TCompiler = class(TObject)
  private
    procedure
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(AProject: TProject; BuildAll: Boolean; const DefaultFilename: String): TCompilerResult;
  end;

implementation

{ TCompiler }

constructor TCompiler.Create;
begin
  inherited;
end;

destructor TCompiler.Destroy;
begin
  inherited;
end;

function TCompiler.Compile(AProject: TXPRProject; BuildAll: boolean; const DefaultFilename: string): Integer;
var
  CmdLine: String;
  Abort: Boolean;
  OldCurDir, ProjectDir, ProjectFilename: String;
  TheProcess : TProcess;
begin
  Result := crCancel;

  OldCurDir := GetCurrentDir;

  ProjectFilename := AProject.ProjectFile;
  ProjectDir := ExtractFilePath(ProjectFilename);

  if not SetCurrentDir(ProjectDir) then
    exit;
  try
    CmdLine := 'fpc';//AProject.CompilerOptions.CompilerPath;
    
    if BuildAll then
      CmdLine := CmdLine + ' -B';

    //CmdLine := CmdLine+ ' '+ AProject.CompilerOptions.MakeOptionsString(ProjectFilename,nil,[])+' '+ PrepareCmdLineOption(ProjectFilename);

    TheProcess := TProcess.Create(nil);
    TheProcess.CommandLine := CmdLine;
    TheProcess.Options:= [poUsePipes, poStdErrToOutput];
    TheProcess.ShowWindow := swoHide;

    Result := crOk;

    try
      TheProcess.CurrentDirectory := ProjectDir;

      TheProcess.Execute;
    finally
      TheProcess.WaitOnExit;
      TheProcess.Free;
    end;
  finally
    SetCurrentDir(OldCurDir);
  end;
end;

end.