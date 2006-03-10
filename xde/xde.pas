(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

program xde;

{$H+}
{$MODE ObjFpc}

uses classes,
     xcl,
     main,
     compiler_opts,
     editor_opts,
     frm_newfile,
     jitforms,
     componentpalette,
     regcomps,
     xde_xrc,
     icons_xrc;

begin
  DoRegComps;
  Application.Initialize;
  Application.Title := 'XDE: XCL''s Development Environment';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
