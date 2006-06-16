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
{$IFDEF WIN32}
  {APPTYPE GUI}
{$ENDIF}

uses classes,
     xcl,
     //--
     exctrls,
     buffer,
     bufferlist,
     txtbuffer,
     pasbuffer,
     frmbuffer,
     compilermsg,
     executor,
     compiler,
     frm_compiling,
     frm_projectopts,
     frm_compileropts,
     frm_editoropts,
     frm_newfile,
     frm_editfind,
     formatread,
     dbg,
     jitform,
     jitforms,
     propeditor,
     componentpalette,
     regcomps,
     xpr,
     frm_main,
     //--
     xde_xrc,
     icons_xrc;

begin
  DoRegComps;
  Application.Initialize;
  Application.Title := 'XDE: XCL''s Development Environment';
  Application.CreateForm(TFrmMain, FrmMain);
  Application.CreateForm(TFrmEditFind, FrmEditFind);
  Application.Run; 
end.
