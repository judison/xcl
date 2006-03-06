(*
   $Id: xde.pas,v 1.7 2006/03/04 16:46:07 judison Exp $

   XDE - XCL's Development Environment
   Copyright (C) 2005 Judison Oliveira Gil Filho

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
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
{
$Log: xde.pas,v $
Revision 1.7  2006/03/04 16:46:07  judison
Changes to compile on Win32

Revision 1.6  2006/02/28 16:26:29  judison
* New Resource System
* Better Editing Functions

Revision 1.5  2005/12/22 23:57:01  judison
*** empty log message ***

Revision 1.4  2005/12/18 06:59:58  judison
Added ComponentPalette and Icons

Revision 1.3  2005/12/18 05:05:59  judison
copyleft stuff

Revision 1.2  2005/12/17 21:48:30  judison
*** empty log message ***

Revision 1.1.1.1  2005/12/17 17:29:47  judison
Initial Import

}