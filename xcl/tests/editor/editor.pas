(*
   $Id: editor.pas,v 1.6 2006/02/28 16:22:58 judison Exp $

   XCL - XDE's Component Library
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
program editor;

{$H+}
{$MODE ObjFpc}

uses Classes, SysUtils, xcl,
     main, editor_xrc;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
{
$Log: editor.pas,v $
Revision 1.6  2006/02/28 16:22:58  judison
New resource system

Revision 1.5  2005/12/17 17:43:41  judison
moved syntax.pas to tests/editor

Revision 1.4  2005/03/28 03:18:39  judison
New Resource System

Revision 1.3  2005/03/26 05:21:47  judison
+ CVS Log Tag

}