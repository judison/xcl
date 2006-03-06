(*
   $Id: dbtest.pas,v 1.3 2006/02/28 16:22:58 judison Exp $

   XCL - XDE's Component Library
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
program dbtest;

{$H+}
{$MODE ObjFpc}

uses Classes, SysUtils, xcl,
     main, dbtest_xrc;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
{
$Log: dbtest.pas,v $
Revision 1.3  2006/02/28 16:22:58  judison
New resource system

Revision 1.2  2005/11/22 01:08:44  judison
removed "useless uses"

Revision 1.1  2005/11/18 20:30:28  judison
*** empty log message ***

}