(*
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
