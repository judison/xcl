(*
   XCL - XDE's Component Library
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   DB Aware Controls

   See the file COPYING.XCL, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit xcldb;

{$H+}
{$MODE ObjFpc}
{$INLINE ON}

interface

uses Classes, Sysutils, xcl, db;

type
{$DEFINE INC_READ_INTERFACE}
  {$I xcldb.inc}
{$UNDEF INC_READ_INTERFACE}

procedure Register;

implementation

uses glib2, atk, pango, gdk2, gdk2pixbuf, gtk2;

{$DEFINE INC_READ_IMPLEMENTATION}
  {$I xcldb.inc}
{$UNDEF INC_READ_MPLEMENTATION}

procedure Register;
begin
  RegisterComponents('Data Controls', [TDBEntry, TDBLabel]);
end;

initialization
//---------------------------------  Registrar as Classes
//-------------------- Nao Visuais
  RegisterClass(TDataSetFirst);
  RegisterClass(TDataSetLast);
  RegisterClass(TDataSetNext);
  RegisterClass(TDataSetPrior);
  RegisterClass(TDataSetRefresh);
  RegisterClass(TDataSetCancel);
  RegisterClass(TDataSetDelete);
  RegisterClass(TDataSetEdit);
  RegisterClass(TDataSetInsert);
  RegisterClass(TDataSetPost);
//-------------------- Visuais
  RegisterClass(TDBEntry);
  RegisterClass(TDBLabel);
//---------------------------------
finalization
end.
