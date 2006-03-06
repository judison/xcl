(*
   $Id: util_quark.inc,v 1.1 2005/03/27 09:53:06 judison Exp $

   XCL - XDE's Component Library
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
{$IFDEF INC_READ_INTERFACE}
//** INTERFACE
//****************************************************************************

function QuarkByName(AName: String): TQuark;
function TryQuarkByName(AName: String): TQuark;
function QuarkName(AQuark: TQuark): String;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

function QuarkByName(AName: String): TQuark;
begin
  Result := g_quark_from_string(PChar(AName));
end;

function TryQuarkByName(AName: String): TQuark;
begin
  Result := g_quark_try_string(PChar(AName));
end;

function QuarkName(AQuark: TQuark): String;
begin
  Result := g_quark_to_string(AQuark);
end;

{$ENDIF}
{
  $Log: util_quark.inc,v $
  Revision 1.1  2005/03/27 09:53:06  judison
  + First Version

}