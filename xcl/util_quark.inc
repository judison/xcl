(*
   XCL - XDE's Component Library
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING.XCL, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
{** INTERFACE *************************************}{$IFDEF INC_READ_INTERFACE}

function QuarkByName(AName: String): TQuark;
function TryQuarkByName(AName: String): TQuark;
function QuarkName(AQuark: TQuark): String;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

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
