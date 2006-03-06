(*
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

procedure ColorToRGB(ADesc: TColor; var R, G, B: Word);
function RGBToColor(R, G, B: Word): TColor;
function GdkColorToColor(AGdkColor: Pointer): TColor;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

(*
TODO:

create more flexible functions..

make ColorToRGB understant:
  rgb:RR/GG/BB
  000,000,000 (decimal)

make RGBToColor returns "red" for "#FF0000" for example


*)

procedure ColorToRGB(ADesc: TColor; var R, G, B: Word);
var
  Data: TGdkColor;
begin
  gdk_color_parse(Pchar(ADesc), @Data);
  R := Data.red;
  G := Data.green;
  B := Data.blue;
end;

function RGBToColor(R, G, B: Word): TColor;
begin
  Result := Format('#%.4x%.4x%.4x', [R, G, B]);
end;

function GdkColorToColor(AGdkColor: Pointer): TColor;
begin
  Result := Format('#%.4x%.4x%.4x', [PGdkColor(AGdkColor)^.red, PGdkColor(AGdkColor)^.green, PGdkColor(AGdkColor)^.blue]);
end;

{$ENDIF}
