(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit FormatRead;

{$H+}
{$MODE ObjFpc}

interface

function FmtRead(Format: String; Str: String; Args: array of Pointer): Boolean;

implementation

uses SysUtils;

function ReadStr(var S: PChar; EndChar: Char; R: Pointer): Boolean; inline;
var
  Res: String;
begin
  Res := '';
  while (S^ <> #0) and (S^ <> EndChar) do
  begin
    Res := Res + S^;
    Inc(S);
  end;
  PString(R)^ := Res;
  Result := (S^ = EndChar);
end;

function ReadDec(var S: PChar; EndChar: Char; R: Pointer): Boolean; inline;
var
  Res: String;
begin
  Res := '';
  while (S^ <> #0) and (S^ <> EndChar) and (S^ in ['0'..'9']) do
  begin
    Res := Res + S^;
    Inc(S);
  end;
  PInteger(R)^ := StrToIntDef(Res, 0);
  Result := (S^ = EndChar);
end;

function ReadHex(var S: PChar; EndChar: Char; R: Pointer): Boolean; inline;
var
  Res: String;
begin
  Res := '';
  while (S^ <> #0) and (S^ <> EndChar) and (S^ in ['0'..'9','a'..'f','A'..'F']) do
  begin
    Res := Res + S^;
    Inc(S);
  end;
  PString(R)^ := Res;
  Result := (S^ = EndChar);
end;

function ReadEnd(var S: PChar; R: Pointer): Boolean; inline;
var
  Res: String;
begin
  Res := '';
  while (S^ <> #0) do
  begin
    Res := Res + S^;
    Inc(S);
  end;
  PString(R)^ := Res;
  Result := True;
end;

function FmtRead(Format: String; Str: String; Args: array of Pointer): Boolean;
var
  F, S: PChar;
  CurArg: Integer;
begin
  SetLength(Format, Length(Format)+1);
  Format[Length(Format)] := #0;

  SetLength(Str, Length(Str)+1);
  Format[Length(Str)] := #0;

  F := @Format[1];
  S := @Str[1];

  Result := True;
  CurArg := Low(Args) - 1;

  while (F^ <> #0) and (S^ <> #0) and Result do
    if (F^ = '%') then
    begin
      Inc(F);
      Inc(CurArg);
      case F^ of
        's': Result := Result and ReadStr(S, (F+1)^, Args[CurArg]);
        'd': Result := Result and ReadDec(S, (F+1)^, Args[CurArg]);
        'h': Result := Result and ReadHex(S, (F+1)^, Args[CurArg]);
        // Special case, read to the end
        #0: Result := Result and ReadEnd(S, Args[CurArg]);
      else
        Dec(CurArg);
      end;
      if F^ <> #0 then
        Inc(F);
    end
    else if (F^ = S^) then
    begin
      Inc(F);
      Inc(S);
    end
    else
      Result := False;
  Result := Result and (F^ = #0) and (S^ = #0);
end;

end.