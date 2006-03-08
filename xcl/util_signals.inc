(*
   XCL - XDE's Component Library
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
{** INTERFACE *************************************}{$IFDEF INC_READ_INTERFACE}

// TODO: XCL: messagess and <User>: messages
// GTK: signals are messages :)

procedure ConnectGtkSignal(Handle: Pointer; AName: PChar);
procedure DisconnectGtkSignal(Handle: Pointer; AName: PChar);

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

procedure ConnectGtkSignal(Handle: Pointer; AName: PChar);
var
  P: Pointer;
begin
  P := GtkSignalHandler(AName);
  if P <> nil then
    g_signal_connect(Handle, AName, TGCallBack(P), AName)
  else
    raise Exception.Create('There''s no Handler for this GTK Signal.');
end;

procedure DisconnectGtkSignal(Handle: Pointer; AName: PChar);
var
  P: Pointer;
begin
  P := GtkSignalHandler(AName);
  if P <> nil then
    g_signal_handlers_disconnect_by_func(Handle, TGCallBack(P), AName);
end;

{$ENDIF}
