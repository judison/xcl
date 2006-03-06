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

  TGtkSig = record
    Signal: ShortString;
  end;

  TGtkSigEvent = record
    Signal: ShortString;
    Event: Pointer;
    Stop: Boolean;
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

procedure Hnd_GTK_Generic(Handle: Pointer; Signal: PChar); cdecl;
var
  Msg: TGtkSig;
begin
  Msg.Signal := 'GTK:'+Signal;
  try
    ControlByHandle(Handle).DispatchStr(Msg);
  except
    on E: Exception do
      Application.ShowException(E);
  end;
end;

function Hnd_GTK_GenericEvent(Handle: Pointer; Event: Pointer; Signal: PChar): gboolean; cdecl;
var
  Msg: TGtkSigEvent;
begin
  Msg.Signal := 'GTK:'+Signal;
  Msg.Event := Event;
  Msg.Stop := False;
  try
    ControlByHandle(Handle).DispatchStr(Msg);
  except
    on E: Exception do
      Application.ShowException(E);
  end;
  Result := Msg.Stop;
end;

type
  TSignalHandler = record
    Signal: ShortString;
    Handler: Pointer;
  end;

const
  GTKSignalHandlers_Count = 44;
  GTKSignalHandlers: array[0..GTKSignalHandlers_Count-1] of TSignalHandler = (
    //** GtkWidget Events ****************************************************
    (Signal: 'button-press-event';          Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'button-release-event';        Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'client-event';                Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'configure-event';             Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'delete-event';                Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'destroy-event';               Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'enter-notify-event';          Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'event';                       Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'event-after';                 Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'expose-after';                Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'focus-in-event';              Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'focus-out-event';             Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'key-press-event';             Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'key-release-event';           Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'leave-notify-event';          Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'map-event';                   Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'motion-notify-event';         Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'no-expose-event';             Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'property-notify-event';       Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'proximity-in-event';          Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'proximity-out-event';         Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'scroll-event';                Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'selection-clear-event';       Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'selection-notify-event';      Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'selection-request-event';     Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'unmap-event';                 Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'visibility-notify-event';     Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'window-state-event';          Handler: @Hnd_GTK_GenericEvent),
    //** GtkWidget ***********************************************************
    (Signal: 'accel-closures-changed';      Handler: @Hnd_GTK_Generic),
  //(Signal: 'can-activate-accel';          Handler: @Hnd_GTK_),
  //(Signal: 'child-notify';                Handler: @Hnd_GTK_),
  //(Signal: 'direction-changed';           Handler: @Hnd_GTK_),
  //(Signal: 'drag-begin';                  Handler: @Hnd_GTK_),
  //(Signal: 'drag-data-delete';            Handler: @Hnd_GTK_),
  //(Signal: 'drag-data-get';               Handler: @Hnd_GTK_),
  //(Signal: 'drag-data-received';          Handler: @Hnd_GTK_),
  //(Signal: 'drag-drop';                   Handler: @Hnd_GTK_),
  //(Signal: 'drag-end';                    Handler: @Hnd_GTK_),
  //(Signal: 'drag-leave';                  Handler: @Hnd_GTK_),
  //(Signal: 'drag-motion';                 Handler: @Hnd_GTK_),
  //(Signal: 'focus';                       Handler: @Hnd_GTK_),
    (Signal: 'grab-focus';                  Handler: @Hnd_GTK_Generic),
  //(Signal: 'grab-notify';                 Handler: @Hnd_GTK_),
    (Signal: 'hide';                        Handler: @Hnd_GTK_Generic),
  //(Signal: 'hierarchy-changed';           Handler: @Hnd_GTK_),
    (Signal: 'map';                         Handler: @Hnd_GTK_Generic),
  //(Signal: 'mnemonic-activate';           Handler: @Hnd_GTK_),
  //(Signal: 'parent-set';                  Handler: @Hnd_GTK_),
    (Signal: 'popup-menu';                  Handler: @Hnd_GTK_Generic),
    (Signal: 'realize';                     Handler: @Hnd_GTK_Generic),
  //(Signal: 'screen-changed';              Handler: @Hnd_GTK_),
  //(Signal: 'selection-get';               Handler: @Hnd_GTK_),
  //(Signal: 'selection-received';          Handler: @Hnd_GTK_),
    (Signal: 'show';                        Handler: @Hnd_GTK_Generic),
  //(Signal: 'show-help';                   Handler: @Hnd_GTK_),
  //(Signal: 'size-allocate';               Handler: @Hnd_GTK_),
  //(Signal: 'size-request';                Handler: @Hnd_GTK_),
  //(Signal: 'state-changed';               Handler: @Hnd_GTK_),
  //(Signal: 'style-set';                   Handler: @Hnd_GTK_),
    (Signal: 'unmap';                       Handler: @Hnd_GTK_Generic),
    (Signal: 'unrealize';                   Handler: @Hnd_GTK_Generic),
    //** GtkButton ***********************************************************
    (Signal: 'clicked';                     Handler: @Hnd_GTK_Generic),
    //** GtkToggleButton *****************************************************
    (Signal: 'toggled';                     Handler: @Hnd_GTK_Generic),
    //** GtkCalendar *********************************************************
    (Signal: 'day-selected';                Handler: @Hnd_GTK_Generic),
    (Signal: 'day-selected-double-click';   Handler: @Hnd_GTK_Generic),
    (Signal: 'month-changed';               Handler: @Hnd_GTK_Generic),
    //** GtkMenuItem *********************************************************
    (Signal: 'activate';                    Handler: @Hnd_GTK_Generic),
    //** GtkEditable *********************************************************
    (Signal: 'changed';                     Handler: @Hnd_GTK_Generic)
  );

function GtkSignalHandler(S: ShortString): Pointer;
var
  I: Integer;
begin
  for I := 0 to GTKSignalHandlers_Count-1 do
    if GTKSignalHandlers[I].Signal = S then
      exit(GTKSignalHandlers[I].Handler);
  Result := nil;
end;

{$ENDIF}