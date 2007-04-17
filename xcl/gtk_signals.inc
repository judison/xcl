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

  TGtkSig = record
    Signal: ShortString;
    P1, P2: Pointer;
    I1, I2: Integer;
  end;

  TGtkSigEvent = record
    Signal: ShortString;
    Event: Pointer;
    Stop: Boolean;
  end;

  TXCLMsg = record
    Name: ShortString;
    UID: LongWord;
    Int: array[0..3] of Integer;
    Ptr: array[0..3] of Pointer;
    Str: array[0..3] of String;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

var
  XCLMsgs: TList;
  XCLMsgLastID: LongWord = 0;

type
  PXCLMsg = ^TXCLMsg;

function AddXCLMsg(const AMsg: TXCLMsg): LongWord;
var
  M: PXCLMsg;
begin
  Inc(XCLMsgLastID);
  New(M);
  M^ := AMsg;
  M^.UID := XCLMsgLastID;
  XCLMsgs.Add(M);
  Result := XCLMsgLastID;
end;

procedure GetXCLMsg(AUID: LongWord; var AMsg: TXCLMsg);
var
  I: Integer;
  M: PXCLMsg;
begin
  AMsg.UID := 0;
  for I := 0 to XCLMsgs.Count -1 do
    if PXCLMsg(XCLMsgs[I])^.UID = AUID then
    begin
      M := PXCLMsg(XCLMsgs[I]);
      AMsg := M^;
      Dispose(M);
      XCLMsgs.Delete(I);
      exit;
    end;
end;

//===================================

function Hnd_GTK_GenericEvent(Handle: Pointer; Event: Pointer; Signal: PChar): gboolean; cdecl;
var
  Msg: TGtkSigEvent;
  O: TObject;
begin
  Msg.Signal := 'GTK:'+Signal;
  Msg.Event := Event;
  Msg.Stop := False;
  O := ObjectByHandle(Handle);
  if O <> nil then
    try
      O.DispatchStr(Msg);
    except
      on E: Exception do
        Application.ShowException(E);
    end;
  Result := Msg.Stop;
end;

function Hnd_GTK_ClientEvent(Handle: Pointer; Event: PGdkEventClient; Signal: PChar): gboolean; cdecl;
var
  UID: LongWord;
  Msg: TXCLMsg;
  O: TObject;
begin
  //TODO: save this _XCL_MESSAGE in var XCL_MESSAGE: TGdkAtom
  if Event^.message_type = gdk_atom_intern('_XCL_MESSAGE', False) then
  begin
    UID := Event^.data.L[0];
    GetXCLMsg(UID, Msg);
    if Msg.UID = UID then
    begin
      O := ObjectByHandle(Handle);
      if O <> nil then
        try
          O.DispatchStr(Msg);
        except
          on E: Exception do
            Application.ShowException(E);
        end;
    end;
    Result := True;  // Sure?
  end
  else
    Result := Hnd_GTK_GenericEvent(Handle, Event, Signal);
end;

procedure Dispatch_GtkSig(Handle: Pointer; Signal: ShortString; P1, P2: Pointer; I1, I2: Integer);
var
  Msg: TGtkSig;
  O: TObject;
begin
  Msg.Signal := 'GTK:'+Signal;
  Msg.P1 := P1;
  Msg.P2 := P2;
  Msg.I1 := I1;
  Msg.I2 := I2;
  O := ObjectByHandle(Handle);
  if O <> nil then
    try
      O.DispatchStr(Msg);
    except
      on E: Exception do
        Application.ShowException(E);
    end
  else WriteLn('Aha');
end;

procedure Hnd_GTK_Generic(Handle: Pointer; Signal: PChar); cdecl;
begin
  Dispatch_GtkSig(Handle, Signal, nil, nil, 0, 0);
end;

procedure Hnd_GtkNotebook_Switch_Page(Handle: Pointer; page: pointer; page_num: guint; Signal: PChar); cdecl;
begin
  Dispatch_GtkSig(Handle, Signal, page, nil, page_num, 0);
end;

procedure Hnd_GtkTreeView_Row_Activated(Handle: Pointer; path: pointer; column: pointer; Signal: PChar); cdecl;
begin
  Dispatch_GtkSig(Handle, Signal, path, column, 0, 0);
end;

type
  TSignalHandler = record
    Signal: ShortString;
    Handler: Pointer;
  end;

const
  GTKSignalHandlers_Count = 47;
  GTKSignalHandlers: array[0..GTKSignalHandlers_Count-1] of TSignalHandler = (
    //** GtkWidget Events ****************************************************
    (Signal: 'button-press-event';          Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'button-release-event';        Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'client-event';                Handler: @Hnd_GTK_ClientEvent),
    (Signal: 'configure-event';             Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'delete-event';                Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'destroy-event';               Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'enter-notify-event';          Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'event';                       Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'event-after';                 Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'expose-after';                Handler: @Hnd_GTK_GenericEvent),
    (Signal: 'expose-event';                Handler: @Hnd_GTK_GenericEvent),
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
    (Signal: 'changed';                     Handler: @Hnd_GTK_Generic),
    //** GtkNotebook *********************************************************
    (Signal: 'switch-page';                 Handler: @Hnd_GtkNotebook_Switch_Page),
    //** GtkTreeView *********************************************************
    (Signal: 'row-activated';               Handler: @Hnd_GtkTreeView_Row_Activated)
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
