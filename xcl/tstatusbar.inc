(*
   $Id: tstatusbar.inc,v 1.2 2005/03/26 05:21:47 judison Exp $

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

  TStatusBar = class(TControl)
  private
    function GetHasResizeGrip: Boolean;
    procedure SetHasResizeGrip(Value: Boolean);
  protected
    procedure CreateHandle; override;
  public
    function GetContextId(AContextDescription: String): Cardinal;
    function Push(AContextId: Cardinal; AText: String): Cardinal;
    procedure Pop(AContextId: Cardinal);
    procedure Remove(AContextId: Cardinal; AMessageId: Cardinal);
  published
    property HasResizeGrip: Boolean read GetHasResizeGrip write SetHasResizeGrip default True;
    // Falta ShadowType
    // Falta eventos text-popped e text-pushed
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TStatusBar }

procedure TStatusBar.CreateHandle;
begin
  Handle := gtk_statusbar_new;
end;

function TStatusBar.GetContextId(AContextDescription: String): Cardinal;
begin
  Result := gtk_statusbar_get_context_id(Handle, PChar(AContextDescription));
end;

function TStatusBar.Push(AContextId: Cardinal; AText: String): Cardinal;
begin
  Result := gtk_statusbar_push(Handle, AContextId, PChar(AText));
end;

procedure TStatusBar.Pop(AContextId: Cardinal);
begin
  gtk_statusbar_pop(Handle, AContextId);
end;

procedure TStatusBar.Remove(AContextId: Cardinal; AMessageId: Cardinal);
begin
  gtk_statusbar_remove(Handle, AContextId, AMessageId);
end;

function TStatusBar.GetHasResizeGrip: Boolean;
begin
  Result := gtk_statusbar_get_has_resize_grip(Handle);
end;

procedure TStatusBar.SetHasResizeGrip(Value: Boolean);
begin
  gtk_statusbar_set_has_resize_grip(Handle, Value);
end;

{$ENDIF}
{
$Log: tstatusbar.inc,v $
Revision 1.2  2005/03/26 05:21:47  judison
+ CVS Log Tag

}