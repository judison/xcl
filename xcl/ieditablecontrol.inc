(*
   $Id: ieditablecontrol.inc,v 1.1 2005/03/28 05:38:23 judison Exp $

   XCL - XDE's Component Library
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

//** INTERFACE
//****************************************************************************

{$IFDEF INTF_PRIVATE}
    function GetEditable: Boolean;
    function GetPosition: Integer;
    procedure SetEditable(AValue: Boolean);
    procedure SetPosition(AValue: Integer);
{$ENDIF}
{$IFDEF INTF_PROTECTED}

{$ENDIF}
{$IFDEF INTF_PUBLIC}
    procedure CutClipboard;
    procedure CopyClipboard;
    procedure PasteClipboard;
    procedure DeleteSelection;
    procedure InsertText(AText: string; APos: Integer);
    procedure DeleteText(AStartPos, AEndPos: Integer);
    procedure GetSelection(var AStartPos, AEndPos: Integer);
    procedure SetSelection(AStartPos, AEndPos: Integer);
{$ENDIF}
{$IFDEF INTF_PUBLISHED}
    property Editable: Boolean read GetEditable write SetEditable default True;
    property Position: Integer read GetPosition write SetPosition;
{$ENDIF}


//** IMPLEMENTATION
//****************************************************************************


{$IFDEF INTF_IMPLEMENTATION}

function the_class.GetEditable: Boolean;
begin
  Result := gtk_editable_get_editable(Handle);
end;

function the_class.GetPosition: Integer;
begin
  Result := gtk_editable_get_position(Handle);
end;

procedure the_class.SetEditable(AValue: Boolean);
begin
  gtk_editable_set_editable(Handle, AValue);
end;

procedure the_class.SetPosition(AValue: Integer);
begin
  gtk_editable_set_position(Handle, AValue);
end;

procedure the_class.CutClipboard;
begin
  gtk_editable_cut_clipboard(Handle);
end;

procedure the_class.CopyClipboard;
begin
  gtk_editable_copy_clipboard(Handle);
end;

procedure the_class.PasteClipboard;
begin
  gtk_editable_paste_clipboard(Handle);
end;

procedure the_class.DeleteSelection;
begin
  gtk_editable_delete_selection(Handle);
end;

procedure the_class.InsertText(AText: string; APos: Integer);
var
  i: integer;
begin
  i := APos;
  gtk_editable_insert_text(Handle, PChar(AText), Length(AText), @i);
end;

procedure the_class.DeleteText(AStartPos, AEndPos: Integer);
begin
  gtk_editable_delete_text(Handle, AStartPos, AEndPos);
end;

procedure the_class.GetSelection(var AStartPos, AEndPos: Integer);
begin
  gtk_editable_get_selection_bounds(Handle, @AStartPos, @AEndPos);
end;

procedure the_class.SetSelection(AStartPos, AEndPos: Integer);
begin
  gtk_editable_select_region(Handle, AStartPos, AEndPos);
end;


{$ENDIF}
{
  $Log: ieditablecontrol.inc,v $
  Revision 1.1  2005/03/28 05:38:23  judison
  - TEditableControl
  + Interface Implementation for GtkEditableControl

  Revision 1.3  2005/03/27 09:53:51  judison
  * New Event System

  Revision 1.2  2005/03/26 05:21:47  judison
  + CVS Log Tag

}