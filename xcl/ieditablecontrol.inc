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

  private
    function GetEditable: Boolean;
    function GetPosition: Integer;
    procedure SetEditable(AValue: Boolean);
    procedure SetPosition(AValue: Integer);
  public
    procedure CutClipboard;
    procedure CopyClipboard;
    procedure PasteClipboard;
    procedure DeleteSelection;
    procedure InsertText(AText: string; APos: Integer);
    procedure DeleteText(AStartPos, AEndPos: Integer);
    procedure GetSelection(var AStartPos, AEndPos: Integer);
    procedure SetSelection(AStartPos, AEndPos: Integer);
  published
    property Editable: Boolean read GetEditable write SetEditable default True;
    property Position: Integer read GetPosition write SetPosition;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

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
