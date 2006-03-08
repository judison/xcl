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

  protected
    procedure CellLayoutPackStart(ARenderer: Pointer; AExpand: Boolean); inline;
    procedure CellLayoutPackEnd(ARenderer: Pointer; AExpand: Boolean); inline;
    procedure CellLayoutReorder(ARenderer: Pointer; APosition: Integer); inline;
    procedure CellLayoutClear(); inline;
    //procedure CellLayoutSetAttributes(ARenderer: Pointer; const Args: array of const); inline;
    procedure CellLayoutAddAttribute(ARenderer: Pointer; AAttribute: PChar; AColumn: Integer); inline;
    procedure CellLayoutClearAttributes(ARenderer: Pointer); inline;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

procedure the_class.CellLayoutPackStart(ARenderer: Pointer; AExpand: Boolean); inline;
begin
  gtk_cell_layout_pack_start(Handle, ARenderer, AExpand);
end;

procedure the_class.CellLayoutPackEnd(ARenderer: Pointer; AExpand: Boolean); inline;
begin
  gtk_cell_layout_pack_end(Handle, ARenderer, AExpand);
end;

procedure the_class.CellLayoutReorder(ARenderer: Pointer; APosition: Integer); inline;
begin
  gtk_cell_layout_reorder(Handle, ARenderer, APosition);
end;

procedure the_class.CellLayoutClear(); inline;
begin
  gtk_cell_layout_clear(Handle);
end;
{
procedure the_class.CellLayoutSetAttributes(ARenderer: Pointer; const Args: array of const); inline;
begin
  gtk_cell_layout_set_attributes(Handle, ARenderer, Args);
end;
}
procedure the_class.CellLayoutAddAttribute(ARenderer: Pointer; AAttribute: PChar; AColumn: Integer); inline;
begin
  gtk_cell_layout_add_attribute(Handle, ARenderer, AAttribute, AColumn);
end;

procedure the_class.CellLayoutClearAttributes(ARenderer: Pointer); inline;
begin
  gtk_cell_layout_clear_attributes(Handle, ARenderer);
end;

{$ENDIF}
