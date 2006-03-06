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

  TSeparatorMenuItem = class(TCustomMenuItem)
  private
  protected
    procedure CreateHandle; override;
  public
  published
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TSeparatorMenuItem }

procedure TSeparatorMenuItem.CreateHandle;
begin
  Handle := gtk_separator_menu_item_new;
  inherited;
end;

{$ENDIF}
