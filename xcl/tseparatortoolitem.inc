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

  TSeparatorToolItem = class(TCustomToolItem)
  private
    function GetDraw: Boolean;
    procedure SetDraw(AValue: Boolean);
  protected
    procedure CreateHandle; override;
  public
  published
    property Draw: Boolean read GetDraw write SetDraw default True;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TSeparatorToolItem }

procedure TSeparatorToolItem.CreateHandle;
begin
  Handle := gtk_separator_tool_item_new;
end;

function TSeparatorToolItem.GetDraw: Boolean;
begin
  Result := gtk_separator_tool_item_get_draw(Handle);
end;

procedure TSeparatorToolItem.SetDraw(AValue: Boolean);
begin
  gtk_separator_tool_item_set_draw(Handle, AValue);
end;

{$ENDIF}
