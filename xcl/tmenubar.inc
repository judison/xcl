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

  TMenuBar = class(TContainerControl)
  private
  protected
    procedure CreateHandle; override;
    procedure SetParent(ANewParent: TControl); override;
    function IsBoxExpandStored: Boolean; override;
  public
  published
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TMenuBar }

procedure TMenuBar.CreateHandle;
begin
  Handle := gtk_menu_bar_new;
end;

function TMenuBar.IsBoxExpandStored: Boolean;
begin
  Result := (Parent is TBox) and (BoxExpand = True);
end;

procedure TMenuBar.SetParent(ANewParent: TControl);
begin
  inherited;
  if Parent is TBox then
    BoxExpand := False;
end;

{$ENDIF}
