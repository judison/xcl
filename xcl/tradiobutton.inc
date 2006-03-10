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

  TRadioButton = class(TCheckButton)
  private
  protected
    procedure CreateHandle; override;
    procedure SetParent(ANewParent: TControl); override;
  public
  published
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TRadioButton }

procedure TRadioButton.CreateHandle;
begin
  Handle := gtk_radio_button_new(nil);
end;

procedure TRadioButton.SetParent(ANewParent: TControl);
var
  I: Integer;
begin
  inherited;

  if Assigned(Parent) then
    for I := 0 to Parent.ControlCount -1 do
      if (Parent.Controls[I] is TRadioButton) and (Parent.Controls[I] <> Self) then
      begin
        gtk_radio_button_set_group(Handle, gtk_radio_button_get_group(Parent.Controls[I].Handle));
        exit;
      end;
  // if reach here... (no parent, or no TRadioButton on Parent)
  gtk_radio_button_set_group(Handle, nil)
end;

{$ENDIF}
