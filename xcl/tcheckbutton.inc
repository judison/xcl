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

  TCheckButton = class(TCustomToggleButton)
  private
  protected
    procedure CreateHandle; override;
    procedure CreateControls; override;
  public
  published
    property Caption;
    property IconName;
    property UseUnderline;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TCheckButton }

procedure TCheckButton.CreateHandle;
begin
  Handle := gtk_check_button_new;
end;

procedure TCheckButton.CreateControls;
begin
  inherited;
  if Assigned(FImage) then
    FImage.IconSize := iszMenu;
end;

{$ENDIF}
