(*
   $Id: tcheckbutton.inc,v 1.3 2005/12/16 03:29:52 judison Exp $

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

  TCheckButton = class(TCustomToggleButton)
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

{ TCheckButton }

procedure TCheckButton.CreateHandle;
begin
  Handle := gtk_check_button_new;
end;

{$ENDIF}
{
$Log: tcheckbutton.inc,v $
Revision 1.3  2005/12/16 03:29:52  judison
*** empty log message ***

Revision 1.2  2005/03/26 05:21:47  judison
+ CVS Log Tag

}