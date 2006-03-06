(*
   $Id: tbutton.inc,v 1.6 2005/11/18 20:06:58 judison Exp $

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

  TButton = class(TCustomButton)
  private
  protected
  public
  published
    property Caption;
    property UseStock;
    property UseUnderline;
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{$ENDIF}
{
  $Log: tbutton.inc,v $
  Revision 1.6  2005/11/18 20:06:58  judison
  long-term changes...

  Revision 1.5  2005/03/27 09:53:51  judison
  * New Event System

  Revision 1.4  2005/03/26 05:45:48  judison
  + TButton.Relief

  Revision 1.3  2005/03/26 05:21:47  judison
  + CVS Log Tag
}