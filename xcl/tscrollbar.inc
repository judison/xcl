(*
   $Id: tscrollbar.inc,v 1.2 2005/03/26 05:21:47 judison Exp $

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

  TScrollBar = class(TRange)
  private
  protected
  public
  published
  end;

  THScrollBar = class(TScrollBar)
  private
  protected
    procedure CreateHandle; override;
  public
  published
  end;

  TVScrollBar = class(TScrollBar)
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

{ TScrollBar }

{ THScrollBar }

procedure THScrollBar.CreateHandle;
begin
  CreateMyAdj;
  Handle := gtk_hscrollbar_new(FAdjustment.Handle);
end;

{ TVScrollBar }

procedure TVScrollBar.CreateHandle;
begin
  CreateMyAdj;
  Handle := gtk_vscrollbar_new(FAdjustment.Handle);
end;

{$ENDIF}
{
$Log: tscrollbar.inc,v $
Revision 1.2  2005/03/26 05:21:47  judison
+ CVS Log Tag

}