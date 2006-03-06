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

  TFixed = class(TContainerControl)
  private
  protected
    procedure CreateHandle; override;
    procedure InternalAddControl(AControl: TControl); override;
  public
    procedure MoveChild(AChild: TControl; AX, AY: Integer);
  published
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TFixed }

procedure TFixed.CreateHandle;
begin
  Handle := gtk_fixed_new;
end;

procedure TFixed.InternalAddControl(AControl: TControl);
begin
  gtk_fixed_put(Handle, AControl.Handle, AControl.FFixedX, AControl.FFixedY);
end;

procedure TFixed.MoveChild(AChild: TControl; AX, AY: Integer);
begin
  gtk_fixed_move(Handle, AChild.Handle, AX, AY);
  AChild.FFixedX := AX;
  AChild.FFixedY := AY;
end;

{$ENDIF}
