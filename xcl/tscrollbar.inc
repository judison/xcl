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

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

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
