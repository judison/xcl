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

  TSeparator = class(TControl)
  private
  protected
    procedure SetParent(ANewParent: TControl); override;
    function IsBoxExpandStored: Boolean; override;
  public
  published
  end;

  THSeparator = class(TSeparator)
  private
  protected
    procedure CreateHandle; override;
  public
  published
  end;

  TVSeparator = class(TSeparator)
  private
  protected
    procedure CreateHandle; override;
  public
  published
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TSeparator }

function TSeparator.IsBoxExpandStored: Boolean;
begin
  Result := (Parent is TBox) and (BoxExpand = True);
end;

procedure TSeparator.SetParent(ANewParent: TControl);
begin
  inherited;
  if Parent is TBox then
    BoxExpand := False;
end;

{ THSeparator }

procedure THSeparator.CreateHandle;
begin
  Handle := gtk_hseparator_new;
end;

{ TVSeparator }

procedure TVSeparator.CreateHandle;
begin
  Handle := gtk_vseparator_new;
end;

{$ENDIF}
