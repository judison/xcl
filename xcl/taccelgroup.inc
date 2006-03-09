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

  TAccelGroup = class(TComponent)
  private
  protected
    Handle: Pointer;
    procedure CreateHandle; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetHandle: Pointer;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TAccelGroup }

constructor TAccelGroup.Create(AOwner: TComponent);
begin
  inherited;
  CreateHandle;
  g_object_ref(Handle);
end;

destructor TAccelGroup.Destroy;
begin
  g_object_unref(Handle);
  inherited;
end;

procedure TAccelGroup.CreateHandle;
begin
  Handle := gtk_accel_group_new();
end;

function TAccelGroup.GetHandle: Pointer;
begin
  Result := Handle;
end;

{$ENDIF}
