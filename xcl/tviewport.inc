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

  TViewport = class(TBinControl)
  private
    function GetShadowType: TShadowType;
    procedure SetShadowType(AValue: TShadowType);
  protected
    function IsResizeModeStored: Boolean; override;
    procedure CreateHandle; override;
    procedure SetParent(ANewParent: TControl); override;
  public
  published
    property ShadowType: TShadowType read GetShadowType write SetShadowType default stIn;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TViewport }

procedure TViewport.CreateHandle;
begin
  Handle := gtk_viewport_new(nil, nil);
end;

procedure TViewport.SetParent(ANewParent: TControl);
begin
  inherited;
  if Parent is TScrolledWindow then
  begin
    gtk_viewport_set_hadjustment(Handle, TScrolledWindow(Parent).HAdjustment.Handle);
    gtk_viewport_set_vadjustment(Handle, TScrolledWindow(Parent).VAdjustment.Handle);
  end;
end;

function TViewport.GetShadowType: TShadowType;
begin
  Result := TShadowType(gtk_viewport_get_shadow_type(Handle));
end;

procedure TViewport.SetShadowType(AValue: TShadowType);
begin
  gtk_viewport_set_shadow_type(Handle, Ord(AValue));
end;

function TViewport.IsResizeModeStored: Boolean;
begin
  Result := ResizeMode <> rmQueue;
end;

{$ENDIF}
