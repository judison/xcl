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

  THandleBox = class(TBinControl)
  private
    function GetShadowType: TShadowType;
    procedure SetShadowType(AValue: TShadowType);
    function GetHandlePosition: TPosition;
    procedure SetHandlePosition(AValue: TPosition);
  protected
    procedure CreateHandle; override;
    {--}
  public
  published
    property HandlePosition: TPosition read GetHandlePosition write SetHandlePosition;
    property ShadowType: TShadowType read GetShadowType write SetShadowType;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ THandleBox }

procedure THandleBox.CreateHandle;
begin
  Handle := gtk_handle_box_new;
end;

function THandleBox.GetHandlePosition: TPosition;
begin
  Result := TPosition(gtk_handle_box_get_handle_position(Handle));
end;

procedure THandleBox.SetHandlePosition(AValue: TPosition);
begin
  gtk_handle_box_set_handle_position(Handle, Ord(AValue));
end;

function THandleBox.GetShadowType: TShadowType;
begin
  Result := TShadowType(gtk_handle_box_get_shadow_type(Handle));
end;

procedure THandleBox.SetShadowType(AValue: TShadowType);
begin
  gtk_handle_box_set_shadow_type(Handle, Ord(AValue));
end;

{$ENDIF}
