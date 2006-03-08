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

  TFrame = class(TBinControl)
  private
    function GetCaption: string;
    function GetShadowType: TShadowType;
    procedure SetCaption(Value: string);
    procedure SetShadowType(Value: TShadowType);
  protected
    procedure CreateHandle; override;
  public
  published
    property Caption: string read GetCaption write SetCaption;
    property ShadowType: TShadowType read GetShadowType write SetShadowType default stEtchedIn;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TFrame }

procedure TFrame.CreateHandle;
begin
  Handle := gtk_frame_new(nil);
end;

function TFrame.GetCaption: string;
begin
  Result := gtk_frame_get_label(Handle);
end;

function TFrame.GetShadowType: TShadowType;
begin
  Result := TShadowType(gtk_frame_get_shadow_type(Handle));
end;

procedure TFrame.SetCaption(Value: string);
begin
  gtk_frame_set_label(Handle, PChar(Value));
end;

procedure TFrame.SetShadowType(Value: TShadowType);
begin
  gtk_frame_set_shadow_type(Handle, Ord(Value));
end;

{$ENDIF}
