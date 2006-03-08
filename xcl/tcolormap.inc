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

  TColormap = class(TObject)
  private
    Handle: Pointer;
    FHandleOwner: Boolean;
  protected
  public
    constructor Create(AHandle: Pointer);
    destructor Destroy; override;
    //--
    function NewColorData(ADesc: TColor): TColorData;
    procedure FreeColorData(var AColorData: TColorData);
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TColormap }

constructor TColormap.Create(AHandle: Pointer);
begin
  if AHandle = nil then
  begin
    FHandleOwner := True;
    Handle := gdk_colormap_new(gdk_visual_get_system, false);
  end
  else
  begin
    FHandleOwner := False;
    Handle := AHandle;
  end;
end;

destructor TColormap.Destroy;
begin
  if FHandleOwner then
    g_object_unref(Handle);
  inherited;
end;

function TColormap.NewColorData(ADesc: TColor): TColorData;
var
  NewData: PGdkColor;
begin
  NewData := New(PGdkColor);
  gdk_color_parse(PChar(ADesc), NewData);
  if gdk_colormap_alloc_color(Handle, NewData, False, True) then
    Result := NewData
  else
  begin
    Result := nil;
    Dispose(NewData);
  end;
end;

procedure TColormap.FreeColorData(var AColorData: TColorData);
begin
  if AColorData <> nil then
  begin
    gdk_color_free(AColorData);
    AColorData := nil;
  end;
end;

{$ENDIF}
