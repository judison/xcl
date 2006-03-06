(*
   $Id: tcolormap.inc,v 1.3 2005/12/30 17:21:38 judison Exp $

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

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

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
{
$Log: tcolormap.inc,v $
Revision 1.3  2005/12/30 17:21:38  judison
* Moved DB Comps to xcldb.inc
* Added TCanvas

Revision 1.2  2005/03/26 05:21:47  judison
+ CVS Log Tag

}