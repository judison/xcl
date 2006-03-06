(*
   $Id: tpixbuf.inc,v 1.3 2006/03/01 18:32:40 judison Exp $

   XCL - XDE's Component Library
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
//** INTERFACE
//****************************************************************************
{$IFDEF INC_READ_INTERFACE}

  IPixBufClient = interface
    procedure PixBufChanged(APixBuf: TPixBuf);
  end;

  TPixBuf = class(TComponent)
  private
    FGdkPixBuf: Pointer;
    FClientNotifyList: TList;
    FFileName: String;
    FResource: String;
    procedure SetFileName(AValue: String);
    procedure SetResource(AValue: String);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddClient(AValue: IPixBufClient);
    procedure RemoveClient(AValue: IPixBufClient);
    function GetGdkPixbuf: Pointer;
  published
    property FileName: String read FFileName write SetFileName;
    property Resource: String read FResource write SetResource;
  end;

{$ENDIF}
//** IMPLEMENTATION
//****************************************************************************
{$IFDEF INC_READ_IMPLEMENTATION}

{ TPixBuf }

constructor TPixBuf.Create(AOwner: TComponent);
begin
  inherited;
  FClientNotifyList := TList.Create;
  FGdkPixBuf := nil;
end;

destructor TPixBuf.Destroy;
begin
  FClientNotifyList.Free;
  inherited;
end;

procedure TPixBuf.AddClient(AValue: IPixBufClient);
begin
  FClientNotifyList.Add(Pointer(AValue));
end;

procedure TPixBuf.RemoveClient(AValue: IPixBufClient);
begin
  FClientNotifyList.Remove(Pointer(AValue));
end;

procedure TPixBuf.Changed;
var
  I: Integer;
begin
  for I := 0 to FClientNotifyList.Count -1 do
    IPixBufClient(FClientNotifyList[I]).PixBufChanged(Self);
end;

function TPixBuf.GetGdkPixbuf: Pointer;
begin
  Result := FGdkPixBuf;
end;

procedure TPixBuf.SetFileName(AValue: String);
var
  err: PGError;
begin
  if AValue <> FFileName then
  begin
    if Assigned(FGdkPixBuf) then
    begin
      g_object_unref(FGdkPixBuf);
      FGdkPixBuf := nil;
    end;

    FFileName := AValue;
    FResource := '';

    if FileExists(AValue) then
    begin
      err := nil;
      FGdkPixBuf := gdk_pixbuf_new_from_file(PChar(FFileName), @err);
      if err <> nil then
      begin
        WriteLn('GTK Error loading pixbuf!');
      end;
      if Assigned(FGdkPixBuf) then
        g_object_ref(FGdkPixBuf);
    end;
    Changed;
  end;
end;

type
  PGdkPixdata = ^TGdkPixdata;
  TGdkPixdata = record
    magic: guint32;        // GDK_PIXBUF_MAGIC_NUMBER
    length: gint32;        // <1 to disable length checks, otherwise:
			   // GDK_PIXDATA_HEADER_LENGTH + pixel_data length
    pixdata_type: guint32; // GdkPixdataType
    rowstride: guint32;
    width: guint32;
    height: guint32;
    pixel_data: PByte;
  end;

function gdk_pixdata_deserialize(pixdata: PGdkPixdata; stream_length: guint; const stream: PByte; error: PPGError): gboolean; cdecl; external gdkpixbuflib;
function gdk_pixbuf_from_pixdata(const pixdata: PGdkPixdata; copy_pixels: gboolean; error: PPGError): PGdkPixbuf; cdecl; external gdkpixbuflib;

procedure TPixBuf.SetResource(AValue: String);
var
  RData: Pointer;
  RSize: LongWord;
  PixData: TGdkPixData;
begin
  if AValue <> FResource then
  begin
    if Assigned(FGdkPixBuf) then
    begin
      g_object_unref(FGdkPixBuf);
      FGdkPixBuf := nil;
    end;

    FResource := AValue;
    FFileName := '';

    try
      GetResource('PIXDATA', FResource, RData, RSize);
      gdk_pixdata_deserialize(@PixData, RSize, RData, nil);
      FGdkPixBuf := gdk_pixbuf_from_pixdata(@pixdata, false, nil);
      if Assigned(FGdkPixBuf) then
        g_object_ref(FGdkPixBuf);
    except
    end;

    Changed;
  end;
end;

{$ENDIF}
{
$Log: tpixbuf.inc,v $
Revision 1.3  2006/03/01 18:32:40  judison
Added PIXDATA resources

Revision 1.2  2005/12/30 17:21:38  judison
* Moved DB Comps to xcldb.inc
* Added TCanvas

Revision 1.1  2005/12/02 22:31:34  judison
Long-term changes (again) I need to use cvs more!

}