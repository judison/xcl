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

  IPixBufClient = interface
    procedure PixBufChanged(APixBuf: TPixBuf);
  end;

  TPixBuf = class(TComponent)
  private
    FGdkPixBuf: Pointer;
    FClientNotifyList: TList;
    FFileName: String;
    FResource: String;
    FIconName: String;
    FIconSize: Integer;
    procedure SetFileName(AValue: String);
    procedure SetResource(AValue: String);
    procedure SetIconName(AValue: String);
    procedure SetIconSize(AValue: Integer);
    procedure SetIconName(AName: String; ASize: Integer);
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
    property IconName: String read FIconName write SetIconName;
    property IconSize: Integer read FIconSize write SetIconSize;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

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
  if Assigned(FGdkPixBuf) then
    begin
      g_object_unref(FGdkPixBuf);
      FGdkPixBuf := nil;
    end;
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
    FIconName := '';

    if FileExists(AValue) then
    begin
      err := nil;
      FGdkPixBuf := gdk_pixbuf_new_from_file(PChar(FFileName), @err);

      RaiseGError(err);

      // acho q nao precisa disso
      //if Assigned(FGdkPixBuf) then
      //  g_object_ref(FGdkPixBuf);
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
  Err: PGError;
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
    FIconName := '';

    try
      GetResource('PIXDATA', FResource, RData, RSize);

      Err := nil;
      gdk_pixdata_deserialize(@PixData, RSize, RData, @Err);
      RaiseGError(Err);
  
      Err := nil;
      FGdkPixBuf := gdk_pixbuf_from_pixdata(@pixdata, false, @Err);
      RaiseGError(Err);
  
      if Assigned(FGdkPixBuf) then
        g_object_ref(FGdkPixBuf);
    except
      on EResNotFound do
      ; // Ignore
      else
        raise;
    end;

    Changed;
  end;
end;

procedure TPixBuf.SetIconName(AValue: String);
begin
  if AValue <> FIconName then
    SetIconName(AValue, FIconSize);
end;

procedure TPixBuf.SetIconSize(AValue: Integer);
begin
  if AValue <> FIconSize then
    SetIconName(FIconName, AValue);
end;

procedure TPixBuf.SetIconName(AName: String; ASize: Integer);
begin
  if Assigned(FGdkPixBuf) then
  begin
    g_object_unref(FGdkPixBuf);
    FGdkPixBuf := nil;
  end;

  FIconName := AName;
  FIconSize := ASize;
  FResource := '';
  FFileName := '';

  // We need a way to get stock icon when IconName starts with 'gtk-'
  if (FIconSize > 0) and (FIconName <> '') then
  begin
    if Application.IconTheme.HasIcon(FIconName) then
      FGdkPixBuf := Application.IconTheme.LoadIcon(FIconName, FIconSize);
    // Don't ref count!
  end;

  Changed;
end;

{$ENDIF}
