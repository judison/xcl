(*
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

  TCanvas = class(TPersistent)
  private
    FGC: Pointer; // GdkGC
    FDrawable: Pointer; // GdkDrawable
    FColormap: TColormap;
    procedure SetColormap(AValue: TColormap);
    //===============================
    FFunction: TCanvasFunction;
    FFill: TCanvasFill;
    FForeground: TColor;
    FBackground: TColor;
  protected
  public
    constructor Create(ADrawable: Pointer);
    destructor Destroy; override;
    procedure CreateGC;
    //===============================
    procedure SetFunction(AValue: TCanvasFunction); inline;
    procedure SetFill(AValue: TCanvasFill); inline;
    procedure SetForeground(AValue: TColor); inline;
    procedure SetBackground(AValue: TColor); inline;
    //===============================
    procedure Point(AX, AY: Integer); inline;
    procedure Line(AX1, AY1, AX2, AY2: Integer); inline;
    procedure Rectangle(AFilled: Boolean; AX, AY, AWidth, AHeight: Integer); inline;
    procedure Arc(AFilled: Boolean; AX, AY, AWidth, AHeight, AAngle1, AAngle2: Integer); inline;
    //===============================
    property Colormap: TColormap read FColorMap write SetColormap;
  published
    property Func: TCanvasFunction read FFunction write SetFunction default fnCopy;
    property Fill: TCanvasFill read FFill write SetFill default flSolid;
    property Foreground: TColor read FForeground write SetForeground;
    property Background: TColor read FBackground write SetBackground;
  end;

{$ENDIF}
//** IMPLEMENTATION
//****************************************************************************
{$IFDEF INC_READ_IMPLEMENTATION}

{ TCanvas }

constructor TCanvas.Create(ADrawable: Pointer);
begin
  FDrawable := ADrawable;
  FColormap := Application.Colormap; // pegar do Drawable
  CreateGC;
  //==
  g_object_ref(FDrawable);
  //g_object_ref(FGC); Acho que nao precisa...
end;

destructor TCanvas.Destroy;
begin
  //==
  g_object_unref(FDrawable);
  g_object_unref(FGC);
end;

procedure TCanvas.CreateGC;
var
  Values: TGdkGCValues;
  Mask: TGdkGCValuesMask;
begin
  FFunction := fnCopy;
  FFill := flSolid;

  Values._function := GDK_COPY;
  Values.fill := GDK_SOLID;
  Mask := GDK_GC_FUNCTION or GDK_GC_FILL;
  FGC := gdk_gc_new_with_values(FDrawable, @Values, Mask);
  //FGC := gdk_gc_new(FDrawable);
  gdk_gc_set_colormap(FGC, FColormap.Handle);
end;

procedure TCanvas.SetColormap(AValue: TColormap);
begin
  if FColormap <> nil then
    gdk_gc_set_colormap(FGC, FColormap.Handle)
  else
    gdk_gc_set_colormap(FGC, nil);
end;

procedure TCanvas.SetFunction(AValue: TCanvasFunction); inline;
begin
  gdk_gc_set_function(FGC, TGdkFunction(AValue));
end;

procedure TCanvas.SetFill(AValue: TCanvasFill); inline;
begin
  gdk_gc_set_fill(FGC, TGdkFill(AValue));
end;

procedure TCanvas.SetForeground(AValue: TColor); inline;
var
  ColorData: PGdkColor;
begin
  if AValue <> FForeground then
  begin
    FForeground := AValue;
    // TODO: use Colormap Functions
    // but it makes FPC run into Internal Error 200311075 compiling xcldb.pas :S
    // I really don't understand
    ColorData := New(PGdkColor);
    gdk_color_parse(PChar(FForeground), ColorData);
    gdk_colormap_alloc_color(FColormap.Handle, ColorData, False, True);
    gdk_gc_set_foreground(FGC, ColorData);
    gdk_color_free(ColorData);
  end;
end;

procedure TCanvas.SetBackground(AValue: TColor); inline;
var
  ColorData: PGdkColor;
begin
  if AValue <> FBackground then
  begin
    FBackground := AValue;
    // TODO: use Colormap Functions
    // but it makes FPC run into Internal Error 200311075 compiling xcldb.pas :S
    // I really don't understand
    ColorData := New(PGdkColor);
    gdk_color_parse(PChar(FBackground), ColorData);
    gdk_colormap_alloc_color(FColormap.Handle, ColorData, False, True);
    gdk_gc_set_background(FGC, ColorData);
    gdk_color_free(ColorData);
  end;
end;

procedure TCanvas.Point(AX, AY: Integer); inline;
begin
  gdk_draw_point(FDrawable, FGC, AX, AY);
end;

procedure TCanvas.Line(AX1, AY1, AX2, AY2: Integer); inline;
begin
  gdk_draw_line(FDrawable, FGC, AX1, AY1, AX2, AY2);
end;

procedure TCanvas.Rectangle(AFilled: Boolean; AX, AY, AWidth, AHeight: Integer); inline;
begin
  gdk_draw_rectangle(FDrawable, FGC, LongInt(AFilled), AX, AY, AWidth, AHeight);
end;

procedure TCanvas.Arc(AFilled: Boolean; AX, AY, AWidth, AHeight, AAngle1, AAngle2: Integer); inline;
begin
  gdk_draw_arc(FDrawable, FGC, LongInt(AFilled), AX, AY, AWidth, AHeight, AAngle1, AAngle2);
end;

{$ENDIF}
