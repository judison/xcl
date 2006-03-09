{
   GTK/XCL implementation for SHEdit
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>
   Copyright (C) 1999-2000 Sebastian Guenther <sg@freepascal.org>

   See the file COPYING.XCL, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit sheditor;

interface

{$MODE objfpc}
{$H+}

uses
  SysUtils, Classes, xcl, doc_text, SHEdit, gtk2, gdk2;

const
  colBlack       = '#000000';
  colDarkBlue    = '#000080';
  colBlue        = '#0000ff';
  colDarkGreen   = '#008000';
  colGreen       = '#00ff00';
  colDarkCyan    = '#008080';
  colCyan        = '#00ffff';
  colBrown       = '#800000';
  colRed         = '#ff0000';
  colDarkMagenta = '#800080';
  colMagenta     = '#ff00ff';
  colDarkYellow  = '#808000';
  colYellow      = '#ffff00';
  colGray        = '#808080';
  colGrey        = colGray;
  colLightGray   = '#c0c0c0';
  colLightGrey   = colLightGray;
  colWhite       = '#ffffff';

//  colInvalid     = $ff000000;
  colDefault     = 'default';

type

  TSHFontStyle = (fsNormal, fsBold, fsItalics, fsBoldItalics);

  TSHStyle = record
    Name: String[32];
    Color, Background: TColor; //LongWord;
    FontStyle: TSHFontStyle;
  end;

//  TSHStyleArray = array[1..255] of TSHStyle;  // Notice the 1!

  TSHEditor = class;

  TSHEditorInt = class(ISHWidget)
    FEditor: TSHEditor;
    constructor Create(AEditor: TSHEditor);
    //--
    procedure InvalidateRect(x, y, w, h: Integer); override;
    procedure ClearRect(x, y, w, h: Integer); override;
    procedure DrawTextLine(x1, x2, y: Integer; s: PChar); override;
    procedure ShowCursor(x, y: Integer); override;
    procedure HideCursor(x, y: Integer); override;
    function  GetHorzPos: Integer; override;
    procedure SetHorzPos(x: Integer); override;
    function  GetVertPos: Integer; override;
    procedure SetVertPos(y: Integer); override;
    function  GetPageWidth: Integer; override;
    function  GetPageHeight: Integer; override;
    function  GetLineWidth: Integer; override;
    procedure SetLineWidth(count: Integer); override;
    function  GetLineCount: Integer; override;
    procedure SetLineCount(count: Integer); override;
    function  GetClipboard: String; override;
    procedure SetClipboard(Content: String); override;
  end;

  TSHEditor = class(TControl)
  protected
    FInterface: TSHEditorInt;

    SHStyles: array[1..255] of TSHStyle;
    SHStyleCount: Integer;              // # of currently registered styles
    shWhitespace: Integer;
    CurGCColor: TColor;

    hadj, vadj: PGtkAdjustment;
    PaintBox: PGtkWidget;
    FEdit: TSHTextEdit;
    LeftIndent: Integer;
    CharW, CharH: Integer;
    Font: array[TSHFontStyle] of PGdkFont; // Fonts for content drawing
    gc: PGdkGC;
    GdkWnd: PGdkWindow;

    procedure SetGCColor(AColor: TColor);

    // ISHWidget Implemenation:
    procedure InvalidateRect(Ax, Ay, w, h: Integer);
    procedure ClearRect(Ax, Ay, w, h: Integer);
    procedure DrawTextLine(x1, x2, ay: Integer; s: PChar);
    procedure ShowCursor(Ax, Ay: Integer);
    procedure HideCursor(Ax, Ay: Integer);
    function  GetHorzPos: Integer; 
    procedure SetHorzPos(Ax: Integer);
    function  GetVertPos: Integer; 
    procedure SetVertPos(Ay: Integer);
    function  GetPageWidth: Integer; 
    function  GetPageHeight: Integer; 
    function  GetLineWidth: Integer; 
    procedure SetLineWidth(count: Integer); 
    function  GetLineCount: Integer; 
    procedure SetLineCount(count: Integer); 
    function  GetClipboard: String; 
    procedure SetClipboard(Content: String); 

    // ???? judison
    property  HorzPos: Integer read GetHorzPos write SetHorzPos;
    property  VertPos: Integer read GetVertPos write SetVertPos;
    property  PageWidth: Integer read GetPageWidth;
    property  PageHeight: Integer read GetPageHeight;
    property  LineWidth: Integer read GetLineWidth write SetLineWidth;
    property  LineCount: Integer read GetLineCount write SetLineCount;
    property  Clipboard: String read GetClipboard write SetClipboard;

  public
    //Widget: PGtkWidget;  // this is the outer editor widget

    constructor Create(AOwner: TComponent; ADoc: TTextDoc; AEditClass: TSHTextEditClass);
    destructor Destroy; override;

    procedure CreateHandle; override;

    procedure SetFocus;

    function  AddSHStyle(AName: String; AColor, ABackground: TColor;
      AStyle: TSHFontStyle): Integer;
    property Edit: TSHTextEdit read FEdit;
  end;


implementation

constructor TSHEditorInt.Create(AEditor: TSHEditor);
begin
  FEditor := AEditor;
end;

procedure TSHEditorInt.InvalidateRect(x, y, w, h: Integer);
begin
  FEditor.InvalidateRect(x, y, w, h);
end;

procedure TSHEditorInt.ClearRect(x, y, w, h: Integer);
begin
  FEditor.ClearRect(x, y, w, h);
end;

procedure TSHEditorInt.DrawTextLine(x1, x2, y: Integer; s: PChar);
begin
  FEditor.DrawTextLine(x1, x2, y, s);
end;

procedure TSHEditorInt.ShowCursor(x, y: Integer);
begin
  FEditor.ShowCursor(x, y);
end;

procedure TSHEditorInt.HideCursor(x, y: Integer);
begin
  FEditor.HideCursor(x, y);
end;

function  TSHEditorInt.GetHorzPos: Integer;
begin
  Result := FEditor.GetHorzPos;
end;

procedure TSHEditorInt.SetHorzPos(x: Integer);
begin
  FEditor.SetHorzPos(x);
end;

function  TSHEditorInt.GetVertPos: Integer;
begin
  Result := FEditor.GetVertPos;
end;

procedure TSHEditorInt.SetVertPos(y: Integer);
begin
  FEditor.SetVertPos(y);
end;

function  TSHEditorInt.GetPageWidth: Integer;
begin
  Result := FEditor.GetPageWidth;
end;

function  TSHEditorInt.GetPageHeight: Integer;
begin
  Result := FEditor.GetPageHeight;
end;

function  TSHEditorInt.GetLineWidth: Integer;
begin
  Result := FEditor.GetLineWidth;
end;

procedure TSHEditorInt.SetLineWidth(count: Integer);
begin
  FEditor.SetLineWidth(count);
end;

function  TSHEditorInt.GetLineCount: Integer;
begin
  Result := FEditor.GetLineCount;
end;

procedure TSHEditorInt.SetLineCount(count: Integer);
begin
  FEditor.SetLineCount(count);
end;

function  TSHEditorInt.GetClipboard: String;
begin
  Result := FEditor.GetClipboard;
end;

procedure TSHEditorInt.SetClipboard(Content: String);
begin
  FEditor.SetClipboard(Content);
end;

var
  InternalClipboardContent: String;


{*****************************************************************************
                              GTK/GDK Callbacks
*****************************************************************************}

procedure TSHEditor_Expose(GtkWidget: PGtkWidget; event: PGdkEventExpose;
  widget: TSHEditor); cdecl;
var
  x, y, w, h: Integer;
begin
  x := (event^.area.x - widget.LeftIndent) div widget.CharW;
  y := event^.area.y div widget.CharH;
  w := (event^.area.x + event^.area.width + widget.CharW - 1) div widget.CharW - x;
  h := (event^.area.y + event^.area.height + widget.CharH - 1) div widget.CharH - y;
//  WriteLn(Format('Expose(%d/%d, %dx%d) for %s', [x, y, w, h, FEdit.ClassName]));

  widget.GdkWnd := widget.PaintBox^.window;
  widget.GC := gdk_gc_new(widget.GdkWnd);
  widget.CurGCColor := '#000000';         // Reset color, because we have a new GC!

  gdk_gc_copy(widget.GC, PGtkStyle(widget.PaintBox^.style)^.fg_gc[widget.PaintBox^.state]);

  widget.FEdit.AdjustCursorToRange;
  widget.FEdit.DrawContent(x, y, w, h);
end;


function TSHEditor_KeyPressed(GtkWidget: PGtkWidget; Event: PGdkEventKey;
  widget: TSHEditor): Integer; cdecl;
var
  KeyState,
  KeyCode: LongWord;
  KeyMods: TShiftState;
begin
  Result := 1;

  case Event^.KeyVal of
    GDK_KEY_Return       : KeyCode:=13;
    GDK_KEY_KP_Insert    : KeyCode:=GDK_KEY_Insert;
    GDK_KEY_KP_Home      : KeyCode:=GDK_KEY_Home;
    GDK_KEY_KP_Left      : KeyCode:=GDK_KEY_Left;
    GDK_KEY_KP_Up        : KeyCode:=GDK_KEY_Up;
    GDK_KEY_KP_Right     : KeyCode:=GDK_KEY_Right;
    GDK_KEY_KP_Down      : KeyCode:=GDK_KEY_Down;
    GDK_KEY_KP_Page_Up   : KeyCode:=GDK_KEY_Page_Up;
    GDK_KEY_KP_Page_Down : KeyCode:=GDK_KEY_Page_Down;
    GDK_KEY_KP_End       : KeyCode:=GDK_KEY_End;
    GDK_KEY_Scroll_Lock,
    GDK_KEY_Num_Lock,
    GDK_KEY_Shift_L..GDK_KEY_Hyper_R :
      begin
        // Don't let modifier keys trough as normal keys
        // *** This doesn't work reliably! (sg)
        exit;
      end;
  else
    KeyCode:=Event^.KeyVal;
  end;
  KeyState:=Event^.State;

  // WriteLn('KeyCode ', KeyCode,'   keystate ',KeyState);

  // Calculate the Key modifiers (shiftstate)
  KeyMods := [];
  if (KeyState and 1) <> 0 then KeyMods := KeyMods + [ssShift];
  if (KeyState and 2) <> 0 then KeyMods := KeyMods + [ssCaps];
  if (KeyState and 4) <> 0 then KeyMods := KeyMods + [ssCtrl];
  if (KeyState and 8) <> 0 then KeyMods := KeyMods + [ssAlt];
  if (KeyState and $10) <> 0 then KeyMods := KeyMods + [ssNum];
  if (KeyState and $40) <> 0 then KeyMods := KeyMods + [ssSuper];
  if (KeyState and $80) <> 0 then KeyMods := KeyMods + [ssScroll];
  if (KeyState and $100) <> 0 then KeyMods := KeyMods + [ssLeft];
  if (KeyState and $200) <> 0 then KeyMods := KeyMods + [ssMiddle];
  if (KeyState and $400) <> 0 then KeyMods := KeyMods + [ssRight];
  if (KeyState and $2000) <> 0 then KeyMods := KeyMods + [ssAltGr];

  widget.FEdit.KeyPressed(KeyCode,KeyMods);
end;


function TSHEditor_ButtonPressEvent(GtkWidget: PGtkWidget;
  event: PGdkEventButton; widget: TSHEditor): Integer; cdecl;
begin
  widget.FEdit.CursorX := Round((event^.x - widget.LeftIndent) / widget.CharW);
  widget.FEdit.CursorY := Trunc(event^.y) div widget.CharH;
  widget.SetFocus;
  Result := 1;
end;


function TSHEditor_FocusInEvent(GtkWidget: PGtkWidget;
  event: PGdkEventFocus; widget: TSHEditor): Integer; cdecl;
begin
//  Writeln('focus in');
  widget.FEdit.FocusIn;
  result:=1;
end;


function TSHEditor_FocusOutEvent(GtkWidget: PGtkWidget; event: PGdkEventFocus; widget: TSHEditor): Integer; cdecl;
begin
//  Writeln('focus out');
  widget.FEdit.FocusOut;
  result:=1;
end;


{*****************************************************************************
                                 TSHEditor
*****************************************************************************}

constructor TSHEditor.Create(AOwner: TComponent; ADoc: TTextDoc; AEditClass: TSHTextEditClass);
var
  lfd: String;    // Logical font descriptor
  i: Integer;
begin
  inherited Create(AOwner);

  // Create fonts
  for i := 0 to 3 do begin
    lfd := '-*-courier-';
    if (i and 1) <> 0 then lfd := lfd + 'bold'
    else lfd := lfd + 'medium';
    lfd := lfd + '-';
    if (i and 2) <> 0 then lfd := lfd + 'i'
    else lfd := lfd + 'r';
    lfd := lfd + '-normal--14-*-*-*-*-*-iso8859-1';
    Font[TSHFontStyle(i)] := gdk_font_load(PChar(lfd));
  end;

  CharW := gdk_char_width(Font[fsBold], ' ')+1; // by judison (+1)
  CharH := 14 {=FontHeight} + 3;   // *** find better way to determine max. cell height

  LeftIndent := CharW;

  // CreateHandle
  FInterface := TSHEditorInt.Create(Self);

  FEdit := AEditClass.Create(ADoc, FInterface);
  shWhitespace       := AddSHStyle('Whitespace', colBlack, colWhite,    fsNormal);
  FEdit.shDefault    := AddSHStyle('Default',    colBlack, colWhite,    fsNormal);
  FEdit.shSelected   := AddSHStyle('Selected',   colWhite, colDarkBlue, fsNormal);
{ Install keys }
  FEdit.AddKeyDef(@FEdit.CursorUp, selClear, 'Cursor up', GDK_KEY_Up, []);
  FEdit.AddKeyDef(@FEdit.CursorDown, selClear, 'Cursor down', GDK_KEY_Down, []);
  FEdit.AddKeyDef(@FEdit.CursorLeft, selClear, 'Cursor left', GDK_KEY_Left, []);
  FEdit.AddKeyDef(@FEdit.CursorRight, selClear, 'Cursor right', GDK_KEY_Right, []);
  FEdit.AddKeyDef(@FEdit.CursorHome, selClear, 'Cursor Home', GDK_KEY_Home, []);
  FEdit.AddKeyDef(@FEdit.CursorEnd, selClear, 'Cursor Home', GDK_KEY_End, []);
  FEdit.AddKeyDef(@FEdit.CursorPageUp, selClear, 'Cursor PageUp', GDK_KEY_Page_Up, []);
  FEdit.AddKeyDef(@FEdit.CursorPageDown, selClear, 'Cursor PageDown', GDK_KEY_Page_Down, []);
  FEdit.AddKeyDef(@FEdit.CursorDocBegin, selClear, 'Cursor Document Start', GDK_KEY_Page_Up, [ssCtrl]);
  FEdit.AddKeyDef(@FEdit.CursorDocEnd, selClear, 'Cursor Document End', GDK_KEY_Page_Down, [ssCtrl]);

  FEdit.AddKeyDef(@FEdit.CursorUp, selExtend, 'Selection up', GDK_KEY_Up, [ssShift]);
  FEdit.AddKeyDef(@FEdit.CursorDown, selExtend, 'Selection down', GDK_KEY_Down, [ssShift]);
  FEdit.AddKeyDef(@FEdit.CursorLeft, selExtend, 'Selection left', GDK_KEY_Left, [ssShift]);
  FEdit.AddKeyDef(@FEdit.CursorRight, selExtend, 'Selection right', GDK_KEY_Right, [ssShift]);
  FEdit.AddKeyDef(@FEdit.CursorHome, selExtend, 'Selection Home', GDK_KEY_Home, [ssShift]);
  FEdit.AddKeyDef(@FEdit.CursorEnd, selExtend, 'Selection Home', GDK_KEY_End, [ssShift]);
  FEdit.AddKeyDef(@FEdit.CursorPageUp, selExtend, 'Selection PageUp', GDK_KEY_Page_Up, [ssShift]);
  FEdit.AddKeyDef(@FEdit.CursorPageDown, selExtend, 'Selection PageDown', GDK_KEY_Page_Down, [ssShift]);
  FEdit.AddKeyDef(@FEdit.CursorDocBegin, selExtend, 'Selection Document Start', GDK_KEY_Page_Up, [ssCtrl,ssShift]);
  FEdit.AddKeyDef(@FEdit.CursorDocEnd, selExtend, 'Selection Document End', GDK_KEY_Page_Down, [ssCtrl,ssShift]);

  FEdit.AddKeyDef(@FEdit.ToggleOverwriteMode, selNothing, 'Toggle overwrite mode', GDK_KEY_Insert, []);
  FEdit.AddKeyDef(@FEdit.EditDelLeft, selClear, 'Delete char left of cursor', GDK_KEY_Backspace, []);
  FEdit.AddKeyDef(@FEdit.EditDelRight, selClear, 'Delete char right of cursor', GDK_KEY_Delete, []);
  FEdit.AddKeyDef(@FEdit.EditDelLine, selClear, 'Delete current line', Ord('Y'), [ssCtrl]);
  FEdit.AddKeyDef(@FEdit.EditDelLine, selClear, 'Delete current line', Ord('y'), [ssCtrl]);
  FEdit.AddKeyDef(@FEdit.EditUndo, selClear, 'Undo last action', GDK_KEY_Backspace, [ssAlt]);
  FEdit.AddKeyDef(@FEdit.EditRedo, selClear, 'Redo last undone action', GDK_KEY_Backspace, [ssShift, ssAlt]);
end;

destructor TSHEditor.Destroy;
begin
  FEdit.Free;
  inherited Destroy;
end;

procedure TSHEditor.CreateHandle;
begin
  hadj := PGtkAdjustment(gtk_adjustment_new(0, 0, 200, 1, 10, 100));
  vadj := PGtkAdjustment(gtk_adjustment_new(0, 0, 200, 1, 10, 100));
  Handle := gtk_scrolled_window_new(hadj, vadj);

  PaintBox := gtk_drawing_area_new;

  gtk_scrolled_window_add_with_viewport(Handle, PaintBox);
  gtk_widget_show(PaintBox);

  gtk_widget_set_flags(PaintBox, GTK_CAN_FOCUS);


  gtk_signal_connect(PGtkObject(PaintBox), 'expose-event', GTK_SIGNAL_FUNC(@TSHEditor_Expose), self);
  gtk_signal_connect_after(PGtkObject(PaintBox), 'key-press-event', GTK_SIGNAL_FUNC(@TSHEditor_Keypressed), self);
  gtk_signal_connect(PGtkObject(PaintBox), 'button-press-event', GTK_SIGNAL_FUNC(@TSHEditor_ButtonPressEvent), self);
  gtk_signal_connect_after(PGtkObject(PaintBox), 'focus-in-event', GTK_SIGNAL_FUNC(@TSHEditor_FocusInEvent), self);
  gtk_signal_connect_after(PGtkObject(PaintBox), 'focus-out-event', GTK_SIGNAL_FUNC(@TSHEditor_FocusOutEvent), self);


  gtk_widget_set_events(PGtkWidget(Paintbox), GDK_EXPOSURE_MASK or GDK_KEY_PRESS_MASK or GDK_KEY_RELEASE_MASK or GDK_BUTTON_PRESS_MASK or GDK_ENTER_NOTIFY_MASK or GDK_LEAVE_NOTIFY_MASK);

  //gtk_widget_show(Handle); // ????

end;


function TSHEditor.AddSHStyle(AName: String; AColor, ABackground: TColor; AStyle: TSHFontStyle): Integer;
begin
  Inc(SHStyleCount);
  SHStyles[SHStyleCount].Name       := AName;
  SHStyles[SHStyleCount].Color      := AColor;
  SHStyles[SHStyleCount].Background := ABackground;
  SHStyles[SHStyleCount].FontStyle  := AStyle;
  Result := SHStyleCount;
end;


procedure TSHEditor.SetGCColor(AColor: TColor);
var
  c: TGdkColor;
begin

  if AColor <> CurGCColor then
  begin
    c.pixel := 0;
    {
    c.red   := (((AColor shr 16) and 255) * 65535) div 255;
    c.green := (((AColor shr 8) and 255) * 65535) div 255;
    c.blue  := ((AColor and 255) * 65535) div 255;
    }
    ColorToRGB(AColor, c.red, c.green, c.blue);
    gdk_colormap_alloc_color(gdk_colormap_get_system, @c, False, True);
    if GC <> nil then // by judison
      gdk_gc_set_foreground(gc, @c);
    CurGCColor := AColor;
  end;
end;


procedure TSHEditor.ClearRect(ax, ay, w, h: Integer);
begin
  SetGCColor(SHStyles[shWhitespace].Background);
  if GdkWnd <> nil then // by judison
  gdk_draw_rectangle(PGdkDrawable(GdkWnd), GC, 1, ax * CharW + LeftIndent, ay * CharH, w * CharW, h * CharH);
end;


procedure TSHEditor.InvalidateRect(ax, ay, w, h: Integer);
var
  r : TGdkRectangle;
begin
  r.x := ax * CharW + LeftIndent;
  r.y := ay * CharH;
  r.Width := w * CharW;
  r.Height := h * CharH;
  gtk_widget_draw(PGtkWidget(PaintBox), @r);
end;


procedure TSHEditor.DrawTextLine(x1, x2, ay: Integer; s: PChar);
var
  CurColor: TColor;
  CurX1, CurX2: Integer;

  procedure DoErase;
  begin
    SetGCColor(CurColor);
    if CurX1 < x1 then
      CurX1 := x1;
    if CurX2 > CurX1 then begin
      gdk_draw_rectangle(PGdkDrawable(GdkWnd), GC, 1,
        CurX1 * CharW + LeftIndent, ay * CharH, (CurX2 - CurX1) * CharW, CharH);
    end;
    CurX1 := CurX2;
  end;

var
  RequestedColor: Integer;
  NewColor: TColor;
  hs : PChar;
begin
//  WriteLn('A');
  // Erase the (potentially multi-coloured) background

  hs := s;
  CurColor := SHStyles[shWhitespace].Background;
  
  CurX1 := 0;
  CurX2 := 0;
  while (hs[0] <> #0) and (CurX2 <= x2) do begin
    case hs[0] of
      LF_Escape: begin
          NewColor := SHStyles[Ord(hs[1])].Background;
          if NewColor = colDefault then
            NewColor := SHStyles[shWhitespace].Background;
          if NewColor <> CurColor then begin
            DoErase;
            CurColor := NewColor;
          end;
          Inc(hs, 2);
        end;
      #9: begin
          repeat
            Inc(CurX2);
          until (CurX2 and 7) = 0;
          Inc(hs);
        end;
      else begin
        Inc(hs);
        Inc(CurX2);
      end;
    end;
  end;
  CurX2 := x2;
  DoErase;
  // Draw text line

  RequestedColor := shWhitespace;
  CurX1 := 0;
  while s[0] <> #0 do
    case s[0] of
      LF_Escape:
        begin
          RequestedColor := Ord(s[1]);
	  if RequestedColor = 0 then
            RequestedColor := shWhiteSpace;
          Inc(s, 2);
        end;
      #9:
        begin
          repeat
            Inc(CurX1);
          until (CurX1 and 7) = 0;
          Inc(s);
        end;
      ' ':
        begin
          Inc(s);
          Inc(CurX1);
        end;
      else
      begin
//        WriteLn('C:A');
        if (CurX1 >= x1) and (CurX1 <= x2) then
        begin
          SetGCColor(SHStyles[RequestedColor].Color);
          if Font[SHStyles[RequestedColor].FontStyle] <> nil then // by judison
          gdk_draw_text(PGdkDrawable(GdkWnd), Font[SHStyles[RequestedColor].FontStyle], GC, CurX1 * CharW + LeftIndent, (ay + 1) * CharH - 3, s, 1)
          else writeln('a');
        end;
        Inc(s);
        Inc(CurX1);
//        WriteLn('C:B');
      end;
    end;
//  WriteLn('Z');
end;


procedure TSHEditor.SetFocus;
begin
  gtk_window_set_focus(PGtkWindow(gtk_widget_get_toplevel(Paintbox)),Paintbox);
end;


procedure TSHEditor.ShowCursor(ax, ay: Integer);
begin
//  writeln('Showcursor ',ax,',',ay);
  if assigned(GdkWnd) then
   begin
     SetGCColor(colBlack);
     gdk_draw_rectangle(PGdkDrawable(GdkWnd), GC, 1, ax*CharW + LeftIndent, ay*CharH, 2, CharH);
   end;
end;


procedure TSHEditor.HideCursor(ax, ay: Integer);
var
  r : TGdkRectangle;
begin
//  writeln('Hidecursor ',ax,',',ay);
  r.x := ax * CharW + LeftIndent;
  r.y := ay * CharH;
  r.Width := 2;
  r.Height := CharH;
  gtk_widget_draw(PGtkWidget(PaintBox), @r);
end;


function TSHEditor.GetLineWidth: Integer;
begin
  Result := (Trunc(hadj^.upper)-LeftIndent) div CharW;
end;


procedure TSHEditor.SetLineWidth(count: Integer);
begin
  hadj^.upper := count * CharW + LeftIndent;
  gtk_adjustment_changed(hadj);
  gtk_widget_set_usize(PaintBox, Trunc(hadj^.upper), Trunc(vadj^.upper));
end;


function TSHEditor.GetLineCount: Integer;
begin
  Result := Trunc(vadj^.upper) div CharH;
end;


procedure TSHEditor.SetLineCount(count: Integer);
begin
  vadj^.upper := (count+1) * CharH;
  gtk_adjustment_changed(vadj);
  gtk_widget_set_usize(PaintBox, Trunc(hadj^.upper), Trunc(vadj^.upper));
end;


function TSHEditor.GetClipboard: String;
begin
  Result := InternalClipboardContent;
end;


procedure TSHEditor.SetClipboard(Content: String);
begin
  InternalClipboardContent := Content;
end;


function TSHEditor.GetHorzPos: Integer;
begin
  Result := Trunc(hadj^.value);
  if Result>0 then
   Result:=(Result-LeftIndent) div CharW;
end;


procedure TSHEditor.SetHorzPos(ax: Integer);
begin
  if ax>0 then
   ax:=ax*CharW+LeftIndent;
  gtk_adjustment_set_value(hadj, ax);
end;


function TSHEditor.GetVertPos: Integer;
begin
  Result := (Trunc(vadj^.value)+CharH-1) div CharH;
end;


procedure TSHEditor.SetVertPos(ay: Integer);
begin
  gtk_adjustment_set_value(vadj, ay*CharH);
end;


function TSHEditor.GetPageWidth: Integer;
begin
  Result := Trunc(hadj^.page_size) div CharW;
end;


function TSHEditor.GetPageHeight: Integer;
begin
  Result := Trunc(vadj^.page_size) div CharH;
end;

end.
