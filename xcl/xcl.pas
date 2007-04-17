(*
   XCL - XDE's Component Library
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING.XCL, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit xcl;

{$IFNDEF FPC}
  {$ERROR This code was not tested without FPC and may not compile}
  {$FATAL If you wanna try this, remove directive and tell me what happens (judison@gmail.com)}
{$ENDIF}

{$MODE ObjFpc}
{$H+}
{$INLINE ON}
{$MACRO ON} // We need it, for GTK Interfaces.

(*

Toda property "label" vira "Caption" (ou tem alguma sugestao melhor?)

*)

interface

uses Classes, Sysutils, Contnrs{$IFDEF FPC}, CustApp{$ENDIF};

type
  TQuark = DWord;
  TColor = String; // For the user a color is a string
  TColorData = Pointer; // PGdkColor

  TTreeIter = record // Think of it as a read-only structure
    STP: Integer;  // Do not touch
    UD1: Pointer;  // Do not touch
    UD2: Pointer;  // Do not touch
    UD3: Pointer;  // Do not touch
  end;

type
  TApplication = class;
  TTextBuffer = class;
  TTimer = class;
  TColormap = class;
  TIconTheme = class;
  {**************************}
  TGList = class;
  TGStrings = class;
  {**************************}
  TAccelGroup = class;
  TAccelerator = class;
  {**************************}
  TMenu = class;
  {**************************}
  TPixBuf = class;
  { ***      Action      *** }
  {} TActionList = class;
  {} TCustomAction = class;
  {  \-} TAction = class;
  { *** Dialog Hierarchy *** }
  {} TDialog = class;
  {  |-} TAboutDialog = class;
  {  |-  TFileSelDialog = class;  Deprecated!! }
  {  \-} TFileChooserDialog = class;
  { *** Class Hierarchy *** }
  {} TControl = class;
  {  |-} TMiscControl = class;
  {  |   |-} TArrow = class;
  {  |   |-} TLabel = class;
  {  |   \-} TImage = class;
  {  |-} TContainerControl = class;
  {  |   |-} TBinControl = class;
  {  |   |   |-} TAlignment = class;
  {  |   |   |-} TForm = class;
  {  |   |   |-} TFrame = class;
  {  |   |   |-} TCustomButton = class;
  {  |   |   |   |-} TColorButton = class;
  {  |   |   |   |-} TFontButton = class;
  {  |   |   |   |-} TButton = class;
  {  |   |   |   \-} TCustomToggleButton = class;
  {  |   |   |       |-} TToggleButton = class;
  {  |   |   |       \-} TCheckButton = class;
  {  |   |   |           \-} TRadioButton = class;
  {  |   |   |-} TScrolledWindow = class;
  {  |   |   |-} TNotebookPage = class;
  {  |   |   |-} THandleBox = class;
  {  |   |   |-} TExpander = class;
  {  |   |   |-} TViewport = class;
  {  |   |   \-} TCustomToolItem = class;
  {  |   |       |-} TToolItem = class;
  {  |   |       |-  TToolButton = class; XCL Deprecated (use TToolItem)}
  {  |   |       \-} TSeparatorToolItem = class;
  {  |   |-} TPaned = class;
  {  |   |   |-} THPaned = class;
  {  |   |   \-} TVPaned = class;
  {  |   |-} TFixed = class;
  {  |   |-} TMenuBar = class;
  {  |   |-} TNotebook = class;
  {  |   |-} TToolBar = class;
  {  |   |-} TTable = class;
  {  |   \-} TBox = class;
  {  |       |-} THBox = class;
  {  |       |-} TVBox = class;
  {  |       \-} TButtonBox = class;
  {  |           |-} THButtonBox = class;
  {  |           \-} TVButtonBox = class;
  {  |-} TDrawingArea = class;
  {  |-} TEntry = class;
  {  |   \-} TSpinButton = class;
  {  |-} TTextView = class;
  {  |-  TCombo = class; Deprecated!! }
  {  |-} TComboBox = class;
  {  |   \-} TComboBoxEntry = class;
  {  |-} TProgressBar = class;
  {  |-} TStatusBar = class;
  {  |-} TCalendar = class;
  {  |-} TColorSelection = class;
  {  |-} TFileChooserWidget = class;
  {  |-} TCustomMenuItem = class;
  {  |   |-} TMenuItem = class;
  {  |   |-} TSeparatorMenuItem = class;
  {  |   \-} TTearoffMenuItem = class;
  {  |-} TSeparator = class;
  {  |   |-} THSeparator = class;
  {  |   \-} TVSeparator = class;
  {  \-} TRange = class;
  {      |-} TScrollBar = class;
  {      |   |-} THScrollBar = class;
  {      |   \-} TVScrollBar = class;
  {      \-} TScale = class;
  {          |-} THScale = class;
  {          \-} TVScale = class;
  {**** TTreeView Related *****}
  TTreeView = class;
  TTreeViewColumn = class;
  {} TTreeModel = class;
  {  |-} TTreeStore = class;
  {  \-} TListStore = class;

  { *** Class Of's *** }
  TFormClass = class of TForm;
  //TTreeViewColumnClass = class of TTreeViewColumn;

  { *** Events *** }
  TSwitchPageEvent = procedure(Sender: TObject; NewPage: Integer) of object;
  TRowActivatedEvent = procedure(Sender: TObject; const Iter: TTreeIter; Column: TTreeViewColumn) of object;
  TExposeEvent = procedure(Sender: TObject; Area: TRect) of object;

  { *** Enumerations *** }
  TResizeMode = (rmParent, rmQueue, rmImmediate);
  TShadowType = (stNone, stIn, stOut, stEtchedIn, stEtchedOut);
  TJustification = (jsLeft, jsRight, jsCenter, jsFill);
  TUpdatePolicy = (upContinuous, upDiscontinuous, upDelayed);
  TScrollBarPolicy = (sbpAlways, sbpAutomatic, sbpNever);
  TPosition = (psLeft, psRight, psTop, psBottom);
  TButtonBoxLayout = (bblDefault, bblSpread, bblEdge, bblStart, bblEnd);
  TArrowType = (atUp, atDown, atLeft, atRight);
  TProgressBarOrientation = (poLeftToRight, poRightToLeft, poBottomToTop, poTopToBottom);
  TWidgetState = (wsNormal, wsActive, wsPreLight, wsSelected, wsInsensitive);
  TTreeViewColumnSizing = (tvcsGrowOnly, tvcsAutosize, tvcsFixed);
  TFileChooserAction = (fcaOpen, fcaSave, fcaSelectFolder, fcaCreateFolder);
  TOrientation = (orHorizontal, orVertical);
  TToolBarStyle = (tbsDefault, tbsIcons, tbsText, tbsBoth, tbsBothHoriz);
  TIconSize = (iszInvalid, iszMenu, iszSmallToolBar, iszLargeToolBar, iszButton, iszDnD, iszDialog);
  TRelief = (rlfNormal, rlfHalf, rlfNone);
  TSelectionMode = (smNone, smSingle, smBrowse{, smMultiple}); //Selection Mode Multiple is not yet implemented in XCL
  TSortType = (stAscending, stDescending);
  TAttachOption = (aoExpand, aoShrink, aoFill);
  //TImageType = (stEmpty, stPixmap, stImage, stPixbuf, stStock, stIconSet, stAnimation, stIconName);
  TCanvasFunction = (fnCopy, fnInvert, fnXor, fnClear, fnAnd, fnAndReverse, fnAndInvert, fnNoop, fnOr, fnEquiv, fnOrReverse, fnCopyInvert, fnOrInvert, fnNand, fnNor, fnSet);
  TCanvasFill = (flSolid, flTiled, flStippled, fsOpaqueStippled);
  TWrapMode = (wmNone, wmChar, wmWord, wmWordChar);
  TEllipsizeMode = (emNone, emStart, emMiddle, emEnd);
  TMessageType = (mtInfo, mtWarning, mtQuestion, mtError);
  TMessageButton = (mbOK, mbCancel, mbClose, mbYes, mbNo);
  TMessageResponse = (mrNone, mrOK, mrCancel, mrClose, mrYes, mrNo);

  {*** Sets ***}
  TAttachOptions = set of TAttachOption;
  TMessageButtons = set of TMessageButton;

{$DEFINE INC_READ_INTERFACE}
  {$I xcl.inc}
{$UNDEF INC_READ_INTERFACE}

  function ObjectByHandle(AHandle: Pointer): TObject;
  function ControlByHandle(AHandle: Pointer): TControl;

var
  Application: TApplication;
  Clipboard: TClipboard;
  Primary: TClipboard;

// Quarks
var { const }
  QTObject: DWord;

procedure Register;

implementation

uses
  // GLib, ATK, Pango, GDK, gdk2pixbuf, GTK+
  glib2, atk, pango, gdk2, gdk2pixbuf, gtk2;

{$I gtk2.inc}

//***********************************************************************

{$DEFINE INC_READ_IMPLEMENTATION}
  {$I xcl.inc}
{$UNDEF INC_READ_IMPLEMENTATION}

procedure Register;
begin
  RegisterComponents('Containers', [THBox, TVBox, TTable, THPaned, TVPaned, THButtonBox, TVButtonBox, TFrame, TNoteBook, TAlignment, TExpander, THandleBox, TScrolledWindow, TViewPort, TFixed]);
  RegisterComponents('Standard', [TActionList, TLabel, TButton, TToggleButton, TEntry, TSpinButton, TTextView, TCheckButton, TRadioButton, TComboBox, TComboBoxEntry, TImage]);
  RegisterComponents('Menu/Tool', [TMenuBar, TMenuItem, TSeparatorMenuItem, TToolBar, TToolItem, TSeparatorToolItem]);
  RegisterComponents('List/Tree', [TTreeView, TTreeStore, TListStore]);
  RegisterComponents('Additional', [THSeparator, TVSeparator, THScrollbar, TVScrollBar, THScale, TVScale, TProgressBar, TStatusBar, TArrow]);
  RegisterComponents('Extra', [TCalendar, TColorButton, TFontButton, TColorSelection, TFileChooserWidget]);
end;

initialization
  QTObject := QuarkByName('TObject');

//---------------------------------  Registrar as Classes
//-------------------- Nao Visuais
  RegisterClass(TActionList);           //OK
  RegisterClass(TAction);
  RegisterClass(TTreeStore);            //OK
  RegisterClass(TListStore);            //OK
  RegisterClass(TPixBuf);
  RegisterClass(TTimer);
  RegisterClass(TAccelGroup);
  RegisterClass(TAccelerator);
  RegisterClass(TMenu);
//-------------------- Visuais
  RegisterClass(TAboutDialog);
  RegisterClass(TArrow);                //OK
  RegisterClass(TLabel);                //OK
  RegisterClass(TImage);                //OK
  RegisterClass(TAlignment);            //OK
  RegisterClass(TForm);
  RegisterClass(TFrame);                //OK
  RegisterClass(TButton);               //OK
  RegisterClass(TColorButton);          //OK
  RegisterClass(TFontButton);           //OK
  RegisterClass(TToggleButton);         //OK
  RegisterClass(TCheckButton);          //OK
  RegisterClass(TRadioButton);          //OK
  RegisterClass(TScrolledWindow);       //OK
  RegisterClass(TNotebookPage);
  RegisterClass(THandleBox);            //OK
  RegisterClass(TExpander);             //OK
  RegisterClass(TViewPort);             //OK
  RegisterClass(TToolItem);             //OK
//RegisterClass(TToolButton);
  RegisterClass(TSeparatorToolItem);    //OK
  RegisterClass(THPaned);               //OK
  RegisterClass(TVPaned);               //OK
  RegisterClass(TMenuBar);              //OK
  RegisterClass(TNotebook);             //OK
  RegisterClass(TToolBar);              //OK
  RegisterClass(TFixed);                //OK
  RegisterClass(TTable);                //OK
  RegisterClass(THBox);                 //OK
  RegisterClass(TVBox);                 //OK
  RegisterClass(THButtonBox);           //OK
  RegisterClass(TVButtonBox);           //OK
  RegisterClass(TDrawingArea);
  RegisterClass(TEntry);                //OK
  RegisterClass(TSpinButton);           //OK
  RegisterClass(TTextView);             //OK
  // RegisterClass(TCombo); Deprecated!!
  RegisterClass(TComboBox);             //OK
  RegisterClass(TComboBoxEntry);        //OK
  RegisterClass(TProgressBar);          //OK
  RegisterClass(TStatusBar);            //OK
  RegisterClass(TCalendar);             //OK
  RegisterClass(TColorSelection);       //OK
  RegisterClass(TFileChooserWidget);    //OK
  RegisterClass(TMenuItem);             //OK
  RegisterClass(TSeparatorMenuItem);    //OK
  RegisterClass(TTearoffMenuItem);
  RegisterClass(THSeparator);           //OK
  RegisterClass(TVSeparator);           //OK
  RegisterClass(THScrollBar);           //OK
  RegisterClass(TVScrollBar);           //OK
  RegisterClass(THScale);               //OK
  RegisterClass(TVScale);               //OK
  RegisterClass(TTreeView);             //OK
//--
  RegisterClass(TFileChooserDialog);
//----------------------------------
  ResourceList := TList.Create;
  Application := TApplication.Create(nil);
  Clipboard := nil; // Created in Application.Initialize
  Primary := nil;   // Created in Application.Initialize
  //==============
  XCLMsgs := TList.Create;
finalization
  XCLMsgs.Free;
  //==============
  Primary.Free;
  Clipboard.Free;
  Application.Free;
  ResourceList.Free;
end.
