(*
   $Id: xcl.pas,v 1.35 2006/03/04 20:17:29 judison Exp $

   XCL - XDE's Component Library
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING.FPC, included in this distribution,
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

{$H+}
{$MODE ObjFpc}
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
  TAccelerator = String;

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
  {**************************}
  TGList = class;
  TGStrings = class;
  {**************************}
  TPixBuf = class;
  { ***      Action      *** }
  {} TActionList = class;
  {} TContainedAction = class;
  {  \-} TCustomAction = class;
  {      \-} TAction = class;
  {} TActionLink = class;
  {  \-} TControlActionLink = class;
  { *** Dialog Hierarchy *** }
  {} TDialog = class;
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
  {  |   |   |-} TForm = class;
  {  |   |   |-} TFrame = class;
  {  |   |   |-} TCustomButton = class;
  {  |   |   |   |-} TColorButton = class;
  {  |   |   |   \-} TButton = class;
  {  |   |   |       \-} TCustomToggleButton = class;
  {  |   |   |           |-} TToggleButton = class;
  {  |   |   |           \-} TCheckButton = class;
  {  |   |   |               \-  TRadioButton = class;}
  {  |   |   |-} TScrolledWindow = class;
  {  |   |   |-} TNotebookPage = class;
  {  |   |   |-} THandleBox = class;
  {  |   |   |-} TExpander = class;
  {  |   |   |-} TViewPort = class;
  {  |   |   \-} TCustomToolItem = class;
  {  |   |       |-} TToolItem = class;
  {  |   |       |-} TToolButton = class;
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

  {*** Sets ***}
  TAttachOptions = set of TAttachOption;

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
  RegisterComponents('Containers', [THBox, TVBox, TTable, THPaned, TVPaned, THButtonBox, TVButtonBox, TFrame, TExpander, THandleBox, TScrolledWindow, TViewPort, TFixed]);
  RegisterComponents('Standard', [TLabel, TButton, TToggleButton, TEntry, TSpinButton, TTextView, TCheckButton, TComboBox, TComboBoxEntry, TImage]);
  RegisterComponents('Additional', [THSeparator, TVSeparator, THScrollbar, TVScrollBar, THScale, TVScale, TProgressBar, TStatusBar, TArrow]);
  RegisterComponents('Extra', [TCalendar, TColorButton, TColorSelection, TFileChooserWidget]);
end;

initialization
  QTObject := QuarkByName('TObject');

//---------------------------------  Registrar as Classes
//-------------------- Nao Visuais
  RegisterClass(TActionList);
  RegisterClass(TAction);
  RegisterClass(TTreeStore);
  RegisterClass(TListStore);
  RegisterClass(TPixBuf);
  RegisterClass(TTimer);
//-------------------- Visuais
  RegisterClass(TArrow);                //OK
  RegisterClass(TLabel);                //OK
  RegisterClass(TImage);                //OK
  RegisterClass(TForm);
  RegisterClass(TFrame);                //OK
  RegisterClass(TButton);               //OK
  RegisterClass(TColorButton);          //OK
  RegisterClass(TToggleButton);         //OK
  RegisterClass(TCheckButton);          //OK
  RegisterClass(TScrolledWindow);       //OK
  RegisterClass(TNotebookPage);
  RegisterClass(THandleBox);            //OK
  RegisterClass(TExpander);             //OK
  RegisterClass(TViewPort);             //OK
  RegisterClass(TToolItem);
  RegisterClass(TToolButton);
  RegisterClass(TSeparatorToolItem);
  RegisterClass(THPaned);               //OK
  RegisterClass(TVPaned);               //OK
  RegisterClass(TMenuBar);
  RegisterClass(TNotebook);
  RegisterClass(TToolBar);
  RegisterClass(TFixed);                //OK
  RegisterClass(TTable);                //OK
  RegisterClass(THBox);                 //OK
  RegisterClass(TVBox);                 //OK
  RegisterClass(THButtonBox);           //OK
  RegisterClass(TVButtonBox);           //OK
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
  RegisterClass(TMenuItem);
  RegisterClass(TSeparatorMenuItem);
  RegisterClass(TTearoffMenuItem);
  RegisterClass(THSeparator);           //OK
  RegisterClass(TVSeparator);           //OK
  RegisterClass(THScrollBar);           //OK
  RegisterClass(TVScrollBar);           //OK
  RegisterClass(THScale);               //OK
  RegisterClass(TVScale);               //OK
  RegisterClass(TTreeView);
//--
  RegisterClass(TFileChooserDialog);
//----------------------------------
  ResourceList := TList.Create;
  Application := TApplication.Create(nil);
  Clipboard := nil; // Created in Application.Initialize
  Primary := nil;   // Created in Application.Initialize
finalization
  Primary.Free;
  Clipboard.Free;
  Application.Free;
  ResourceList.Free;
end.
{
  $Log: xcl.pas,v $
  Revision 1.35  2006/03/04 20:17:29  judison
  Added ToolTip

  Revision 1.34  2006/03/01 20:52:34  judison
  Added TImage.Resource
  Removed TImage.StorageType

  Revision 1.33  2006/02/27 17:43:30  judison
  Added TClipboard

  Revision 1.32  2005/12/30 17:21:38  judison
  * Moved DB Comps to xcldb.inc
  * Added TCanvas

  Revision 1.31  2005/12/19 22:49:23  judison
  various changes
  added TSpinButton

  Revision 1.30  2005/12/18 07:26:25  judison
  *** empty log message ***

  Revision 1.29  2005/12/18 07:00:29  judison
  component registration

  Revision 1.28  2005/12/16 03:29:52  judison
  *** empty log message ***

  Revision 1.27  2005/12/16 02:50:45  judison
  Added TScrolledWindow.HPolicy and .VPolicy

  Revision 1.26  2005/12/13 05:18:28  judison
  Bug Fix

  Revision 1.25  2005/12/12 04:32:12  judison
  TTable Stuff

  Revision 1.24  2005/12/11 21:38:16  judison
  TImage.StorageType

  Revision 1.23  2005/12/11 11:45:30  judison
  Added TTable
  TControl.X, .Y, .Position -> .FixedX, .FixedY, .FixedPosition

  Revision 1.22  2005/12/10 17:52:01  judison
  Added ITreeSortable (Gtk Interface) and TSortType

  Revision 1.21  2005/12/10 07:48:32  judison
  *** empty log message ***

  Revision 1.20  2005/12/02 22:31:34  judison
  Long-term changes (again) I need to use cvs more!

  Revision 1.19  2005/11/18 20:06:58  judison
  long-term changes...

  Revision 1.18  2005/04/03 18:26:15  judison
  * TGStrings

  Revision 1.17  2005/03/28 05:41:23  judison
  - TEditableControl
  + GtkEditableControl Interface Implementation

  Revision 1.16  2005/03/28 02:50:35  judison
  + New Resource System

  Revision 1.15  2005/03/27 19:43:37  judison
  * QTControl -> QTObject
  + ObjectByHandle

  Revision 1.14  2005/03/27 09:53:51  judison
  * New Event System

  Revision 1.13  2005/03/27 00:27:54  judison
  + TControlActionLink, TControl.Action
  + TCustomToolitem
  + TMenuItem.Action, TToolItem.Action
  + TControl.Visible

  Revision 1.12  2005/03/26 05:45:48  judison
  + TButton.Relief

  Revision 1.11  2005/03/26 05:21:47  judison
  + CVS Log Tag

}
