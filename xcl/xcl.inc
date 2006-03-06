(*
   $Id: xcl.inc,v 1.19 2006/02/27 17:43:30 judison Exp $

   XCL - XDE's Component Library
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)


{$I gtk_signals.inc}
{$I tcolormap.inc}
{$I tglist.inc}
{$I tgstrings.inc}
//--------------------- Actions / ActionLists / ActionLinks
{$I tactionlist.inc}
{$I tcontainedaction.inc}
{$I tcustomaction.inc}
{$I tactionlink.inc}
{$I tcontrolactionlink.inc}
//--------------------- Interfaces
{$I idesigner.inc}
{$I ifilechooser.inc}
{$I ieditablecontrol.inc}
{$I icelllayout.inc}
//--------------------- Non Visual
{$I tclipboard.inc}
{$I tcanvas.inc}
{$I tpixbuf.inc}
//---------------------
{$I tapplication.inc}
{$I tcontrol.inc}
{$I tmisccontrol.inc}
{$I tarrow.inc}
{$I tlabel.inc}
{$I timage.inc}
{$I tcontainercontrol.inc}
{$I tbincontrol.inc}
{$I tform.inc}
{$I tframe.inc}
{$I tcustombutton.inc}
{$I tcolorbutton.inc}
{$I tbutton.inc}
{$I ttogglebutton.inc}
{$I tcheckbutton.inc}
{$I tadjustment.inc}
{$I tscrolledwindow.inc}
{$I tnotebookpage.inc}
{$I thandlebox.inc}
{$I texpander.inc}
{$I tviewport.inc}
{$I tcustomtoolitem.inc}
{$I ttoolitem.inc}
{$I ttoolbutton.inc}
{$I tseparatortoolitem.inc}
{$I tpaned.inc}
{$I tfixed.inc}
{$I tmenubar.inc}
{$I tnotebook.inc}
{$I ttoolbar.inc}
{$I ttable.inc}
{$I tbox.inc}
{$I tbuttonbox.inc}
{$I tentry.inc}
{$I tspinbutton.inc}
{$I ttextview.inc}
{I tcombo.inc Deprecated}
{$I tcombobox.inc}
{$I tcustommenuitem.inc}
{$I tmenuitem.inc}
{$I tseparatormenuitem.inc}
{$I ttearoffmenuitem.inc}
{$I tprogressbar.inc}
{$I tstatusbar.inc}
{$I tcalendar.inc}
{$I tcolorselection.inc}
{$I tfilechooserwidget.inc}
{$I trange.inc}
{$I tscrollbar.inc}
{$I tscale.inc}
{$I tseparator.inc}
{$I ttimer.inc}
{$I ttextbuffer.inc}
{$I tdialog.inc}
{I tfileseldialog.inc Deprecated}
{$I tfilechooserdialog.inc}
// TreeView Itens (Arrumar ordem depois)
{$I ttreeviewcolumn.inc}
{$I ttreemodel.inc}
{$I ttreestore.inc}
{$I tliststore.inc}
{$I ttreeview.inc}
//--------------------- Utils
{$I util_quark.inc}
{$I util_color.inc}
{$I util_signals.inc}
{$I util_resources.inc}

{
$Log: xcl.inc,v $
Revision 1.19  2006/02/27 17:43:30  judison
Added TClipboard

Revision 1.18  2005/12/30 17:21:38  judison
* Moved DB Comps to xcldb.inc
* Added TCanvas

Revision 1.17  2005/12/19 22:49:23  judison
various changes
added TSpinButton

Revision 1.16  2005/12/11 11:45:30  judison
Added TTable
TControl.X, .Y, .Position -> .FixedX, .FixedY, .FixedPosition

Revision 1.15  2005/12/02 22:31:34  judison
Long-term changes (again) I need to use cvs more!

Revision 1.14  2005/11/18 20:06:58  judison
long-term changes...

Revision 1.13  2005/04/03 16:50:33  judison
+ Added TGStings

Revision 1.12  2005/03/28 05:41:23  judison
- TEditableControl
+ GtkEditableControl Interface Implementation

Revision 1.11  2005/03/28 02:50:35  judison
+ New Resource System

Revision 1.10  2005/03/27 09:53:51  judison
* New Event System

Revision 1.9  2005/03/27 00:27:54  judison
+ TControlActionLink, TControl.Action
+ TCustomToolitem
+ TMenuItem.Action, TToolItem.Action
+ TControl.Visible

Revision 1.8  2005/03/26 05:21:47  judison
+ CVS Log Tag

}