XCL_PATH=../../xcl
XRC_PATH=$(XCL_PATH)/tools

XRC=$(XRC_PATH)/xrc

OPT=-Fu$(XCL_PATH) -Fu./sourceview
FPC=fpc

#	selector.xpm \
#	taccellabel.xpm \
#	taspectframe.xpm \
#	tcheckmenuitem.xpm \
#	tclock.xpm \
#	tcolorseldialog.xpm \
#	tdbcontrol.xpm \
#	tdial.xpm \
#	tdrawingarea.xpm \
#	teventbox.xpm \
#	tfilechooserdialog.xpm \
#	tfileseldialog.xpm \
#	tfontsel.xpm \
#	tfontseldialog.xpm \
#	thruler.xpm \
#	tinputdialog.xpm \
#	tlayout.xpm \
#	toptionmenu.xpm \
#	tpacker.xpm \
#	tpixbuf.xpm \
#	tradiobutton.xpm \
#	tradiomenuitem.xpm \
#	tradiotoolbutton.xpm \
#	tseparatortoolitem.xpm \
#	ttoggletoolbutton.xpm \
#	ttoolbutton.xpm \
#	tvruler.xpm \


ICON_FILES = unknown.xpm \
	tactionlist.xpm \
	tarrow.xpm \
	talignment.xpm \
	tbutton.xpm \
	tcalendar.xpm \
	tcheckbutton.xpm \
	tcolorbutton.xpm \
	tcolorselection.xpm \
	tcombobox.xpm \
	tcomboboxentry.xpm \
	tdatasource.xpm \
	tdbentry.xpm  \
	tdbf.xpm \
	tdblabel.xpm \
	tentry.xpm \
	texpander.xpm \
	tfilechooserwidget.xpm \
	tfixed.xpm \
	tfixedformatdataset.xpm \
	tfontbutton.xpm \
	tframe.xpm \
	thandlebox.xpm \
	thbox.xpm \
	thbuttonbox.xpm \
	thpaned.xpm \
	thscale.xpm \
	thscrollbar.xpm \
	thseparator.xpm \
	tibconnection.xpm \
	tibdatabase.xpm \
	tibquery.xpm \
	tibstoredproc.xpm \
	tibtransaction.xpm \
	timage.xpm \
	tlabel.xpm \
	tmemdataset.xpm \
	tmenubar.xpm \
	tmenuitem.xpm \
	tmysqlconnection.xpm \
	tnotebook.xpm \
	tpqconnection.xpm \
	tprogressbar.xpm \
	tradiobutton.xpm \
	tscrolledwindow.xpm \
	tsdfdataset.xpm \
	tspinbutton.xpm \
	tsqlquery.xpm \
	tsqltransaction.xpm \
	tstatusbar.xpm \
	ttable.xpm \
	ttextview.xpm \
	ttogglebutton.xpm \
	ttoolbar.xpm \
	ttoolitem.xpm \
	ttreeview.xpm \
	tvbox.xpm \
	tvbuttonbox.xpm \
	tviewport.xpm \
	tvpaned.xpm \
	tvscale.xpm \
	tvscrollbar.xpm \
	tvseparator.xpm

all: icons_xrc.pp

icons_xrc.pp: xde.png $(ICON_FILES)
	$(XRC) xde.png $(ICON_FILES) $@

clean:
	rm -f *~ *.ppu *.xrc *.o ppas.sh core