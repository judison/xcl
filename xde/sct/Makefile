# $Id: Makefile,v 1.2 2005/12/27 03:14:10 judison Exp $

#=============
# Scintilla
#=============
SCINTILLA_PATH=../../scintilla

# XCL Path
XCL_PATH=../../xcl

XRC=$(XCL_PATH)/tools/xrc

OPT=-Fu$(XCL_PATH) -Xs-
FPC=ppc386


all: scintilla.ppu

scintilla.pas: scintilla.tmpl Scintilla.iface scigen.py
	python scigen.py

scintilla.ppu: scintilla.pas
	$(FPC) $(OPT) scintilla.pas

clean:
	rm -f *~ *.ppu *.o *.xrc ppas.sh core scintilla.pas
