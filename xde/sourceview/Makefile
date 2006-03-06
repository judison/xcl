# $Id: Makefile,v 1.1 2006/02/25 19:19:16 judison Exp $

# XCL Path
XCL_PATH=../../xcl

OPT=-Fu$(XCL_PATH)
FPC=ppc386

all: xclsourceview.ppu

xclsourceview.ppu: xclsourceview.pas gtksourceview.pp
	$(FPC) $(OPT) xclsourceview.pas

clean:
	rm -f *~ *.ppu *.xrc *.o ppas.sh core
