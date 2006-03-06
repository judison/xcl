# $Id: Makefile,v 1.3 2005/12/23 20:43:50 judison Exp $

XCL_PATH=..
OPT=-Fu$(XCL_PATH)
FPC=ppc386
XRC=./xrc

all: xrc

xrc: xrc.pas
	$(FPC) $(OPT) xrc.pas

clean:
	rm -f *~ *.ppu *.o *.xrc ppas.sh core xrc
