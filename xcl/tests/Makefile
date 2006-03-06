# $Id: Makefile,v 1.8 2006/02/28 16:22:58 judison Exp $

# XCL Path
XCL_PATH=..

XRC=$(XCL_PATH)/tools/xrc

OPT=-Fu$(XCL_PATH)
FPC=ppc386

all: test1 testtv
	make -C editor
	make -C db

test1: test1.pas test1_xrc.pp
	$(FPC) $(OPT) test1.pas

testtv: testtv.pas testtv_xrc.pp
	$(FPC) $(OPT) testtv.pas

test1_xrc.pp: test1.frm
	$(XRC) test1.frm $@

testtv_xrc.pp: testtv.frm
	$(XRC) testtv.frm $@

clean:
	rm -f *~ *.ppu *.o *.xrc *_xrc.pp ppas.sh core test1 testtv
	make -C editor clean
	make -C db clean

run: test1
	./test1