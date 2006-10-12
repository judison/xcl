# XCL Path
XCL_PATH=..

XRC=$(XCL_PATH)/tools/xrc

OPT=-Fu$(XCL_PATH)
FPC=fpc

all: gtkspell.ppu

gtkspell.ppu: gtkspell.pas
	$(FPC) $(OPT) gtkspell.pas

clean:
	rm -f *~ *.ppu *.o *.xrc *_xrc.pp ppas.sh core
