# XCL Path
XCL_PATH=../../xcl

OPT=-Fu$(XCL_PATH)
FPC=fpc

all: xclsourceview.ppu

xclsourceview.ppu: xclsourceview.pas gtksourceview.pp
	$(FPC) $(OPT) xclsourceview.pas

clean:
	rm -f *~ *.ppu *.xrc *.o ppas.sh core
