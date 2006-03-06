XCL_PATH=..
OPT=-Fu$(XCL_PATH)
FPC=ppc386
XRC=./xrc

all: xrc

xrc: xrc.pas
	$(FPC) $(OPT) xrc.pas

clean:
	rm -f *~ *.ppu *.o *.xrc ppas.sh core xrc
