XCL_PATH=..
OPT=-Fu$(XCL_PATH)
FPC=ppc386
XRC=./xrc

all: xrc gtkpas

xrc: xrc.pas
	$(FPC) $(OPT) xrc.pas

gtkpas: gtkpas.pas
	$(FPC) $(OPT) gtkpas.pas

clean:
	rm -f *~ *.ppu *.o *.xrc ppas.sh core xrc gtkpas
