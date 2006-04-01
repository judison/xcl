XCL_PATH=..
OPT=-Fu$(XCL_PATH)
FPC=ppc386
XRC=./xrc

all: xrc.o gtkpas.o

xrc.o: xrc.pas
	$(FPC) $(OPT) xrc.pas

gtkpas.o: gtkpas.pas
	$(FPC) $(OPT) gtkpas.pas

clean:
	rm -f *~ *.ppu *.o *.exe ppas.sh core xrc gtkpas
