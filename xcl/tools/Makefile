XCL_PATH=..
OPT=-Fu$(XCL_PATH)
FPC=fpc
XRC=./xrc

all: xrc.o gtkpas.o xmkskel.o

xrc.o: xrc.pas
	$(FPC) $(OPT) xrc.pas

gtkpas.o: gtkpas.pas
	$(FPC) $(OPT) gtkpas.pas

xmkskel.o: xmkskel.pas
	$(FPC) $(OPT) xmkskel.pas

clean:
	rm -f *~ *.ppu *.o *.exe ppas.sh core xrc gtkpas xmkskel
