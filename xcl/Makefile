OPT=
FPC=fpc

all: xcl.ppu xcldb.ppu
	make -C tools
	make -C extra
	make -C tests

xcl.ppu: xcl.pas *.inc
	$(FPC) $(OPT) xcl.pas

xcldb.ppu: xcldb.pas *.inc
	$(FPC) $(OPT) xcldb.pas

run: xcl.ppu

clean:
	rm -f *~ *.ppu *.o ppas.sh core
	make -C tools clean
	make -C extra clean
	make -C tests clean
