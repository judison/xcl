# XCL Path
XCL_PATH=../..
XRC=$(XCL_PATH)/tools/xrc

OPT=-Fu$(XCL_PATH)
FPC=fpc

all: dbtest.o

dbtest.o: *.pas main.frm
	$(XRC) main.frm dbtest_xrc.pp
	$(FPC) $(OPT) dbtest.pas

clean:
	rm -f *~ *.ppu *.exe *_xrc.pp *.o ppas.sh core dbtest

run: dbtest.o
	./dbtest
