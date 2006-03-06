# XCL Path
XCL_PATH=../..
XRC=$(XCL_PATH)/tools/xrc

OPT=-Fu$(XCL_PATH)
FPC=ppc386

all: dbtest

dbtest: *.pas main.frm
	$(XRC) main.frm dbtest_xrc.pp
	$(FPC) $(OPT) dbtest.pas

clean:
	rm -f *~ *.ppu *.xrc *_xrc.pp *.o ppas.sh core dbtest

run: dbtest
	./dbtest
