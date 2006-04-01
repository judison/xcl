# XCL Path
XCL_PATH=..
EXTRA_PATH=$(XCL_PATH)/extra

XRC=$(XCL_PATH)/tools/xrc

OPT=-Fu$(XCL_PATH) -Fu$(EXTRA_PATH)
FPC=fpc

all: test1.o testtv.o
	make -C editor
	make -C db

test1.o: test1.pas test1_xrc.pp
	$(FPC) $(OPT) test1.pas

testtv.o: testtv.pas testtv_xrc.pp
	$(FPC) $(OPT) testtv.pas

test1_xrc.pp: test1.frm
	$(XRC) test1.frm $@

testtv_xrc.pp: testtv.frm
	$(XRC) testtv.frm $@

clean:
	rm -f *~ *.ppu *.o *.exe *_xrc.pp ppas.sh core test1 testtv
	make -C editor clean
	make -C db clean

run: test1.o
	./test1