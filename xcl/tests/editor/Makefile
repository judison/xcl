# XCL Path
XCL_PATH=../..
XRC=$(XCL_PATH)/tools/xrc

OPT=-Fu$(XCL_PATH)
FPC=ppc386

all: editor

editor: *.pas editor_xrc.pp
	$(FPC) $(OPT) editor.pas

editor_xrc.pp: main.frm
	$(XRC) main.frm $@

clean:
	rm -f *~ *.ppu *.xrc *_xrc.pp *.o ppas.sh core editor

run: editor
	./editor