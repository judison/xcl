XCL_PATH=../xcl
XRC_PATH=$(XCL_PATH)/tools

XRC=$(XRC_PATH)/xrc

OPT=-Fu$(XCL_PATH) -Fu$(XCL_PATH)/extra  -Fu./sourceview -Fu./icons
FPC=fpc

PAS_FILES =	exctrls.pas \
		buffer.pas \
		bufferlist.pas \
		txtbuffer.pas \
		pasbuffer.pas \
		frmbuffer.pas \
		compilermsg.pas \
		executor.pas \
		compiler.pas \
		compilingdlg.pas \
		project_opts.pas \
		compiler_opts.pas \
		editor_opts.pas \
		frm_newfile.pas \
		dbg.pas \
		jitform.pas \
		jitforms.pas \
		main.pas \
		propeditor.pas \
		componentpalette.pas \
		regcomps.pas \
		xpr.pas \
		xde.pas


FRM_FILES =     compiler_opts.frm \
		editor_opts.frm \
		project_opts.frm \
		compilingdlg.frm \
		frm_newfile.frm \
		frmbuffer.frm \
		txtbuffer.frm \
		main.frm

all: xde.o

xde.o: $(PAS_FILES) xde_xrc.pp icons/icons_xrc.pp
	$(FPC) $(OPT) xde.pas

$(XRC).o:
	make -C $(XRC_PATH)

icons/icons_xrc.pp:
	make -C icons

xde_xrc.pp: $(FRM_FILES) license.str $(XRC).o
	$(XRC) $(FRM_FILES) license.str $@

clean:
	rm -f *~ *.ppu *.xrc *_xrc.pp *.o *.exe link.res ppas.sh core xde
	make -C sourceview clean
	make -C icons clean

run: all
	./xde
