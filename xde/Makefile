XCL_PATH=../xcl
XRC_PATH=$(XCL_PATH)/tools

XRC=$(XRC_PATH)/xrc

OPT=-Fu$(XCL_PATH) -Fu./sourceview -Fu./icons
FPC=ppc386

PAS_FILES =	buffer.pas \
		txtbuffer.pas \
                pasbuffer.pas \
                frmbuffer.pas \
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
		xde.pas


FRM_FILES =     compiler_opts.frm \
		editor_opts.frm \
		frm_newfile.frm \
		frmbuffer.frm \
		txtbuffer.frm \
		main.frm

all: xde

xde: sourceview/xclsourceview.ppu $(PAS_FILES) xde_xrc.pp icons/icons_xrc.pp
	make -C $(XCL_PATH)
	$(FPC) $(OPT) xde.pas

icons/icons_xrc.pp:
	make -C icons

$(XRC):
	make -C $(XRC_PATH)

xde_xrc.pp: $(FRM_FILES) license.str $(XRC)
	$(XRC) $(FRM_FILES) license.str $@

sourceview/xclsourceview.ppu:
	make -C sourceview

clean:
	rm -f *~ *.ppu *.xrc *_xrc.pp *.o ppas.sh core xde out.frm
	make -C sourceview clean
	make -C icons clean

run: all
	./xde