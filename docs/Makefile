# XCL Path
XCL_PATH=../xcl
XMKSKEL=$(XCL_PATH)/tools/xmkskel
OPT=--package="xcl" --disable-private --disable-protected --update
FPDOC=fpdoc

all: xcl.xml xcldb.xml

xcl.xml: $(XCL_PATH)/xcl.pas $(XCL_PATH)/*.inc
	$(XMKSKEL) $(OPT) --input="-Fi../xcl ../xcl/xcl.pas" --output="xcl.xml"

xcldb.xml: $(XCL_PATH)/xcldb.pas $(XCL_PATH)/*.inc
	$(XMKSKEL) $(OPT) --input="-Fi../xcl ../xcl/xcldb.pas" --output="xcldb.xml"

html: html/index.html

html/index.html: xcl.xml xcldb.xml
	$(FPDOC) --input="-Fi../xcl/ ../xcl/xcl.pas" --descr="xcl.xml" --input="-Fi../xcl/ ../xcl/xcldb.pas" --descr="xcldb.xml" --package=xcl --format=html --output=html

clean:
	rm -fr *~ core html/*.html html/xcl html/xcldb