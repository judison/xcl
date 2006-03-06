OPT=
FPC=ppc386

all:
	make -C xcl
	make -C xde

clean:
	rm -f *~
	make -C xcl clean
	make -C xde clean
