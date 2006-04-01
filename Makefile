OPT=
FPC=fpc

all:
	make -C xcl
	make -C xde

run:
	make -C xde run

clean:
	rm -f *~
	make -C xcl clean
	make -C xde clean
