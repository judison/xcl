all:
	make -C xcl
	make -C xde
	make -C docs

html:
	make -C docs html

clean:
	rm -f *~
	make -C xcl clean
	make -C xde clean
