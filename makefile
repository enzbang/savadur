
all: build

build:
	gnatmake -P savadur

regtests:
	make -C test regtests

clean:
	gnatclean -P savadur