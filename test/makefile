all: build

build:
	gnatmake -P savadur_regtests

regtests: build
	./harness

clean:
	gnatclean -P savadur_regtests