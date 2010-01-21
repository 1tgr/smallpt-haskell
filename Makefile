all: test

build:
	mkdir -p bin obj
	ghc --make -Wall -o bin/global-illum -outputdir obj *.hs

clean:
	rm -r bin obj

test: build
	bin/global-illum > bin/global-illum.ppm
