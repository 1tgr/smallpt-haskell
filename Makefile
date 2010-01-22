all: test

build:
	mkdir -p bin obj
	ghc --make -Wall -threaded -o bin/global-illum -outputdir obj *.hs

clean:
	rm -r bin obj

test: build
	time bin/global-illum +RTS -N8 > bin/global-illum.ppm
