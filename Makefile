all: test

build:
	mkdir -p bin obj
	hlint *.hs
	ghc --make -Wall -threaded -o bin/global-illum -outputdir obj *.hs

clean:
	rm -r bin obj

bin/global-illum.ppm: bin/global-illum
	time bin/global-illum +RTS -N8 > bin/global-illum.ppm

test: build bin/global-illum.ppm
