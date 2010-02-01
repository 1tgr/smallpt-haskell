all: test

build:
	mkdir -p bin obj
	hlint *.hs
	ghc --make -Wall -O -threaded -o bin/smallpt -outputdir obj *.hs

clean:
	rm -r bin obj

bin/smallpt.ppm: bin/smallpt
	time bin/smallpt +RTS -N8 > bin/smallpt.ppm

test: build bin/smallpt.ppm
