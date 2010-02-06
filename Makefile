all: test

build:
	mkdir -p bin obj
	hlint *.hs
	ghc --make -Wall -O -threaded -o bin/smallpt -outputdir obj Main.hs

clean:
	rm -r bin obj

bin/image.png: bin/smallpt
	cd bin
	time ./smallpt +RTS -N8

test: build bin/image.png
