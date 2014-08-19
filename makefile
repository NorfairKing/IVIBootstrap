all:
	ghc -Wall --make Bootstrap.hs -o ../bootstrap.bin -XOverloadedStrings

install:
	cabal install --only-dependencies

clean:
	rm -f *.o *.hi
