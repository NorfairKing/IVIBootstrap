all:
	ghc -Wall --make Bootstrap.hs -o ../bootstrap.bin -XOverloadedStrings

clean:
	rm -f *.o *.hi
