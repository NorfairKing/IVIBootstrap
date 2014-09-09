
GHC         = ghc
GHC_FLAGS   =   -Wall \
                --make

MAIN_SRC    = Bootstrap.hs

EXECUTABLE_NAME     = bootstrap.bin
EXECUTABLE_DIR      = ..
EXECUTABLE          = $(EXECUTABLE_DIR)/$(EXECUTABLE_NAME)

all:
	$(GHC) $(GHC_FLAGS) $(MAIN_SRC) -o $(EXECUTABLE)

install:
	cabal install --only-dependencies

clean:
	rm -f *.o *.hi
