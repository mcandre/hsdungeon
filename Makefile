EXECUTABLE=rogue

all: rogue.hs dungeon.hs
	ghc --make -o $(EXECUTABLE) rogue.hs

clean:
	rm $(EXECUTABLE)
	rm *.o
	rm *.hi