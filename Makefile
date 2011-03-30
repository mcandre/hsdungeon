EXECUTABLE=rogue

all: rogue.hs dungeon.hs
	ghc --make -o $(EXECUTABLE) -package hscurses -package split rogue.hs

clean:
	rm $(EXECUTABLE)
	rm *.o
	rm *.hi