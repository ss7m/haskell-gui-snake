all:
	ghc -o Snake --make main.hs

clean:
	rm *.o *.hi *.exe
