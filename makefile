default:
	ghc --make gol.hs
	rm *.hi *.o
clean:
	rm gol
