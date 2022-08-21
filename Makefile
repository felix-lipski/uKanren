scf4: exe
	./exe

exe: main.hs
	ghc -o exe main.hs
