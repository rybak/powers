HC=ghc -O3
RM=rm -f

all : runTests

runTests : test
	./test

test : Test.hs Powers.hs
	$(HC) --make $^ -o $@

main : Main.hs Powers.hs
	$(HC) --make $^ -o $@

clear :
	$(RM) test main *.hi Main.o Test.o Powers.o
