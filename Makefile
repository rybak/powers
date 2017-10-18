HC=ghc -O3
RM=rm -f

all : runTests main

runTests : test
	./test

test : Test.hs Powers.hs
	$(HC) --make $^ -o $@

main : Main.hs Powers.hs
	$(HC) --make $^ -o $@

run : main
	./main

clear :
	$(RM) test main *.hi Main.o Test.o Powers.o
