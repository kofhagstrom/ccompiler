compile:
	stack build
	stack run $(cfile) > test/compilation/assembly.s
	as test/compilation/assembly.s -o test/compilation/assembly.o
	clang -arch arm64 -o test/compilation/main test/compilation/assembly.o

lex:
	stack build
	stack run $(cfile) lex

parse:
	stack build
	stack run $(cfile) parse

assemble:
	stack build
	stack run $(cfile) assemble
llvm:
	stack build
	stack run $(cfile) llvm
make run:
	stack build
	stack run $(cfile) > test/compilation/assembly.s
	as test/compilation/assembly.s -o test/compilation/assembly.o
	clang -arch arm64 -o test/compilation/main test/compilation/assembly.o
	./test/compilation/main

make real:
	gcc -S -O3 -fno-asynchronous-unwind -tables test/compilation/testingfile.c 