compile:
	stack build
	stack run $(cfile) > test/compilation/assembly.s
	as test/compilation/assembly.s -o test/compilation/assembly.o
	clang -arch arm64 -o test/compilation/main test/compilation/assembly.o
	./test/compilation/main