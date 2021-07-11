build:
	dune build

examples:
	dune exec ./examples.exe

tests:
	dune exec ./tests.exe

clean:
	dune clean
