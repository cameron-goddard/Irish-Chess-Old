.PHONY: test

build:
	dune build
	
utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe
	
run:
	OCAMLRUNPARAM=b dune exec bin/main.exe default.json

cli:
	OCAMLRUNPARAM=b dune exec bin/main.exe "cli" default.json

gui: 
	OCAMLRUNPARAM=b dune exec bin/main.exe "gui" default.json

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

zip:
	rm -rf chess.zip
	zip -r chess.zip . -x@exclude.lst

clean:
	dune clean
	rm -f chess.zip