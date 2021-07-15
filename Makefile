SRCFILES = debug/*.ml* tests/*.ml* src/*.ml src/*.mli

CINAPSFILES = src/*.cinaps tests/*.cinaps

OCAMLFORMAT = ocamlformat \
	--inplace \
	$(SRCFILES) \
	$(CINAPSFILES)

OCPINDENT = ocp-indent \
	--inplace \
	$(SRCFILES) \
	$(CINAPSFILES)

.PHONY: all
all :
	esy dune build @all

.PHONY: lib
lib :
	esy dune build src

.PHONY: test
test :
	OCAMLRUNPARAM=esy dune exec ./tests/main.exe

.PHONY: covtest
covtest :
	rm -rf _coverage
	rm -rf bisect*.coverage
	esy dune runtest --instrument-with bisect_ppx --force
	BISECT_ENABLE=yes OCAMLRUNPARAM=esy dune exec ./tests/main.exe
	esy bisect-ppx-report html

.PHONY: debug
debug :
	esy dune exec ./debug/main.exe

.PHONY: doc
doc :
	esy dune build @doc

.PHONY: format
format :
	$(OCAMLFORMAT)
	$(OCPINDENT)

.PHONY: cinaps
cinaps :
	cinaps -i $(SRCFILES)
	$(OCAMLFORMAT)
	$(OCPINDENT)

.PHONY : clean
clean:
	esy dune clean
