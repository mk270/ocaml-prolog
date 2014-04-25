default: all

all:
	ocaml setup.ml -all

clean:
	ocaml setup.ml -clean
	oasis setup-clean
