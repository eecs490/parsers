.PHONY: all clean

MENHIR          := menhir

MENHIRFLAGS     := --infer

OCAMLBUILD      := ocamlbuild -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)"

MAIN            := run

all:
	$(OCAMLBUILD) $(MAIN).native

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean

