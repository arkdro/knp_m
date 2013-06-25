OASISFILES = \
			 _build \
			 myocamlbuild.ml \
			 $(wildcard setup.log*) \
			 $(wildcard setup.ml*) \
			 $(wildcard _tags.*)

all:
	ocaml setup.ml -build -classic-display

profile:
	## fucked ocaml
	ocaml setup.ml -build -classic-display -tag profile

boot:
	oasis setup-dev
	ocaml setup.ml -configure
	$(MAKE)

clean:
	ocaml setup.ml -clean -classic-display

distclean:
	-ocaml setup.ml -distclean -classic-display

mrproper: distclean
	rm -rf $(OASISFILES)
