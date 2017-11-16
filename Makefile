.PHONY: all clean native byte profile debug various_tests doc_html doc_man doc_tex doc_texinfo doc_dot sanity

OCBFLAGS = -I src -I tests
OCB = ocamlbuild -use-ocamlfind $(OCBFLAGS)

all:  native byte

clean:
	$(OCB) -clean

# build executable
native: sanity
	$(OCB) src/moloss.native

byte: sanity
	$(OCB) src/moloss.byte

profile: sanity
	$(OCB) -tag profile src/moloss.native

debug: sanity
	$(OCB) -tag debug src/moloss.byte

custom: sanity
	$(OCB) -tag custom src/moloss.byte

# tests
various_tests: native
	$(OCB) tests/various_tests.native

# documentation
doc_html: doc/moloss.odocl
	$(OCB) doc/moloss.docdir/index.html

doc_man: doc/moloss.odocl
	$(OCB) doc/moloss.docdir/man

doc_tex: doc/moloss.odocl
	$(OCB) doc/moloss.docdir/moloss.tex

doc_texinfo: doc/moloss.odocl
	$(OCB) doc/moloss.docdir/moloss.texi

doc_dot: doc/moloss.odocl
	$(OCB) doc/moloss.docdir/moloss.dot

doc: doc_html doc_man doc_tex doc_texinfo doc_dot

# check if packages are available
sanity:
	ocamlfind query unix
	ocamlfind query minisat
	ocamlfind query msat
