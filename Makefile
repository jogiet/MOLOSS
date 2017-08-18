OCB = ocamlbuild\
	  		-classic-display \
			-use-menhir \
			-libs unix \
			-package msat \
			-package minisat

TARGET = native

all:  main

tst:
	$(OCB) src/test.$(TARGET)
	$(OCB) src/printformula.$(TARGET)

main:
	$(OCB) src/main.$(TARGET)
	mv main.native moloss

doc:
	$(OCB) -I _build/ src/solve.docdir/dep.dot
	$(OCB) -I _build/ src/solve.docdir/index.html
	ln -f -s solve.docdir/index.html

graph:
	dot -Tpdf solve.docdir/dep.dot -o dep.pdf

clean:
	rm -rf _build/
	rm -f *.pdf
	rm -f moloss direct
	rm -f *.native
	rm -f *.html
	rm -f *.sh
	rm -rf solve.docdir
	rm -rf *~

realclear:
	rm -f *~
