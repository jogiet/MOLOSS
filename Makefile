OCB = ocamlbuild -classic-display \
			-use-menhir \
			-libs unix	

TARGET = native

PDFLTX = pdflatex -synctex=1 -interaction=nonstopmode

all: test report

test:
	$(OCB) test.$(TARGET) 

report: 
	cd report; pdflatex report.tex
	mv report/report.pdf ./

doc:
	$(OCB)-I _build/ solve.docdir/dep.dot
	$(OCB)-I _build/ solve.docdir/index.html
	ln -f -s solve.docdir/index.html 

graph:
	dot -Tpdf solve.docdir/dep.dot -o dep.pdf


clean:
	rm -rf _build/
	rm -f *.native
	rm -f *.html
	rm -rf solve.docdir
	cd report; rm -rf *.aux *.log *.out *.toc *.pdf

realclear:
	rm -f *~
