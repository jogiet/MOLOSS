OCAMLBUILD = ocamlbuild -classic-display \
			-use-menhir \
			-libs unix	

TARGET = native

test:
	$(OCAMLBUILD) test.$(TARGET) 

clean:
	rm -rf _build/
	rm *.native

realclear:
	rm *~
