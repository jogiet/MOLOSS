OCAMLBUILD = ocamlbuild -classic-display \
			-use-menhir \
			-libs unix	

TARGET = native

test:
	$(OCAMLBUILD) test.$(TARGET) 
	$(OCAMLBUILD) solve.$(TARGET) 

clean:
	rm -rf _build/
	rm *.native

clear:
	rm *~
