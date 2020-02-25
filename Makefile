all: main.native

.PHONY: main.native
main.native:
	make -C alpaga
	./alpaga/alpaga \
			-g expr_grammar_action.g \
			-pml src/generated_parser.ml \
			-t grammar.html
	./configure
	make -C src
	cp src/main.native main.native


clean:
	make -C alpaga clean
	rm -f src/generated_parser.ml
	rm -f grammar.html
	make -C src clean
	rm -f main.native
	make -C tests clean

test: main.native
	make -C tests
