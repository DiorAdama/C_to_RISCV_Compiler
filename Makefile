all: main.native

.PHONY: main.native

src/config.ml: configure
	./configure

main.native: src/config.ml
	make -C alpaga
	./alpaga/alpaga \
			-g expr_grammar_action.g \
			-pml src/generated_parser.ml \
			-t grammar.html
	make -C src
	ln -sf src/main.native main.native

clean:
	make -C alpaga clean
	rm -f src/generated_parser.ml
	rm -f grammar.html
	make -C src clean
	rm -f main.native
	make -C tests clean

test: main.native
	make -C tests
