OCB=ocamlbuild -use-ocamlfind -cflag -g

SRC=ml_parser_generator.ml \
	grammar_parser.ml \
	ll_parser.ml \
	list_utils.ml	\
	grammar.ml \
	grammar_lexer.mll \
	grammar_parser_yacc.mly

TG = alpaga

all: $(TG)

$(TG): ml_parser_generator.native
	cp ml_parser_generator.native $(TG)

ml_parser_generator.native: $(SRC)
	$(OCB) ml_parser_generator.native

clean:
	rm -f ml_parser_generator.native $(TG)
	rm -rf _build
