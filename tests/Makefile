# if make is launched with a DIR variable, pass it as the -f option to test.py
# 'make DIR=basic/mul*.e' launches all the files starting with mul in the basic directory
# otherwise, use basic/*.e as a default
FILES := $(if $(DIR),$(DIR),basic/*.e)

OPTS := $(if $(OPTS), $(OPTS),)

.PHONY: all
all: ../main.native
	./test.py -f $(FILES) $(OPTS)

expect: ../main.native
	OCAMLRUNPARAM=b ./test.py --make-expect -f $(FILES) $(OPTS) --args 1 2 3
	OCAMLRUNPARAM=b ./test.py --make-expect -f $(FILES) $(OPTS) --args 14 12 3 8 12
	for f in $(FILES); do ../main.native -f $$f -show-tokens $$f.expect_lexer; done


clean:
	find . -name '*.rig' -or -name '*.ltl' -or -name '*.cfg' -or -name '*.exe' -or -name '*.s' -or -name '*.rtl' -or -name '*.ast' -or -name '*.cfg0' -or -name '*.cfg1' -or -name '*.cfg2' -or -name '*.cfg3' -or -name '*.riscv' -or -name '*.dump' -or	-name '*.linear1' -or -name '*.linear' -or -name '*.html' -or -name '*.svg'	-or -name '*.html' -or -name '*.lex' -or -name "*.json" | xargs -I{} rm {}
	rm -rf __pycache__
