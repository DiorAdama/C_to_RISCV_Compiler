# if make is launched with a DIR variable, pass it as the -f option to test.py
# 'make DIR=basic/mul*.e' launches all the files starting with mul in the basic directory
# otherwise, use basic/*.e as a default
OPTS := $(if $(DIR),-f $(DIR),-f basic/*.e)

.PHONY: all
all: ../main.native
	./test.py $(OPTS)

clean:
	find . -name *.ltl -or -name *.cfg -or -name *.exe -or -name *.s -or -name		\
	*.rtl -or -name *.ast -or -name *.cfg0 -or -name *.cfg1 -or -name *.cfg2 -or	\
	-name *.riscv -or -name *.dump -or -name *.linear1 -or -name *.linear -or			\
	-name *.html -or -name *.svg -or -name *.html -or -name *.lex | xargs -I{} rm	\
	{}
	rm -rf __pycache__
