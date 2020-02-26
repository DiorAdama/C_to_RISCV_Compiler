SRC=archi.ml ast.ml builtins.ml config.ml cfg_constprop.ml cfg_dead_assign.ml		\
cfg.ml cfg_print.ml cfg_gen.ml cfg_nop_elim.ml cfg_run.ml elang.ml							\
elang_print.ml elang_gen.ml elang_run.ml e_regexp.ml generated_parser.ml 				\
lexer_generator.ml linear_dse.ml linear_liveness.ml linear.ml linear_print.ml 	\
linear_gen.ml	linear_run.ml ltl.ml ltl_print.ml ltl_gen.ml ltl_run.ml				 		\
ltl_debug.ml main.ml options.ml parser.ml prog.ml regalloc.ml report.ml 				\
riscv.ml rtl.ml rtl_print.ml rtl_gen.ml rtl_run.ml symbols.ml utils.ml

TG = main.native

all: $(TG)

$(TG): $(SRC)
	ocamlbuild -cflags -warn-error,"+a-26" -use-ocamlfind $(TG)

clean:
	rm -rf _build
	rm -f config.ml main.native
