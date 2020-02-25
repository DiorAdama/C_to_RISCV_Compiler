tokens SYM_EOF SYM_IDENTIFIER<string> SYM_INTEGER<int> SYM_PLUS SYM_MINUS SYM_ASTERISK SYM_DIV SYM_MOD
tokens SYM_LPARENTHESIS SYM_RPARENTHESIS SYM_LBRACE SYM_RBRACE
tokens SYM_ASSIGN SYM_SEMICOLON SYM_RETURN SYM_IF SYM_WHILE SYM_ELSE SYM_COMMA SYM_PRINT
tokens SYM_EQUALITY SYM_NOTEQ SYM_LT SYM_LEQ SYM_GT SYM_GEQ
non-terminals S INSTR INSTRS LINSTRS ELSE EXPR FACTOR
non-terminals LPARAMS REST_PARAMS
non-terminals IDENTIFIER INTEGER
non-terminals GLOBDEF
non-terminals ADD_EXPRS ADD_EXPR
non-terminals MUL_EXPRS MUL_EXPR
non-terminals CMP_EXPRS CMP_EXPR
non-terminals REQ_EXPRS REQ_EXPR
axiom S
{

  open Symbols
  open Utils
  open Ast
  open BatPrintf
  open BatBuffer
  open Batteries

  (* TODO *)
  let resolve_associativity term other =
       (* TODO *)
    term


}

rules
S -> GLOBDEF SYM_EOF {  Node (Tlistglobdef, [$1]) }
IDENTIFIER -> SYM_IDENTIFIER {  StringLeaf ($1) }
INTEGER -> SYM_INTEGER { IntLeaf ($1) }
GLOBDEF -> IDENTIFIER SYM_LPARENTHESIS LPARAMS SYM_RPARENTHESIS INSTR {
    let fargs = $3 in
    let instr = $5 in
    Node (Tfundef, [$1; Node (Tfunargs, fargs) ; instr ])
}
