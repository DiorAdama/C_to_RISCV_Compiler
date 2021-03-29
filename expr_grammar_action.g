tokens SYM_EOF SYM_IDENTIFIER<string> SYM_INTEGER<int> SYM_PLUS SYM_MINUS SYM_ASTERISK SYM_DIV SYM_MOD
tokens SYM_LPARENTHESIS SYM_RPARENTHESIS SYM_LBRACE SYM_RBRACE
tokens SYM_ASSIGN SYM_SEMICOLON SYM_RETURN SYM_IF SYM_WHILE SYM_ELSE SYM_COMMA 
tokens SYM_EQUALITY SYM_NOTEQ SYM_LT SYM_LEQ SYM_GT SYM_GEQ 
tokens SYM_VOID SYM_INT SYM_CHAR
tokens SYM_CHARACTER<char>
non-terminals S INSTR INSTRS LINSTRS ELSE EXPR FACTOR REST_IDENTIFIER_INSTR REST_IDENTIFIER_EXPR
non-terminals LPARAMS REST_PARAMS
non-terminals IDENTIFIER INTEGER
non-terminals FUNDEF FUNDEFS
non-terminals FUNCALL_LPARAMS FUNCALL_REST_PARAMS
non-terminals ADD_EXPRS ADD_EXPR
non-terminals MUL_EXPRS MUL_EXPR
non-terminals CMP_EXPRS CMP_EXPR
non-terminals EQ_EXPRS EQ_EXPR
non-terminals FUNC_DEF DATA_DEF REST_IDENTIFIER_ASSIGN
non-terminals CHARACTER
axiom S
{

  open Symbols
  open Ast
  open BatPrintf
  open BatBuffer
  open Batteries
  open Utils

  
  let resolve_associativity term other =
    let f_fold a expri = 
      match expri with
        | Node(tagi, childreni) -> Node(tagi, a::childreni)
        | leaf -> failwith "operator not found"
    in
    List.fold_left f_fold term other


  let resolve_identifier ident subtree  = 
    match subtree with 
      | NullLeaf -> ident 
      | Node (Targs, argums) -> Node (Tcall, [ident; subtree]) 
      | _ -> Node (Tassign, [Node (Tassignvar, [ident; subtree])])

}

rules
S -> FUNDEFS SYM_EOF {  Node (Tlistglobdef, $1) }

IDENTIFIER -> SYM_IDENTIFIER    { StringLeaf($1) }
INTEGER -> SYM_INTEGER     { IntLeaf($1) }
CHARACTER -> SYM_CHARACTER {CharLeaf($1)}

DATA_DEF -> SYM_INT IDENTIFIER {Node(Tint, [$2])}
DATA_DEF -> SYM_CHAR IDENTIFIER {Node(Tchar, [$2])}

FUNC_DEF -> DATA_DEF {$1}
FUNC_DEF -> SYM_VOID IDENTIFIER {Node (Tvoid, [$2])}

FUNDEFS -> FUNDEF FUNDEFS   { $1::$2 }
FUNDEFS -> { [] }

FUNDEF -> FUNC_DEF SYM_LPARENTHESIS LPARAMS SYM_RPARENTHESIS INSTR   
          { Node (Tfundef, [$1] @ [Node (Tfunargs, $3)] @ [Node (Tfunbody, [$5])] ) }

LPARAMS -> DATA_DEF REST_PARAMS  { Node(Targ, [$1])::$2 }
LPARAMS -> SYM_VOID {[Node (Tvoid, [NullLeaf])]}
LPARAMS -> { [] }

REST_PARAMS -> SYM_COMMA DATA_DEF REST_PARAMS   { Node(Targ, [$2])::$3 }
REST_PARAMS -> { [] }

FUNCALL_LPARAMS -> EXPR FUNCALL_REST_PARAMS  { $1::$2 }
FUNCALL_LPARAMS -> { [] }

FUNCALL_REST_PARAMS -> SYM_COMMA EXPR FUNCALL_REST_PARAMS   { $2::$3 }
FUNCALL_REST_PARAMS -> { [] }


INSTR -> SYM_IF SYM_LPARENTHESIS EXPR SYM_RPARENTHESIS LINSTRS ELSE  { Node (Tif, ([$3] @ [$5] @ $6)) }
INSTR -> SYM_WHILE SYM_LPARENTHESIS EXPR SYM_RPARENTHESIS INSTR {Node (Twhile, ([$3] @ [$5]) )}
INSTR -> SYM_RETURN EXPR SYM_SEMICOLON  { Node (Treturn, [$2]) }
INSTR -> IDENTIFIER REST_IDENTIFIER_INSTR SYM_SEMICOLON { resolve_identifier $1 $2 }
INSTR -> DATA_DEF REST_IDENTIFIER_ASSIGN SYM_SEMICOLON {resolve_identifier $1 $2}
INSTR -> SYM_VOID IDENTIFIER SYM_SEMICOLON {Node(Tvoid, [$2])}
INSTR -> LINSTRS { $1 }

LINSTRS -> SYM_LBRACE INSTRS SYM_RBRACE   { Node (Tblock, $2) }

INSTRS -> INSTR INSTRS  { $1::$2 } 
INSTRS -> { [] } 

ELSE -> SYM_ELSE LINSTRS { [$2] }
ELSE -> { [] }

REST_IDENTIFIER_ASSIGN -> SYM_ASSIGN EXPR  { $2 } 
REST_IDENTIFIER_ASSIGN -> {NullLeaf}

REST_IDENTIFIER_INSTR -> SYM_LPARENTHESIS FUNCALL_LPARAMS SYM_RPARENTHESIS { Node(Targs, $2) } 
REST_IDENTIFIER_INSTR -> SYM_ASSIGN EXPR  { $2 } 

EXPR -> EQ_EXPR EQ_EXPRS  { resolve_associativity $1 $2 }
EXPR -> CHARACTER {$1}

ADD_EXPR -> MUL_EXPR MUL_EXPRS   { resolve_associativity $1 $2 }
ADD_EXPR -> SYM_MINUS MUL_EXPR MUL_EXPRS  { resolve_associativity (Node ( Tneg, [$2])) $3 }

ADD_EXPRS -> SYM_PLUS ADD_EXPR ADD_EXPRS  { Node(Tadd, [$2])::$3 }
ADD_EXPRS -> SYM_MINUS ADD_EXPR ADD_EXPRS { Node(Tsub, [$2])::$3 }
ADD_EXPRS -> { [] }

MUL_EXPR -> FACTOR { $1 }

FACTOR -> INTEGER {$1}
FACTOR -> IDENTIFIER REST_IDENTIFIER_EXPR { resolve_identifier $1 $2 }
FACTOR -> SYM_LPARENTHESIS EXPR SYM_RPARENTHESIS   {$2}

REST_IDENTIFIER_EXPR -> SYM_LPARENTHESIS FUNCALL_LPARAMS SYM_RPARENTHESIS { Node(Targs, $2) } 
REST_IDENTIFIER_EXPR -> { NullLeaf }


MUL_EXPRS -> SYM_ASTERISK MUL_EXPR MUL_EXPRS  { (Node (Tmul, [$2]))::$3 }
MUL_EXPRS -> SYM_DIV MUL_EXPR MUL_EXPRS     { (Node (Tdiv, [$2]))::$3 }
MUL_EXPRS -> SYM_MOD MUL_EXPR MUL_EXPRS     { (Node (Tmod, [$2]))::$3 }
MUL_EXPRS -> { [] }

CMP_EXPR -> ADD_EXPR ADD_EXPRS { resolve_associativity $1 $2 }

CMP_EXPRS -> SYM_GT CMP_EXPR CMP_EXPRS  { (Node (Tcgt, [$2]))::$3 }
CMP_EXPRS -> SYM_GEQ CMP_EXPR CMP_EXPRS { (Node (Tcge, [$2]))::$3 }
CMP_EXPRS -> SYM_LT CMP_EXPR CMP_EXPRS  { (Node (Tclt, [$2]))::$3 }
CMP_EXPRS -> SYM_LEQ CMP_EXPR CMP_EXPRS { (Node (Tcle, [$2]))::$3 }
CMP_EXPRS -> { [] }

EQ_EXPR -> CMP_EXPR CMP_EXPRS { resolve_associativity $1 $2 }

EQ_EXPRS -> SYM_EQUALITY EQ_EXPR EQ_EXPRS { (Node (Tceq, [$2]))::$3 }
EQ_EXPRS -> SYM_NOTEQ EQ_EXPR EQ_EXPRS    { (Node (Tne, [$2]))::$3 }
EQ_EXPRS -> { [] }














