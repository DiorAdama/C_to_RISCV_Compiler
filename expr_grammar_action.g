tokens SYM_EOF SYM_IDENTIFIER<string> SYM_INTEGER<int> SYM_PLUS SYM_MINUS SYM_ASTERISK SYM_DIV SYM_MOD
tokens SYM_LPARENTHESIS SYM_RPARENTHESIS SYM_LBRACE SYM_RBRACE
tokens SYM_ASSIGN SYM_SEMICOLON SYM_RETURN SYM_IF SYM_WHILE SYM_ELSE SYM_COMMA 
tokens SYM_EQUALITY SYM_NOTEQ SYM_LT SYM_LEQ SYM_GT SYM_GEQ 
tokens SYM_VOID SYM_INT SYM_CHAR
tokens SYM_CHARACTER<char>
tokens SYM_AMPERSAND 
tokens SYM_STRUCT SYM_POINT
non-terminals S INSTR INSTRS LINSTRS ELSE EXPR FACTOR REST_IDENTIFIER_INSTR REST_IDENTIFIER_EXPR
non-terminals LPARAMS REST_PARAMS
non-terminals IDENTIFIER INTEGER
non-terminals GLOBDEFS GLOBDEF
non-terminals FUNDEF STRUCTDEF
non-terminals FUNCALL_LPARAMS FUNCALL_REST_PARAMS
non-terminals ADD_EXPRS ADD_EXPR
non-terminals MUL_EXPRS MUL_EXPR
non-terminals CMP_EXPRS CMP_EXPR
non-terminals EQ_EXPRS EQ_EXPR
non-terminals FUN_DECL DATA_DECL REST_IDENTIFIER_ASSIGN FUN_DEF_OR_DECL
non-terminals CHARACTER 
non-terminals REST_TYPE
non-terminals DATA_DECLS VOID_OR_POINTER
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


  let resolve_identifier ident subtree = 
    match subtree with 
      | NullLeaf -> ident 
      | Node (Targs, argums) -> Node (Tcall, [ident; subtree]) 
      | StringLeaf structfield -> Node( Tstructdata, [ident; subtree])
      | Node(Tstructdata, [field; ex]) -> Node (Tassign, [Node (Tassignvar, [Node(Tstructdata ,[ident; field]); ex])])
      | _ -> Node (Tassign, [Node (Tassignvar, [ident; subtree])])

  let rec resolve_ptr data_type ident is_declaration = 
    match data_type, ident with 
      | Node(Tstruct, [structname]), [StringLeaf s] when is_declaration -> Node(Tstruct, structname::ident)
      | Node(data_type, []), [StringLeaf s] when is_declaration -> Node(data_type, ident) 
      | _ , [StringLeaf s] when not is_declaration -> StringLeaf s
      | _ , Node(Tptr, [])::tl when is_declaration -> Node(Tptr, [resolve_ptr data_type tl is_declaration])
      | _ , Node(Tptr, [])::tl when not is_declaration -> Node(Tvalueat, [resolve_ptr data_type tl is_declaration])
      | _ -> assert false

}

rules
S -> GLOBDEFS SYM_EOF {  Node (Tlistglobdef, $1) }

IDENTIFIER -> SYM_IDENTIFIER    { StringLeaf($1) }
INTEGER -> SYM_INTEGER     { IntLeaf($1) }
CHARACTER -> SYM_CHARACTER {CharLeaf($1)}

DATA_DECLS -> DATA_DECL SYM_SEMICOLON DATA_DECLS {$1::$3}
DATA_DECLS -> { [] }

DATA_DECL -> SYM_INT REST_TYPE { resolve_ptr (Node(Tint,[])) $2 true}
DATA_DECL -> SYM_CHAR REST_TYPE { resolve_ptr (Node(Tchar,[])) $2 true}
DATA_DECL -> SYM_VOID SYM_ASTERISK REST_TYPE { Node(Tptr, [resolve_ptr (Node(Tvoid, [])) $3 true])}
DATA_DECL -> SYM_STRUCT IDENTIFIER REST_TYPE {resolve_ptr (Node(Tstruct, [$2])) $3 true}

REST_TYPE -> IDENTIFIER {[$1]}
REST_TYPE -> SYM_ASTERISK REST_TYPE {Node(Tptr, [])::$2}



GLOBDEFS -> GLOBDEF GLOBDEFS   { $1::$2 }
GLOBDEFS -> { [] }

GLOBDEF -> FUNDEF {$1}
GLOBDEF -> STRUCTDEF {$1}


STRUCTDEF -> SYM_STRUCT IDENTIFIER SYM_LBRACE DATA_DECLS SYM_RBRACE SYM_SEMICOLON 
              { Node(Tstruct, [$2; Node(Tstructfields, $4)]) }

FUN_DECL -> SYM_INT REST_TYPE { resolve_ptr (Node(Tint,[])) $2 true}
FUN_DECL -> SYM_CHAR REST_TYPE { resolve_ptr (Node(Tchar,[])) $2 true}
FUN_DECL -> SYM_VOID REST_TYPE { Node(Tptr, [resolve_ptr (Node(Tvoid,[])) $2 true]) }

FUNDEF -> FUN_DECL SYM_LPARENTHESIS LPARAMS SYM_RPARENTHESIS FUN_DEF_OR_DECL   
          { Node (Tfundef, [$1] @ [Node (Tfunargs, $3)] @ [Node (Tfunbody, $5)] ) }

FUN_DEF_OR_DECL -> INSTR {[$1]}
FUN_DEF_OR_DECL -> SYM_SEMICOLON {[]}

VOID_OR_POINTER -> REST_TYPE REST_PARAMS { Node(Tptr, [resolve_ptr (Node(Tvoid,[])) $1 true])::$2 } 
VOID_OR_POINTER -> {[Node (Tvoid, [NullLeaf])]}

LPARAMS -> SYM_INT REST_TYPE REST_PARAMS    { (resolve_ptr (Node(Tint, [])) $2 true)::$3 }
LPARAMS -> SYM_CHAR REST_TYPE REST_PARAMS   { (resolve_ptr (Node(Tchar,[])) $2 true)::$3 }
LPARAMS -> SYM_STRUCT IDENTIFIER REST_TYPE REST_PARAMS{ (resolve_ptr (Node(Tstruct, [$2])) $3 true)::$4 }
LPARAMS -> SYM_VOID VOID_OR_POINTER {$2}
LPARAMS -> { [] }

REST_PARAMS -> SYM_COMMA DATA_DECL REST_PARAMS   { Node(Targ, [$2])::$3 }
REST_PARAMS -> { [] }

FUNCALL_LPARAMS -> EXPR FUNCALL_REST_PARAMS  { $1::$2 }
FUNCALL_LPARAMS -> { [] }

FUNCALL_REST_PARAMS -> SYM_COMMA EXPR FUNCALL_REST_PARAMS   { $2::$3 }
FUNCALL_REST_PARAMS -> { [] }


INSTR -> SYM_IF SYM_LPARENTHESIS EXPR SYM_RPARENTHESIS LINSTRS ELSE  { Node (Tif, ([$3] @ [$5] @ $6)) }
INSTR -> SYM_WHILE SYM_LPARENTHESIS EXPR SYM_RPARENTHESIS INSTR {Node (Twhile, ([$3] @ [$5]) )}
INSTR -> SYM_RETURN EXPR SYM_SEMICOLON  { Node (Treturn, [$2]) }
INSTR -> IDENTIFIER REST_IDENTIFIER_INSTR SYM_SEMICOLON { resolve_identifier $1 $2 }
INSTR -> DATA_DECL REST_IDENTIFIER_ASSIGN SYM_SEMICOLON {resolve_identifier $1 $2}
INSTR -> SYM_ASTERISK REST_TYPE SYM_ASSIGN EXPR SYM_SEMICOLON 
          { Node(Tassign, [Node(Tassignvar, [Node(Tvalueat, [resolve_ptr (Node(Tvoid,[])) $2 false]); $4])])} 
INSTR -> LINSTRS { $1 }

LINSTRS -> SYM_LBRACE INSTRS SYM_RBRACE   { Node (Tblock, $2) }

INSTRS -> INSTR INSTRS  { $1::$2 } 
INSTRS -> { [] } 

ELSE -> SYM_ELSE LINSTRS { [$2] }
ELSE -> { [] }

REST_IDENTIFIER_ASSIGN -> SYM_ASSIGN EXPR  { $2 } 
REST_IDENTIFIER_ASSIGN -> {NullLeaf}

REST_IDENTIFIER_INSTR -> SYM_LPARENTHESIS FUNCALL_LPARAMS SYM_RPARENTHESIS { Node(Targs, $2) } 
REST_IDENTIFIER_INSTR -> SYM_POINT IDENTIFIER SYM_ASSIGN EXPR { Node(Tstructdata, [$2; $4]) }
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
FACTOR -> SYM_AMPERSAND IDENTIFIER { Node(Taddrof, [$2]) }
FACTOR -> SYM_ASTERISK IDENTIFIER  {Node(Tvalueat, [$2])}
FACTOR -> SYM_LPARENTHESIS EXPR SYM_RPARENTHESIS   {$2}

REST_IDENTIFIER_EXPR -> SYM_LPARENTHESIS FUNCALL_LPARAMS SYM_RPARENTHESIS { Node(Targs, $2) } 
REST_IDENTIFIER_EXPR -> SYM_POINT IDENTIFIER {$2}
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














