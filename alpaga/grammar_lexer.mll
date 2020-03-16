{
open Grammar_parser_yacc
open Lexing
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let id = letter (digit|letter|'_')*

rule token = parse
  | [' ' '\t'] { token lexbuf }
  | "//" { comment lexbuf }
  | "/*" { comment_multiline lexbuf }
  | '\n' { Lexing.new_line lexbuf; EOL }
  | '{' { action 0 "" lexbuf }
  | "->" { ARROW }
  | ">" { GT }
  | "<" { LT }
  | "axiom" { AXIOM }
  | "tokens" { TOK }
  | "non-terminals" { NT }
  | "rules" { RULES }
  | id as s { IDENTIFIER s }
  | eof { EOF }
  | _ as x { let open Lexing in
             failwith (Printf.sprintf "unexpected char '%c' at line %d col %d\n" x
                         (lexbuf.lex_curr_p.pos_lnum)
                         (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol))}
and action level s = parse
  | '}' { if level = 0 then CODE s else action (level-1) (s ^ "}") lexbuf }
  | '{' { action (level + 1) (s ^ "{") lexbuf }
  | _ as x { if x == '\n' then Lexing.new_line lexbuf;
             action level (Printf.sprintf "%s%c" s x) lexbuf }
and comment = parse
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | _ { comment lexbuf }
and comment_multiline = parse
  | '\n' { Lexing.new_line lexbuf; comment_multiline lexbuf }
  | "*/" { token lexbuf }
  | _ { comment_multiline lexbuf }
