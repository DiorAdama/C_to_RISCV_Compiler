open Str
open List_utils
open Lexing
open Grammar_lexer
open Grammar_parser_yacc
open Grammar

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try main token lexbuf with
  | Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let parse_grammar file : grammar * nonterm =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with pos_fname = file};
  let gram : grammar = parse_with_error lexbuf in
  let (undefined_strings, used_strings) : string list * string list =
    List.fold_left (fun (undef, used) rule ->
        List.fold_left
          (fun (undef, used) prod ->
             let undef =
               if not (List.mem prod (List.map fst gram.tokens) || List.mem prod gram.nonterms || List.mem prod undef)
               then prod::undef
               else undef in
             let used =
               if List.mem prod used then used else prod::used in
             (undef, used)
          )
          (undef, used) (rule.rule_prods)
      ) ([],[]) (gram.rules) in

  (* Error if undefined tokens or non-terminals are encountered *)
  if undefined_strings <> []
  then (Printf.printf "Undefined tokens or non-terminals: %a\n" print_list undefined_strings;
       failwith "Undefined tokens or non-terminals");

  match gram.axiom with
  | None -> failwith "No axiom was defined for the grammar.\n Aborting."
  | Some axiom ->

    (* Warn if some non terminals are never seen on the right hand side of a rule. *)
    let unused_nts = List.filter (fun nt -> not (List.mem nt used_strings) && Some nt <> gram.axiom) gram.nonterms in
    if unused_nts <> [] then Printf.printf "The following non-terminals are declared but never appear on the right hand-side of a rule:\n%a\n" print_list unused_nts;

    (* Warn if some tokens are never seen on the right hand side of a rule. *)
    let unused_toks = (List.filter_map (fun (t,_) -> if not (List.mem t used_strings) then Some t else None) gram.tokens) in
    if unused_toks <> [] then Printf.printf "The following tokens are declared but never appear on the right hand-side of a rule:\n%a\n" print_list unused_toks;

    let h : (nonterm , rule list) Hashtbl.t =
      Hashtbl.create (List.length gram.nonterms) in
    List.iter ( fun r ->
        match Hashtbl.find_opt h r.rule_nt with
        | None -> Hashtbl.add h r.rule_nt [r]
        | Some lp -> Hashtbl.replace h r.rule_nt (lp@[r]) ) (gram.rules);
    let rules = List.concat (List.map (fun n -> hashget_def h n []) gram.nonterms) in
    { gram with rules = rules }, axiom
