open Grammar_parser
open Batteries
open Ll_parser
open List_utils
open Grammar


let int_of_lltype = function
  | First i
  | Follow i -> i

let nth_rule (toks,nts,rules) a =
  (List.nth rules (int_of_lltype a - 1))

let rec make_list l =
  match l with
    [] -> "[]"
  | i::r -> Printf.sprintf "$%d :: %s" (i+1) (make_list r)

(* Return the list of elements of the rule. *)
let default_action (pl: string list) : string =
  make_list (List.mapi (fun i e -> i) pl)

let resolve_vars s =
  Str.global_replace (Str.regexp "\\$\\([0-9]+\\)") "p\\1" s

let make_nt (table: string*string -> lltype list) (toks,nts,rules) oc n () =
  Printf.fprintf oc "and parse_%s tokens () =\n" n;
  Printf.fprintf oc " begin match tokens with\n";
  List.iteri
    (fun i t ->
       let rules = List.map (fun a -> nth_rule (toks,nts,rules) a) (table (n,t)) in
       match rules with
         [] -> ()
       | {rule_prods = pl; rule_action = act}::_ ->
         Printf.fprintf oc " | (symbol, _) :: _ when is_%s symbol -> begin\n" t;
         List.iteri
           (fun i t ->
              if List.mem t toks
              then Printf.fprintf oc "    eat_%s tokens >>= fun (p%d, tokens) ->\n" t (i + 1)
              else Printf.fprintf oc "    parse_%s tokens ()  >>= fun (p%d, tokens) ->\n" t (i+1))
           pl;
         Printf.fprintf oc "\n" ;
         Printf.fprintf oc " let res =\n" ;
         (match act with
          | Some act -> Printf.fprintf oc "    %s\n" (resolve_vars act)
          | _ ->
            Printf.fprintf oc "    %s\n" (resolve_vars (default_action pl))
         );
         Printf.fprintf oc " in OK (res, tokens)\n" ;
         Printf.fprintf oc "end\n";
    )
    toks;
  let expected = List.filter (fun t -> List.length (table (n,t)) > 0) toks in
  Printf.fprintf oc "  | tokens -> \n";
  Printf.fprintf oc " let got,lexpos = match tokens with [] -> \"EOF\",None | (symbol, lexpos) :: _ -> (string_of_symbol symbol, lexpos) in Error (\n";
  Printf.fprintf oc "      (match lexpos with \n";
  Printf.fprintf oc "      | Some lexpos -> Printf.sprintf \"At %%s, error while parsing %s\\n\" (string_of_position lexpos) \n" n;
  Printf.fprintf oc "      | None -> Printf.sprintf \"Error while parsing %s\\n\" )^\n" n;
  Printf.fprintf oc "  Printf.sprintf \"Expected one of \"^\n";
  begin
    match expected with
      [] -> Printf.fprintf oc "Printf.sprintf \"{}\" ^\n"
    | a::r -> 
      List.iteri (fun i t ->
          Printf.fprintf oc "Printf.sprintf \"%s %%s\" (string_of_symbol default_%s)^\n" (if i = 0 then "{" else ",") t;
        ) (a::r);
      Printf.fprintf oc "Printf.sprintf \"}\" ^ \n"
  end;
  Printf.fprintf oc "  Printf.sprintf \" but got '%%s' instead.\\n\" got\n";
  Printf.fprintf oc "  )";
  Printf.fprintf oc "\n  end\n\n"

let make_parser  (table: string*string -> lltype list)
    (toks,nts,rules,mlcode)
    (typ: (tokent * string) list)
    oc () =
  Stdlib.Option.iter (fun mlcode -> Printf.fprintf oc "\n\n%s\n\n" mlcode) mlcode;
  List.iter (fun t ->
      begin match List.assoc_opt t typ with
        | Some ty ->
          begin
            Printf.fprintf oc "let is_%s = function \n" t;
            Printf.fprintf oc " | %s _ -> true\n" t;
            Printf.fprintf oc " | _ -> false\n";

            Printf.fprintf oc "let default_%s = %s %s\n" t t
                       (match ty with
                          "string" -> "\"\""
                        | "int" -> "0"
                        | "bool" -> "false"
                        | _ -> failwith (Printf.sprintf "Don't know how to generate a default value of type %s" ty)
                )
          end
        | None -> begin
            Printf.fprintf oc "let is_%s = function \n" t;
            Printf.fprintf oc " | %s -> true\n" t;
            Printf.fprintf oc " | _ -> false\n";
            Printf.fprintf oc "let default_%s = %s\n" t t
          end
      end;
    ) toks;
  List.iter (fun t ->
      Printf.fprintf oc "let eat_%s = function \n" t;
      begin match List.assoc_opt t typ with
        | Some _ -> Printf.fprintf oc "| (%s(x),_) :: rtokens -> OK (x, rtokens)\n" t
        | None -> Printf.fprintf oc "| (%s,_) :: rtokens -> OK ((), rtokens)\n" t
      end;
      Printf.fprintf oc "|   (x,Some pos) :: _ -> Error (Printf.sprintf \"At position %%s, expected %%s, got %%s.\\n\"";
      Printf.fprintf oc "    (string_of_position pos)";
      Printf.fprintf oc "    (string_of_symbol default_%s)" t;
      Printf.fprintf oc "    (string_of_symbol x))";
      Printf.fprintf oc "    | (x,None) :: _ -> Error (Printf.sprintf \"Expected %%s, got %%s.\\n\"";
      Printf.fprintf oc "    (string_of_symbol default_%s)" t;
      Printf.fprintf oc "    (string_of_symbol x))";
      Printf.fprintf oc "    | _ -> Error  (Printf.sprintf \"Expected %%s, got EOF.\\n\" (string_of_symbol default_%s))\n" t;

    ) toks;
  Printf.fprintf oc "let rec ____unused = () \n";
  List.iter (fun n -> make_nt table (toks,nts,rules) oc n ()) nts

let nts_ordered start (toks,nts,rules) =
  let nts =
    let rec aux acc nt =
      if List.mem nt acc then acc
      else let acc = nt::acc in
        let rules : (string list * string) list =
          filter_map (fun {rule_nt; rule_prods; rule_action} ->
              if rule_nt = nt
              then Some (rule_prods, match rule_action with Some a -> a | None -> "")
              else None) rules
        in
        List.fold_left (fun acc (rule,act) ->
            List.fold_left (fun acc tokornt ->
                if List.mem tokornt toks then acc else aux acc tokornt
              ) acc rule
          ) acc rules
    in List.rev (aux [] start)
  in
  let rules =
    List.concat (List.map (fun nt -> List.filter (fun r -> r.rule_nt = nt) rules) nts)
  in (nts,rules)

let _ =
  let grammar_file = ref None in
  let table_file = ref None in
  let parser_ml_file = ref None in

  Arg.parse
    [("-g", Arg.String (fun s -> grammar_file := Some s), "Input grammar file (.g)");
     ("-t", Arg.String (fun s -> table_file := Some s), "Where to output tables (.html)");
     ("-pml", Arg.String (fun s -> parser_ml_file := Some s), "Where to output the parser code (.ml)");
    ] print_endline "Usage: ";
  match !grammar_file with
  | None -> failwith "Please specify a grammar file using '-g <grammar.g>'"
  | Some gramfile ->
    let gram, axiom = parse_grammar gramfile in
    let (toks, nts, rules, mlcode) =
      (gram.tokens, gram.nonterms, gram.rules, gram.mlcode) in
    let toks = List.map fst gram.tokens in
    let (nts, rules) = nts_ordered axiom (toks,nts,rules) in
    iter_nullnt (toks, nts, rules) ();
    iter_first (toks, nts, rules) ();
    iter_follownt (toks, nts, rules) ();
    fill_lltable (toks, nts, rules) ();
    (match !table_file with
     | Some tfile -> let oc = open_out tfile in
       print_html (toks, nts, rules) (Format.formatter_of_out_channel oc) ();
       close_out oc
     | None -> ());
    (match !parser_ml_file with
     | None -> Printf.fprintf stderr "Please specify where I should write the generated parser code using '-pml <generated_parser.ml>'"
     | Some mlfile ->
       let oc = open_out mlfile in
       make_parser (fun (n,t) -> hashget_def lltable (n,t) [])
         (toks, nts, rules, mlcode)
         (List.filter_map (fun (t,o) ->
              match o with
              | None -> None
              | Some typ -> Some (t,typ)
            ) gram.tokens)
         oc ();
       close_out oc
    )
