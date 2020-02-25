open List_utils

type tokent = string
type nonterm = string

type action = string option

type rule = { rule_nt: nonterm;
              rule_prods: string list;
              rule_action: action
            }

type grammar = { tokens: (tokent * string option) list;
                 nonterms: nonterm list;
                 rules: rule list;
                 mlcode: string option;
                 axiom: nonterm option
               }


let dump_grammar oc (toks, nts, rules) =
  Printf.fprintf oc "tokens";
  List.iter (fun n -> Printf.fprintf oc " %s" n) toks;
  Printf.fprintf oc "\nnon-terminals ";
  List.iter (fun n -> Printf.fprintf oc " %s" n) nts;
  Printf.fprintf oc "\nrules\n";
  List.iter (fun (n,lt,a) -> Printf.fprintf oc "%s ->%s\n" n (print_seq (fun x -> x) lt)) rules

