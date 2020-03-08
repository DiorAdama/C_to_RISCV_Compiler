open E_regexp
open Lexer_generator
open Batteries
open Utils
open Symbols

let () =
  let regexp_list = [
    (keyword_regexp "while",    fun s -> Some (SYM_WHILE));
    (keyword_regexp "if",    fun s -> Some (SYM_IF));
  ] in
  (* Décommentez la ligne suivante pour tester sur la vraie liste d'expressions
     régulières. *)
  (* let regexp_list = list_regexp in *)
  List.iteri
    (fun i (rg, _) -> Printf.printf "%d: %s\n" i (string_of_regexp rg))
    regexp_list;

  let nfa = nfa_of_list_regexp regexp_list in

  Printf.printf "%s\n" (nfa_to_string nfa);

  let oc = open_out "/tmp/nfa.dot" in
  nfa_to_dot oc nfa;
  close_out oc;

  let dfa = dfa_of_nfa nfa in
  let oc = open_out "/tmp/dfa.dot" in
  dfa_to_dot oc dfa alphabet;
  close_out oc;


