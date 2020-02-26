open E_regexp
open Lexer_generator
open Batteries
open Utils
open Symbols

let () =
  let lowercase_letters = "abcdefghijklmnopqrstuvwxyz" in
  let uppercase_letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let digits = "0123456789" in
  let other_characters = "?!=<>_ ;,{}()[]-+*/%\n\t" in
  let alphabet = char_list_of_string (lowercase_letters ^ uppercase_letters ^ digits ^ other_characters) in
  let letter_regexp = char_range (char_list_of_string (uppercase_letters ^ lowercase_letters)) in
  let digit_regexp = char_range (char_list_of_string digits) in
  let keyword_regexp s = str_regexp (char_list_of_string s) in
  let regexp_list = [
    (keyword_regexp "while",    fun s -> Some (SYM_WHILE));
    (keyword_regexp "if",    fun s -> Some (SYM_IF));
  ] in

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


