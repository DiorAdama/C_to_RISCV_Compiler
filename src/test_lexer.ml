open E_regexp
open Lexer_generator
open Batteries
open Utils
open Symbols

let () =
  let regexp_list = [
    (keyword_regexp "while",    fun s -> Some (SYM_WHILE));
    (keyword_regexp "if",    fun s -> Some (SYM_IF));
    (Cat(letter_regexp,
         Star(identifier_material)),
     fun s -> Some (SYM_IDENTIFIER s));

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

  let n =
    {
      nfa_states = [1; 2; 3; 4] ;
      nfa_initial = [1] ;
      nfa_final = [(3, fun s -> None); (4, fun s -> None)];
      nfa_step = fun q ->
        match q with
        | 1 -> [(Some (Set.singleton '0'), 2); (None, 3)]
        | 2 -> [(Some (Set.singleton '1'), 2); (Some (Set.singleton '1'), 4)]
        | 3 -> [(Some (Set.singleton '0'), 4); (None, 2)]
        | 4 -> [(Some (Set.singleton '0'), 2)]
        | _ -> []

    } in

  let expect_set str s_got s_exp =
    if Set.equal s_got s_exp
    then Printf.printf "[OK] %s\n" str
    else Printf.printf "[KO] %s : got %s, expected %s\n" str (string_of_int_set s_got)
        (string_of_int_set s_exp) in

  let ec1 = epsilon_closure n 1 in
  let ec2 = epsilon_closure n 2 in
  let ec3 = epsilon_closure n 3 in
  let ec4 = epsilon_closure n 4 in

  expect_set "epsilon_closure 1" ec1 (Set.of_list [1;2;3]);
  expect_set "epsilon_closure 2" ec2 (Set.of_list [2]);
  expect_set "epsilon_closure 3" ec3 (Set.of_list [2;3]);
  expect_set "epsilon_closure 4" ec4 (Set.of_list [4]);

  expect_set "dfa_initial_state" (dfa_initial_state n) (Set.of_list [1;2;3]);

  let string_of_opt_tok ot =
    match ot with
      None -> "None"
    | Some t -> Printf.sprintf "Some (%s)" (string_of_symbol t)
  in

  let expect_token_option str to_got to_exp =
    if to_got = to_exp
    then Printf.printf "[OK] %s\n" str
    else Printf.printf "[KO] %s : got %s, expected %s\n" str (string_of_opt_tok to_got)
        (string_of_opt_tok to_exp)
  in

  expect_token_option "min_priority 1" (min_priority [SYM_EOF; SYM_IDENTIFIER "bla"; SYM_WHILE]) (Some SYM_WHILE);
  expect_token_option "min_priority 2" (min_priority [SYM_EOF; SYM_IDENTIFIER "bla"]) (Some (SYM_IDENTIFIER "bla"));
  expect_token_option "min_priority 3" (min_priority [SYM_EOF; SYM_WHILE]) (Some SYM_WHILE);

  expect_token_option "min_priority 4" (min_priority []) None;

  let set_incl s1 s2 =
    Set.for_all (fun s -> Set.exists (Set.equal s) s2) s1
  in

  let set_eq s1 s2 = set_incl s1 s2 && set_incl s2 s1 in

  let string_of_int_set_set s =
    Set.map (fun s ->
        Printf.sprintf "{%s}" (String.concat "," (Set.to_list (Set.map string_of_int s)))
      ) s
    |> Set.to_list
    |> String.concat ", "
    |> Printf.sprintf "{%s}"
  in

  let expect_set_set str (set_got : int set set) (set_exp : int set set) =
    if set_eq set_got set_exp
    then Printf.printf "[OK] %s\n" str
    else Printf.printf "[KO] %s : got %s, expected %s\n" str
        (string_of_int_set_set set_got)
        (string_of_int_set_set set_exp)
  in

  let table = Hashtbl.create 10 in
  build_dfa_table table n (dfa_initial_state n);
  expect_set_set "dfa states" (Hashtbl.keys table |> Set.of_enum) (Set.of_list [Set.of_list [1;2;3]; Set.of_list [2;4]; Set.of_list [2]])

