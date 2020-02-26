open Batteries
open BatList
open Symbols
open Utils
open E_regexp

type 'a set = 'a Set.t

(* Non-deterministic Finite Automata (NFA) *)

(* Les états d'un NFA [nfa_state] sont des entiers.

   Un NFA est modélisé sous la forme d'un record, avec les quatre champs
   suivants:
   - [nfa_states] contient la liste des états de l'automate.
   - [nfa_initial] contient la liste des états initiaux de l'automate.
   - [nfa_final] contient la liste des états finaux de l'automate sous la
     forme (q, t), où q est un état de l'automate, et t, de type
     [string -> token option] est une fonction qui construit un token
     à partir d'une chaîne de caractères.
   - [nfa_step q] donne la liste des transitions depuis l'état [q] sous la
     forme d'une liste [(charset, q')]. [charset] est l'ensemble des caractères
     qui permettent de prendre la transition vers l'état [q']. [charset] peut
     éventuellement être [None], ce qui indique une epsilon-transition. 
*)

type nfa_state = int

type nfa =
  {
    nfa_states: nfa_state list;
    nfa_initial: nfa_state list;
    nfa_final: (nfa_state * (string -> token option)) list;
    nfa_step: nfa_state -> (char set option * nfa_state) list
  }

(* [empty_nfa] est un NFA vide. *)
let empty_nfa =
  {
    nfa_states = [];
    nfa_initial = [];
    nfa_final = [];
    nfa_step = fun q -> [];
  }

(* Concaténation de NFAs.  *)
let cat_nfa n1 n2 =
   (* TODO *)
   empty_nfa

(* Alternatives de NFAs *)
let alt_nfa n1 n2 =
   (* TODO *)
   empty_nfa

(* Répétition de NFAs *)
(* t est de type [string -> token option] *)
let star_nfa n t =
   (* TODO *)
   empty_nfa


(* [nfa_of_regexp r freshstate t] construit un NFA qui reconnaît le même langage
   que l'expression régulière [r].
   [freshstate] correspond à un entier pour lequel il n'y a pas encore d'état dans 
   le nfa. Il suffit d'incrémenter [freshstate] pour obtenir de nouveaux état non utilisés.
   [t] est une fonction du type [string -> token option] utile pour les état finaux.
*)
let rec nfa_of_regexp r freshstate t =
  match r with
  | Eps -> { nfa_states = [freshstate];
             nfa_initial = [freshstate];
             nfa_final = [(freshstate,t)];
             nfa_step = fun q -> []}, freshstate + 1
  | Charset c -> { nfa_states = [freshstate; freshstate + 1];
                nfa_initial = [freshstate];
                nfa_final = [freshstate + 1, t];
                nfa_step = fun q -> if q = freshstate then [(Some c, freshstate + 1)] else []
              }, freshstate + 2
   (* TODO *)
   | _ -> empty_nfa, freshstate

(* Deterministic Finite Automaton (DFA) *)

(* Les états d'un DFA [dfa_state] sont des ensembles d'entiers.

   Similairement aux NFA, un DFA est modélisé sous la forme d'un record, avec
   les quatre champs suivants:

   - [dfa_states] contient la liste des états de l'automate.
   - [dfa_initial] contient l'état initial de l'automate.
   - [dfa_final] contient la liste des états finaux de l'automate sous la
     forme (q, t), où q est un état de l'automate, et t, de type
     [string -> token option] est une fonction qui construit un token
     à partir d'une chaîne de caractères.
   - [dfa_step q c] donne l'état [q'] accessible après avoir lu le caractère
     [c], depuis l'état [q]. [charset] peut éventuellement être [None], ce qui
     indique qu'aucune transition n'est possible depuis cet état, et avec ce
     caractère.
*)

type dfa_state = int set

type dfa =
  {
    dfa_states: dfa_state list;
    dfa_initial: dfa_state;
    dfa_final: (dfa_state * (string -> token option)) list;
    dfa_step: dfa_state -> char -> dfa_state option
  }

(* On va maintenant déterminiser notre NFA pour en faire un DFA. *)


(* [epsilon_closure] calcule la epsilon-fermeture d'un état [s] dans un NFA [n],
   c'est-à-dire l'ensemble des états accessibles depuis [s] en ne prenant que
   des epsilon-transitions. *)
let epsilon_closure (n: nfa) (s: nfa_state) : nfa_state set =
  (* La fonction [traversal visited s] effectue un parcours de l'automate en
     partant de l'état [s], et en suivant uniquement les epsilon-transitions. *)
  let rec traversal (visited: nfa_state set) (s: nfa_state) : nfa_state set =
         (* TODO *)
         visited
  in
  traversal Set.empty s

(* [epsilon_closure_set n ls] calcule l'union des epsilon-fermeture de chacun
   des états du NFA [n] dans l'ensemble [ls]. *)
let epsilon_closure_set (n: nfa) (ls: nfa_state set) : nfa_state set =
   (* TODO *)
   ls

(* [dfa_initial_state n] calcule l'état initial de l'automate déterminisé. *)
let dfa_initial_state (n: nfa) : dfa_state =
   (* TODO *)
   Set.empty

(* Construction de la table de transitions de l'automate DFA. *)

(* Comme vu en cours, pour construire la table de l'automate DFA à partir de
   l'automate NFA [n], on part d'un état [q] de l'automate (initialement, l'état
   initial, que l'on vient de calculer ci-dessus).

   On calcule l'ensemble [t] des transitions dans [n] de chacun des états de
   [q]. Cet ensemble est de type [(char set option * nfa_state) list].

   On transforme cet ensemble [t] de la manière suivante :
   - on jette les epsilon-transitions : [assoc_throw_none]
   - on transforme chaque transition ({c1,c2,..,cn}, q) en une liste de
     transitions [(c1,q); (c2,q); ...; (cn,q)] : [assoc_distribute_key]
   - on fusionne les transitions qui consomment le même caractère:
     [(c1,q1);(c1,q2);...;(c1,qn);(c2,q'1);...(c2,q'm)] ->
     [(c1,{q1,q2,...,qn});(c2,{q'1,...,q'm})] : [assoc_merge_vals]
   - on applique la epsilon-fermeture sur tous les états:
     [(c1,{q1,q2,...,qn});...;(cn,{qn}])] -> [(c1, eps({q1,q2,...,qn})); ...; (cn, eps({qn}))] :
     [epsilon_closure_set]

   On obtient alors l'ensemble des transitions depuis l'état [q] dans
   l'automate DFA.

   On réitère ce processus pour tous les nouveaux états que l'on atteint.
*)

let assoc_throw_none (l : ('a option * 'b) list) : ('a * 'b) list =
  List.filter_map (fun (o,n) ->
      match o with
        None -> None
      | Some x -> Some (x,n)
    ) l

let assoc_distribute_key (l : ('a set * 'b) list) : ('a * 'b) list =
  List.fold_left (fun (acc : ('a * 'b) list) (k, v) ->
      Set.fold (fun c acc -> (c, v)::acc) k acc)
    [] l

let assoc_merge_vals (l : ('a * 'b) list) : ('a * 'b set) list =
  List.fold_left (fun (acc : ('a * 'b set) list) (k, v) ->
      match List.assoc_opt k acc with
      | None -> (k, Set.singleton v)::acc
      | Some vl -> (k, Set.add v vl)::List.remove_assoc k acc
    ) [] l

let rec build_dfa_table (table: (dfa_state, (char * dfa_state) list) Hashtbl.t)
    (n: nfa)
    (ds: dfa_state) : unit =
  match Hashtbl.find_option table ds with
  | Some _ -> ()
  | None ->
    (* [transitions] contient les transitions du DFA construites
     * à partir des transitions du NFA comme décrit auparavant *)
    let transitions : (char * dfa_state) list =
         (* TODO *)
         []
      in
    Hashtbl.replace table ds transitions;
    List.iter (build_dfa_table table n) (List.map snd transitions)

(* Calcul des états finaux de l'automate DFA *)

(* Comme vu en cours, un état [q] du DFA est final si et seulement si il existe
   un état [q'] dans [q] qui soit un état final dans le NFA.

   Il nous faut de plus calculer le token qui sera reconnu par chaque état
   final.

   Supposons que l'on ait deux états finaux [q1, fun s -> SYM_IDENTIFIER s] et
   [q2, fun s -> SYM_WHILE] dans notre NFA.
   L'état [q = {q1,q2}] est final, mais comment choisir le token à reconnaître ?

   Dans ce cas précis, on souhaite reconnaître le mot-clé 'while' plutôt qu'un
   identifiant quelconque.

   Pour résoudre ce problème plus généralement, on introduit une fonction de
   priorité pour départager les tokens. La fonction [priority : token -> int]
   donne une valeur plus petite aux tokens les plus prioritaires.

*)

let priority t =
  match t with
  | SYM_EOF -> 100
  | SYM_IDENTIFIER _ -> 50
  | _ -> 0

(* [min_priority l] renvoie le token de [l] qui a la plus petite priorité, ou
   [None] si la liste [l] est vide. *)
let min_priority (l: token list) : token option =
   (* TODO *)
   None

(* [dfa_final_states n dfa_states] renvoie la liste des états finaux du DFA,
   accompagnés du token qu'ils reconnaissent. *)
let dfa_final_states (n: nfa) (dfa_states: dfa_state list) :
  (dfa_state * (string -> token option)) list  =
   (* TODO *)
   []

(* Construction de la relation de transition du DFA. *)

(* [make_dfa_step table] construit la fonction de transition du DFA, où [table]
   est la table générée par [build_dfa_table], définie ci-dessus. *)
let make_dfa_step (table: (dfa_state, (char * dfa_state) list) Hashtbl.t) =
  fun (q: dfa_state) (a: char) ->
   (* TODO *)
   None

(* Finalement, on assemble tous ces morceaux pour construire l'automate. La
   fonction [dfa_of_nfa n] vous est grâcieusement offerte. *)
let dfa_of_nfa (n: nfa) : dfa =
  let table : (dfa_state, (char * dfa_state) list) Hashtbl.t =
    Hashtbl.create (List.length n.nfa_states) in
  let dfa_initial = dfa_initial_state n in
  build_dfa_table table n dfa_initial;
  let dfa_states = Hashtbl.keys table |> List.of_enum in
  let dfa_final = dfa_final_states n dfa_states in
  let dfa_step = make_dfa_step table in
  {
    dfa_states  ;
    dfa_initial ;
    dfa_final   ;
    dfa_step    ;
  }

(* Analyse lexicale *)

(* Maintenant que tout est en place, on va pouvoir écrire un analyseur lexical,
   qui va découper notre programme source en une liste de tokens. *)

(* La fonction [tokenize_one d w] tente de reconnaître le plus grand préfixe
   possible de [w]. Elle renvoie un couple [(res,w')], où [res] est le résultat
   de l'analyse lexicale d'un mot et [w'] est le reste du programme à analyser.

   Le résultat est de type [lexer_result], défini ci-dessous:
   - [LRToken tok] indique que l'automate a reconnu le token [tok]
   - [LRskip] indique que l'automate a reconnu un mot qui ne génère pas de token
     (c'est le cas par exemple des espaces, tabulations, retours à la ligne et
     commentaires)
   - [LRerror] indique que l'automate n'a rien reconnu du tout : il s'agit donc
     d'une erreur.

*)

type lexer_result =
  | LRtoken of token
  | LRskip
  | LRerror

(* La fonction [tokenize_one] utilise une fonction interne [recognize q w
   current_word last_accepted] qui essaie de lire le plus grand préfixe de [w]
   reconnu par l'automate.

   - [q] est l'état courant de l'automate.
   - [w] est le reste du programme source à analyser.
   - [current_word] est le mot reconnu depuis l'état initial de l'automate.
   - [last_accepted] est de type [lexer_result * char list]. La première
     composante est le dernier résultat valable de l'analyseur : celui vers
     lequel on se rabattra lorsque l'on sera bloqué dans un état non final de
     l'automate. La deuxième composante est le reste du programme à analyser,
     après ce dernier token reconnu.

   La fonction recognize est lancée avec [q = d.dfa_initial], l'état initial du
   DFA, le programme à analyser [w], un mot courant vide, et un dernier état
   accepté dénotant une erreur (si on ne passe par aucun état final, il s'agit
   bien d'une erreur lexicale).

*)

let tokenize_one (d : dfa) (w: char list) : lexer_result * char list =
  let rec recognize (q: dfa_state) (w: char list)
      (current_token: char list) (last_accepted: lexer_result * char list)
    : lexer_result * char list =
         (* TODO *)
         last_accepted
  in
  recognize d.dfa_initial w [] (LRerror, w)

(* La fonction [tokenize_all d w] répète l'application de [tokenize_one] tant qu'on
   n'est pas arrivé à la fin du fichier (token [SYM_EOF]). Encore une fois,
   cette fonction vous est offerte. *)
let rec tokenize_all (d: dfa) (w: char list) : (token list * char list) =
  match tokenize_one d w with
  | LRerror, w -> [], w
  | LRskip, w -> tokenize_all d w
  | LRtoken token, w ->
    let (tokens, w) =
      if token = SYM_EOF
      then ([], w)
      else tokenize_all d w in
    (token :: tokens, w)



(* Fonctions d'affichage - Utile pour déboguer *)

(* Affichage d'un NFA *)
let nfa_to_string (n : nfa) : string =
  Printf.sprintf "===== NFA\nStates : %s\nInitial states : %s\nFinal states : %s\n%s"
    (String.concat " " (List.map (fun q -> string_of_int q) n.nfa_states))
    (String.concat " " (List.map (fun q -> string_of_int q) n.nfa_initial))
    (String.concat " " (List.map (fun (q,_) -> string_of_int q) n.nfa_final)) 
    (String.concat ""
       (List.map (fun q ->
            let l = n.nfa_step q in
            String.concat ""
              (List.map (fun (oa, q') ->
                   Printf.sprintf "step(%d, %s) = [%d]\n" q (match oa with Some a -> Printf.sprintf "[%s]" (string_of_char_set a) | _ -> "eps")
                     q'
                 ) l)
          ) n.nfa_states))

(* Affichage d'un DFA *)
let dfa_to_string (n : dfa) (alphabet: char list): string =
  Printf.sprintf "===== DFA\nStates : %s\nInitial state : %s\nFinal states : [%s]\n%s"
    (String.concat " " (List.map (fun q -> string_of_int_set q) n.dfa_states))
    (string_of_int_set n.dfa_initial)
    (String.concat " " (List.map (fun (q,_) -> string_of_int_set q) n.dfa_final))
    (String.concat "" (List.map (fun q ->
         String.concat "" (List.map (fun a ->
             let l = n.dfa_step q a in
             match l with
             | None -> ""
             | Some q' ->
               if not (Set.is_empty q') then
                 Printf.sprintf "step(%s, %c) = %s\n"
                   (string_of_int_set q)
                   a (string_of_int_set q')
               else ""
           ) alphabet);
       ) n.dfa_states))

(* Affichage graphique d'un DFA. Génère un fichier .dot que vous pouvez ensuite
   convertir en pdf avec la commande 'dot fichier.dot -Tsvg -o fichier.svg' ou
   bien en copiant le code DOT dans un convertisseur en ligne (par exemple :
   http://proto.informatics.jax.org/prototypes/dot2svg/). *)

let char_list_to_char_ranges s =
  let rec recognize_range (cl: int list) l opt_c n =
    match cl with
    | [] -> (match opt_c with
          None -> l
        | Some c -> l @ [(c,n)]
      )
    | c::r -> (match opt_c with
        | None -> recognize_range r l (Some c) 0
        | Some c' ->
          if c' + n + 1 = c
          then recognize_range r l (Some c') (n + 1)
          else recognize_range r ((c',n)::l) (Some c) 0
      )
  in
  let l = recognize_range (List.sort Stdlib.compare (List.map Char.code s)) [] None 0 in
  List.fold_left (fun acc (c,n) ->
      if n = 0
      then Printf.sprintf "%c%s" (Char.chr c) acc
      else Printf.sprintf "%c-%c%s" (Char.chr c) (Char.chr (c + n)) acc
    ) "" l

let dfa_to_dot oc (n : dfa) (cl: char list): unit =
  Printf.fprintf oc "digraph {\n";
  Printf.fprintf oc "N%s [shape=\"house\" color=\"red\"]\n" (string_of_int_set n.dfa_initial);
  List.iter (fun (q,t) ->
      Printf.fprintf oc "N%s [shape=\"rectangle\", label=\"%s\"]\n"
        (string_of_int_set q) (match t "0" with | Some s -> string_of_symbol s | None -> "" )) n.dfa_final;
  List.iter (fun q ->
      let l = List.fold_left (fun l a ->
          match n.dfa_step q a with
            None -> l
          | Some q' ->
            match List.assoc_opt q' l with
            | None -> (q', [a])::l
            | Some ql -> (q', a::ql)::List.remove_assoc q' l
        ) [] cl in
      List.iter (fun (q', cl) ->
          Printf.fprintf oc "N%s -> N%s [label=\"[%s]\"]\n"
            (string_of_int_set q)
            (string_of_int_set q') (char_list_to_char_ranges cl)
        ) l;
    ) n.dfa_states;
  Printf.fprintf oc "}\n"

let alts l =
  match l with
    [] -> Eps
  | a::r -> List.fold_left (fun acc r -> Alt(acc,r)) a l

let nfa_of_list_regexp l =
  let (n, fs) = List.fold_left (fun (nfa, fs) (r,t) ->
      let n,fs = nfa_of_regexp r fs t in
      (alt_nfa nfa n, fs)
    ) ({ nfa_states = []; nfa_initial = []; nfa_final = []; nfa_step = fun _ -> [] },1)
      l in n

let dfa_of_list_regexp l =
  let n = nfa_of_list_regexp l in
  dfa_of_nfa n

let tokenize_list_regexp l s =
  let d = dfa_of_list_regexp l in
  let tokens, leftover = tokenize_all d (char_list_of_string s) in
  if leftover <> []
  then Error (Printf.sprintf "Lexer failed to recognize string starting with '%s'\n"
                   (string_of_char_list (take 20 leftover))
                )
  else OK tokens

let file_contents file =
  let ic = open_in file in
  let rec aux s () =
    try
      let line = input_line ic in  (* read line from in_channel and discard \n *)
      aux (s ^ line ^ "\n") ()   (* close the input channel *)
    with e ->                      (* some unexpected exception occurs *)
      close_in_noerr ic;           (* emergency closing *)
      s in
  aux "" ()


let tokenize_file f =
  tokenize_list_regexp list_regexp (file_contents f)
