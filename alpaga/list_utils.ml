open List

(* Affiche une liste de string. *)
let print_list oc l =
  List.iter (fun s -> Printf.fprintf oc "%s " s) l


let rec print_seq p = function
  | [] -> ""
  | a::r -> " " ^ p a ^ print_seq p r

let hashget_def t k d =
  match Hashtbl.find_opt t k with
  | None -> d
  | Some b -> b

(* Teste si toutes les valeurs de [l1] sont dans [l2].
   (Relation "sous-ensemble") *)
let incl l1 l2 =
  for_all (fun x -> mem x l2) l1

(* Relation "même ensemble". *)
let same_list l1 l2 = incl l1 l2 && incl l2 l1

(* L'ensemble des éléments de [l1] qui ne sont pas dans [l2]. *)
let diff l1 l2 =
  filter (fun x -> not (mem x l2)) l1

(* Enlève les doublons dans une liste. *)
let rec cleardup l =
  match l with
    [] -> []
  | a::r -> if mem a r then cleardup r else a::cleardup r

let inter l1 l2 =
  filter (fun x -> mem x l2) l1

let disjoint l1 l2 =
  inter l1 l2 = []

let rec filter_map f l =
  match l with
    [] -> []
  | a::r ->
    match f a with
    | Some fa -> fa :: filter_map f r
    | _ -> filter_map f r

let filter_mapi f l =
  filter_map (fun (i,e) -> f i e)
    (List.mapi (fun i e -> (i,e)) l)
