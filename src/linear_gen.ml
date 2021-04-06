open Batteries
open Rtl
open Linear
open Prog
open Utils
open Report
open Linear_print
open Options
open Linear_liveness

let succs_of_rtl_instr (i: rtl_instr) =
  match i with
  | Rtl.Rbranch (_, _, _, s1) -> [s1]
  | Rtl.Rjmp s -> [s]
  | _ -> []

let rec succs_of_rtl_instrs il : int list =
  List.concat (List.map succs_of_rtl_instr il)

(* effectue un tri topologique des blocs.  *)
let sort_blocks (nodes: (int, rtl_instr list) Hashtbl.t) entry =
  let rec add_block order n =
    let order = order @ [n] in 
    match succs_of_rtl_instrs (Hashtbl.find nodes n) with 
      | [] -> order
      | [s] -> if (List.mem s order) then order else add_block order s 
      | [s1; s2] -> add_block (add_block order s2 ) s1 
      | _ -> failwith "CFG Node with more than 2 successors"  
  in
  add_block [] entry 


(* Supprime les jumps inutiles (Jmp à un label défini juste en dessous). *)
let rec remove_useless_jumps (l: rtl_instr list) =
  let f_fold rtl_ins a = 
    match a with 
      | [] -> [rtl_ins]
      | hd::tl -> 
          (match hd with 
            | Rlabel i -> 
                (match rtl_ins with 
                  | Rjmp j when j=i -> a 
                  | _ -> rtl_ins::a)
            | _ -> rtl_ins::a)
  in
  List.fold_right f_fold l []


let rec useful_labels l = 
  match l with
    | [] -> Set.empty
    | (Rjmp i)::tl ->  Set.add i (useful_labels tl) 
    | (Rbranch (_, _, _, i))::tl -> Set.add i (useful_labels tl) 
    | _::tl -> useful_labels tl

(* Remove labels that are never jumped to. *)
let remove_useless_labels (l: rtl_instr list) =
  let ul = useful_labels l in 
  let f_filter rtl_ins = 
    match rtl_ins with 
      | Rlabel i when not (Set.mem i ul) -> false 
      | _ -> true
  in
  List.filter f_filter l

let linear_of_rtl_fun
    ({ rtlfunargs; rtlfunbody; rtlfunentry; rtlfuninfo; rtlfunstksz }: rtl_fun) =
  let block_order = sort_blocks rtlfunbody rtlfunentry in
  let linearinstrs =
    Rjmp rtlfunentry ::
    List.fold_left (fun l n ->
        match Hashtbl.find_option rtlfunbody n with
        | None -> l
        | Some li -> l @ Rlabel(n) :: li
      ) [] block_order in
  { linearfunargs = rtlfunargs;
    linearfunbody =
      linearinstrs |> remove_useless_jumps |> remove_useless_labels;
    linearfuninfo = rtlfuninfo;
    linearfunstksz = rtlfunstksz;
  }

let linear_of_rtl_gdef = function
    Gfun f -> Gfun (linear_of_rtl_fun f)

let linear_of_rtl r =
  assoc_map linear_of_rtl_gdef r

let pass_linearize rtl =
  let linear = linear_of_rtl rtl in
  let lives = liveness_linear_prog linear in
  dump !linear_dump (fun oc -> dump_linear_prog oc (Some lives)) linear
    (fun file () -> add_to_report "linear" "Linear" (Code (file_contents file)));
  OK (linear, lives)

