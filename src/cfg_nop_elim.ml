open BatList
open Batteries
open Prog
open Utils
open Cfg

(* [nop_transitions cfg] gives the list of NOP transitions in the CFG.

   If node n is [Cnop s], then [(n,s)] should be in the result list.
*)
let nop_transitions (cfgfunbody: (int, cfg_node) Hashtbl.t) : (int * int) list =
   (* TODO *)
   []


(* [follow n l visited] gives the first non-nop successor of [n], according to
   the successor relation encoded in list [l]. [(x,y)] in [l] means there is a
   NOP-transition from node [x] to node [y].

   The set [visited] is used to make sure we don't fall into an infinite loop.
*)
let rec follow (n: int) (l: (int * int) list) (visited: int Set.t) : int =
   (* TODO *)
   n

(* [nop_transitions_closed] contains the list [(n,s)] of nodes [n] such that the
   instruction at node [n] is the beginning of a NOP-chain ending in node [s]. *)
let nop_transitions_closed cfgfunbody =
  List.map (fun (node_id, node) ->
      (node_id, follow node_id (nop_transitions cfgfunbody) Set.empty))
    (nop_transitions cfgfunbody)



(* [preds n] gives the list of predecessors of a node [n]. *)
let preds cfgfunbody n =
  Hashtbl.fold (fun m m' acc ->
      match m' with
      | Cfg.Cassign (_, _, s)
      | Cfg.Cprint (_, s)
      | Cfg.Cnop s -> if s = n then Set.add m acc else acc
      | Cfg.Creturn _ -> acc
      | Cfg.Ccmp (_, s1, s2) -> if s1 = n || s2 = n then Set.add m acc else acc
    ) cfgfunbody Set.empty

(* [replace_succ nop_succs s] gives the new name for node [s], after applying
   nop-transitions. *)
let replace_succ nop_succs s =
  match List.assoc_opt s nop_succs with
    None -> s
  | Some t -> t

(* [replace_succs nop_succs n] replaces the old CFG node names in node [n]
   with the new ones, according to [nop_succs]. *)
let replace_succs nop_succs (n: cfg_node) =
   (* TODO *)
   n

(* [nop_elim_fun f] transforms CFG function [f] by eliminating NOP instructions *)
let nop_elim_fun ({ cfgfunargs; cfgfunbody; cfgentry } as f: cfg_fun) =
  let nop_transf = nop_transitions_closed cfgfunbody in
  let cfgfunbody = Hashtbl.filter_map (fun n node ->
         Some node
    ) cfgfunbody in
  {f with cfgfunbody; cfgentry = replace_succ nop_transf cfgentry }

let nop_elim_gdef gd =
  match gd with
    Gfun f -> Gfun (nop_elim_fun f)

let nop_elimination cp =
  assoc_map nop_elim_gdef cp
