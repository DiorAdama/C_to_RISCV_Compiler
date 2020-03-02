open Batteries
open Cfg
open Prog
open Utils

(* Liveness analysis *)

(* [vars_in_expr e] returns the set of variables appearing in [e]. *)
let rec vars_in_expr (e: expr) =
   Set.empty

(* [live_cfg_node node live_after] gives the set of variables live before the
   node [node], given the set [live_after] of variables live after this node. *)
let live_cfg_node (node: cfg_node) (live_after: string Set.t) =
   live_after

(* Computes the set of live variables after a node [n] in a CFG [cfg].
   [lives] is a mapping from CFG node identifier to the set of variables that
   are live before this node.
*)
let live_after_node cfg n (lives: (int, string Set.t) Hashtbl.t) : string Set.t =
   Set.empty

(* [live_cfg_nodes cfg lives] makes one iteration of the fixpoint computation.

   This returns a boolean that is true if some progress has been made in this
   iteration (the set of variables live at at least one node has changed), false
   otherwise. *)
let live_cfg_nodes cfg (lives : (int, string Set.t) Hashtbl.t) =
   false

(* [live_cfg_fun f] computes the set of live variables at each point by
   iterating [live_cfg_nodes] as long as progress is made. *)
let live_cfg_fun ({ cfgfunargs; cfgfunbody; cfgentry }: cfg_fun) =
  let lives : (int, string Set.t) Hashtbl.t = Hashtbl.create 17 in
  let rec aux () =
    if live_cfg_nodes cfgfunbody lives
    then aux ()
    else () in
  aux ();
  lives

let live_cfg_fun ({ cfgfunargs; cfgfunbody; cfgentry }: cfg_fun) =
  fixpoint (fun cfg lives n ->
      match Hashtbl.find_option cfg n with
      | Some cn -> live_cfg_node cn (live_after_node cfg n lives)
      | None -> failwith "Unknown node"
    )
    (fun cfg n cn -> Set.empty)
    cfgfunbody
