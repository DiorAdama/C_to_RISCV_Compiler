open Batteries
open Elang
open Cfg
open Utils
open Prog

(* [cfg_expr_of_eexpr e] converts an [Elang.expr] into a [expr res]. This should
   always succeed and be straightforward.

   In later versions of this compiler, you will add more things to [Elang.expr]
   but not to [Cfg.expr], hence the distinction.
*)
let rec cfg_expr_of_eexpr (e: Elang.expr) : expr res =
   Error "cfg_expr_of_eexpr not implemented yet."

(* [cfg_node_of_einstr next cfg succ i] builds the CFG node(s) that correspond
   to the E instruction [i].

   [cfg] is the current state of the control-flow graph.

   [succ] is the successor of this node in the CFG, i.e. where to go after this
   instruction.

   [next] is the next available CFG node identifier.

   This function returns a pair (n, next) where [n] is the identifer of the
   node generated, and [next] is the new next available CFG node identifier.

   Hint: several nodes may be generated for a single E instruction.
*)
let rec cfg_node_of_einstr (next: int) (cfg : (int, cfg_node) Hashtbl.t)
    (succ: int) (i: instr) : (int * int) res =
  match i with
  | Elang.Iassign (v, e) ->
    cfg_expr_of_eexpr e >>= fun e ->
    Hashtbl.replace cfg next (Cassign(v,e,succ));
    OK (next, next + 1)
   | _ -> Error "cfg_node_of_einstr not implemented yet."

(* Some nodes may be unreachable after the CFG is entirely generated. The
   [reachable_nodes n cfg] constructs the set of node identifiers that are
   reachable from the entry node [n]. *)
let rec reachable_nodes n (cfg: (int,cfg_node) Hashtbl.t) =
  let rec reachable_aux n reach =
    if Set.mem n reach then reach
    else let reach = Set.add n reach in
      match Hashtbl.find_option cfg n with
      | None -> reach
      | Some (Cnop succ)
      | Some (Cprint (_, succ))
      | Some (Cassign (_, _, succ)) -> reachable_aux succ reach
      | Some (Creturn _) -> reach
      | Some (Ccmp (_, s1, s2)) ->
        reachable_aux s1 (reachable_aux s2 reach)
  in reachable_aux n Set.empty

(* [cfg_fun_of_efun f] builds the CFG for E function [f]. *)
let cfg_fun_of_efun { funargs; funbody } =
  let cfg = Hashtbl.create 17 in
  Hashtbl.replace cfg 0 (Creturn (Eint 0));
  cfg_node_of_einstr 1 cfg 0 funbody >>= fun (node, _) ->
  (* remove unreachable nodes *)
  let r = reachable_nodes node cfg in
  Hashtbl.filteri_inplace (fun k _ -> Set.mem k r) cfg;
  OK { cfgfunargs = funargs;
       cfgfunbody = cfg;
       cfgentry = node;
     }

let cfg_gdef_of_edef gd =
  match gd with
    Gfun f -> cfg_fun_of_efun f >>= fun f -> OK (Gfun f)

let cfg_prog_of_eprog (ep: eprog) : cfg_fun prog res =
  assoc_map_res (fun fname -> cfg_gdef_of_edef) ep
