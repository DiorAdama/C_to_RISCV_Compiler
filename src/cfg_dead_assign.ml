open Batteries
open Cfg
open Prog
open Utils
open Cfg_liveness
    
(* Dead Assign Elimination *)

(* [dead_assign_elimination_fun f] performs DAE on function [f]. *)
let dead_assign_elimination_fun ({ cfgfunargs; cfgfunbody; cfgentry } as f: cfg_fun) =
  let cfgfunbody =
    Hashtbl.map (fun (n: int) (m: cfg_node) ->
        match m with
           (* TODO *)
        | _ -> m
      ) cfgfunbody in
  { f with cfgfunbody }

let dead_assign_elimination_gdef = function
    Gfun f -> Gfun (dead_assign_elimination_fun f)

let dead_assign_elimination p =
  assoc_map dead_assign_elimination_gdef p
