open Batteries
open Cfg
open Prog
open Utils
open Cfg_liveness

(* Dead Assign Elimination  -- Élimination des affectations mortes *)

(* [dead_assign_elimination_fun f] élimine les affectations mortes dans la
   function [f]. Cette fonction renvoie un couple [(f',c)] oú [f'] est la
   nouvelle fonction, et [c] est un booléen qui indique si du progrès a été
   fait. *)
let dead_assign_elimination_fun ({ cfgfunargs; cfgfunbody; cfgentry } as f: cfg_fun) =
  let changed = ref false in
  let cfgfunbody =
    Hashtbl.map (fun (n: int) (m: cfg_node) ->
        match m with
           (* TODO *)
        | _ -> m
      ) cfgfunbody in
  ({ f with cfgfunbody }, !changed )

(* Applique l'élimination de code mort autant de fois que nécessaire. Testez
   notamment sur le fichier de test [basic/useless_assigns.e]. *)
let rec iter_dead_assign_elimination_fun f =
  let f, c = dead_assign_elimination_fun f in
   (* TODO *)
   f

let dead_assign_elimination_gdef = function
    Gfun f -> Gfun (iter_dead_assign_elimination_fun f)

let dead_assign_elimination p =
  assoc_map dead_assign_elimination_gdef p
