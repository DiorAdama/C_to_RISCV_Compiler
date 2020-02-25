open Batteries
open BatList
open Prog
open Utils
open Linear
open Rtl



   let liveness_instrs linearfunbody =
   (Hashtbl.create 0, Hashtbl.create 0)


let liveness_linear_prog lp =
  let lives = Hashtbl.create 17 in
  List.iter (function
        (s,Gfun f) -> Hashtbl.replace lives s (liveness_instrs f.linearfunbody)
    ) lp;
  lives
