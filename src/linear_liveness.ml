open Batteries
open BatList
open Prog
open Utils
open Linear
open Rtl

let gen_live (i: rtl_instr) =
  match i with
  | Rbinop (b, rd, rs1, rs2) -> Set.of_list [rs1; rs2]
  | Rprint rs
  | Runop (_, _, rs) -> Set.singleton rs
  | Rconst (_, _) -> Set.empty
  | Rbranch (_, rs1, rs2, _) -> Set.of_list [rs1; rs2]
  | Rjmp _ -> Set.empty
  | Rmov (_, rs) -> Set.singleton rs
  | Rret r -> Set.singleton r
  | Rlabel _ -> Set.empty

let kill_live (i: rtl_instr) =
  match i with
  | Rbinop (_, rd, _, _)
  | Runop (_, rd,_)
  | Rconst (rd, _)
  | Rmov (rd,_) -> Set.singleton rd
  | Rbranch (_, _, _, _)
  | Rprint _
  | Rret _
  | Rjmp _
  | Rlabel _ -> Set.empty

let linear_succs (ins: rtl_instr) i labels =
  match ins with
  | Rbranch(_, _, _, s1) -> [Hashtbl.find_default labels s1 0; i+1]
  | Rjmp s -> [Hashtbl.find_default labels s 0]
  | Rret r -> []
  | _ -> [i+1]

let setup_labels insl =
  let labels = Hashtbl.create 17 in
  List.iteri (fun i ins ->
      match ins with
      | Rlabel l -> Hashtbl.replace labels l i
      | _ -> ()
    ) insl;
  labels

let add_changes h k v =
  let orig = Hashtbl.find_default h k Set.empty in
  Hashtbl.replace h k v;
  not (Set.equal v orig)

let iter_liveness insl live_in live_out labels =
  List.fold_lefti (fun changed i ins ->

      let gl = gen_live ins in
      let kl = kill_live ins in
      let oi = Hashtbl.find_default live_out i Set.empty in
      let newin = Set.union gl (Set.diff oi kl) in
      let changed = add_changes live_in i newin || changed in
      let succs = linear_succs ins i labels in
      let j = List.fold_left (fun j succ ->
          Set.union j (Hashtbl.find_default live_in succ Set.empty)
        ) Set.empty succs in
      add_changes live_out i j || changed
    ) false insl

let liveness_instrs insns =
  let live_in = Hashtbl.create 17 in
  let live_out = Hashtbl.create 17 in
  let labels = setup_labels insns in
  let rec aux () =
    if iter_liveness insns live_in live_out labels
    then aux ()
    else (live_in, live_out) in
  aux ()

let liveness_linear_prog lp =
  let lives = Hashtbl.create 17 in
  List.iter (function
        (s,Gfun f) -> Hashtbl.replace lives s (liveness_instrs f.linearfunbody)
    ) lp;
  lives
