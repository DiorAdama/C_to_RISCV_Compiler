open Batteries
open Rtl
open Linear
open Ltl
open Ltl_print
open Prog
open Utils
open Regalloc
open Linear_liveness

(* list of registers used to store arguments. [a0-a7] *)
let arg_registers =
  range ~start:starting_arg_register 8

(* Helpers to build pseudo-instructions.
 ** [push x] is compiled into [sub sp, sp, 8; sd r, 0(sp)]
 ** [pop x] is compiled into [ld r, 0(sp); add sp, sp, 8]
*)

let make_push r =
  [LSubi(reg_sp, reg_sp, !Archi.wordsize);
   LStore(reg_sp, 0, r, !Archi.wordsize)]

let make_pop r =
  [LLoad(r, reg_sp, 0, !Archi.wordsize);
   LAddi(reg_sp, reg_sp, !Archi.wordsize)]

let make_sp_sub v =
  [LSubi(reg_sp, reg_sp, v)]

let make_sp_add v =
  [LAddi(reg_sp, reg_sp, v)]




   let ltl_prog_of_linear_with_alloc_order alloc_order  lp =
   let prog = List.map (function
        (fname, Gfun f) ->
        (fname, Gfun {
   ltlfunargs = 0;
   ltlfunbody = [];
   ltlfuninfo = [];
   ltlregalloc = [];
   }
    )) lp in
   prog



let alloc_order_st = [
  reg_s1; reg_s2; reg_s3; reg_s4; reg_s5;
  reg_s6; reg_s7; reg_s8; reg_s9; reg_s10; reg_s11;
  reg_t2; reg_t3; reg_t4; reg_t5; reg_t6;
]

let alloc_order_ts = [
  reg_t2; reg_t3; reg_t4; reg_t5; reg_t6;
  reg_s1; reg_s2; reg_s3; reg_s4; reg_s5;
  reg_s6; reg_s7; reg_s8; reg_s9; reg_s10; reg_s11;
]

let ltl_prog_of_linear lp () =
  let alloc_order =
    if !Options.alloc_order_st then alloc_order_st else alloc_order_ts in
  ltl_prog_of_linear_with_alloc_order alloc_order lp

