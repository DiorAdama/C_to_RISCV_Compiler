open Batteries
open BatList
open BatBuffer
open Elang
open Rtl
open Regalloc
open Linear
open Linear_liveness
open Prog
open Utils

(* LTL/Risc-V registers *)
type ltl_reg = int
let reg_zero = 0
let reg_ra = 1
let reg_sp = 2
let reg_gp = 3
let reg_tp = 4
let reg_t0 = 5
let reg_t1 = 6
let reg_t2 = 7
let reg_s0 = 8
let reg_s1 = 9
let reg_a0 = 10
let reg_a1 = 11
let reg_a2 = 12
let reg_a3 = 13
let reg_a4 = 14
let reg_a5 = 15
let reg_a6 = 16
let reg_a7 = 17
let reg_s2 = 18
let reg_s3 = 19
let reg_s4 = 20
let reg_s5 = 21
let reg_s6 = 22
let reg_s7 = 23
let reg_s8 = 24
let reg_s9 = 25
let reg_s10 = 26
let reg_s11 = 27
let reg_t3 = 28
let reg_t4 = 29
let reg_t5 = 30
let reg_t6 = 31

let reg_tmp = [reg_t0; reg_t1; reg_t2; reg_t3; reg_t4; reg_t5; reg_t6]
let reg_tmp1 = reg_t0
let reg_tmp2 = reg_t1
let reg_fp = reg_s0
let reg_ret = reg_a0

type ltl_instr =
    LAddi of ltl_reg * ltl_reg * int
  | LSubi of ltl_reg * ltl_reg * int
  | LStore of ltl_reg * int * ltl_reg * int (* LStore(rd, rofs, rs, sz) : store
                                               value in [rs] on [sz] bytes at
                                               address [rd+rofs] *)
  | LLoad of ltl_reg * ltl_reg * int * int (* LLoad(rd, rs, rofs, sz) : load
                                              value at address [rs+rofs] on [sz]
                                              bytes in register [rd]. *)
  | LMov of ltl_reg * ltl_reg   (* LMov(rd, rs) : move value of [rs] into [rd].
                                   *)
  | LLabel of string
  | LJmp of string
  | LBinop of binop * ltl_reg * ltl_reg * ltl_reg (* LBinop(b, rd, rs1, rs2) :
                                                     performs binary operation
                                                     [b] on values in registers
                                                     [rs1] and [rs2]. Stores the
                                                     result in [rd]. *)
  | LUnop of unop * ltl_reg * ltl_reg (* LUnop(u,rd,rs) : performs unary
                                         operation [u] on register [rs]. Stores
                                         the result in [rd]. *)
  | LConst of ltl_reg * int     (* LConst(rd,i) : load immediate value [i] in
                                   register [rd]. *)
  | LComment of string
  | LCall of string             (* LCall(f) : calls function [f]. *)
  | LBranch of rtl_cmp * ltl_reg * ltl_reg * string (* LBranch(cmp, rs1, rs2,
                                                       label): compares [rs1]
                                                       and [rs2] according to
                                                       comparison [cmp]. If
                                                       true, jump to [label]. *)
  | LJmpr of ltl_reg            (* LJmpr(r) : jumps to address in [r]. *)
  | LHalt                       (* Stops the program. *)

(* An LTL function is essentially a list of instructions (field [ltlfunbody]).
   The other fields are mainly here for debugging and statistics :
   - [ltlfunargs] gives the number of arguments that the function takes;
   - [ltlfuninfo] is a mapping from source variable names to the RTL register
     they're allocated in;
   - [ltlregalloc] is a mapping from RTL registers to LTL locations.
   *)

type ltl_fun =
  { ltlfunargs: int;
    ltlfunbody: ltl_instr list;
    ltlfuninfo: (string*reg) list;
    ltlregalloc: (reg*loc) list;
  }


(* Lists of caller-save and callee-save registers *)

let caller_saved = [
  reg_a0; reg_a1; reg_a2; reg_a3; reg_a4; reg_a5; reg_a6; reg_a7;
  reg_t0; reg_t1; reg_t2; reg_t3; reg_t4; reg_t5; reg_t6; reg_ra;
]

let callee_saved = [
  reg_s0; reg_s1; reg_s2; reg_s3; reg_s4; reg_s5; reg_s6; reg_s7;
  reg_s8; reg_s9; reg_s10; reg_s11; reg_sp; reg_ra
]

(* We pass 8 arguments in registers (a0-a7). *)
let number_of_arguments_passed_in_registers = 8
let starting_arg_register = reg_a0
