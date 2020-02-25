open Batteries
open Elang
open Ltl
open Prog
open Rtl_print
open Utils
open Regalloc

(* Printing LTL registers, locations, instructions. *)
let string_of_reg = function
  | 0 -> "zero"
  | 1 -> "ra"
  | 2 -> "sp"
  | 3 -> "gp"
  | 4 -> "tp"
  | 5 -> "t0"
  | 6 -> "t1"
  | 7 -> "t2"
  | 8 -> "s0"
  | 9 -> "s1"
  | 10 -> "a0"
  | 11 -> "a1"
  | 12 -> "a2"
  | 13 -> "a3"
  | 14 -> "a4"
  | 15 -> "a5"
  | 16 -> "a6"
  | 17 -> "a7"
  | 18 -> "s2"
  | 19 -> "s3"
  | 20 -> "s4"
  | 21 -> "s5"
  | 22 -> "s6"
  | 23 -> "s7"
  | 24 -> "s8"
  | 25 -> "s9"
  | 26 -> "s10"
  | 27 -> "s11"
  | 28 -> "t3"
  | 29 -> "t4"
  | 30 -> "t5"
  | 31 -> "t6"
  | _ -> "undefreg"

let print_reg r =
  string_of_reg r

let print_loc loc =
  match loc with
  | Stk o -> Format.sprintf "stk(%d)" o
  | Reg r -> print_reg r

let print_binop (b: binop) =
  match b with
  | Elang.Eadd -> "add"
  | Elang.Emul -> "mul"
  | Elang.Emod -> "mod"
  | Elang.Exor -> "xor"
  | Elang.Ediv -> "div"
  | Elang.Esub -> "sub"
  | Elang.Eclt -> "clt"
  | Elang.Ecle -> "cle"
  | Elang.Ecgt -> "cgt"
  | Elang.Ecge -> "cge"
  | Elang.Eceq -> "ceq"
  | Elang.Ecne -> "cne"


let print_unop  (u: unop) =
  match u with
  | Elang.Eneg -> "neg"

let dump_ltl_instr oc (i: ltl_instr) =
  match i with
  | LAddi(rd, rs, i) ->
    Format.fprintf oc "%s <- addi %s, %d" (print_reg rd) (print_reg rs) i
  | LSubi(rd, rs, i) ->
    Format.fprintf oc "%s <- subi %s, %d" (print_reg rd) (print_reg rs) i
  | LBinop(b, rd, rs1, rs2) ->
    Format.fprintf oc "%s <- %s %s, %s"
      (print_reg rd) (print_binop b) ( print_reg rs1) (print_reg rs2)
  | LUnop(u, rd, rs) ->
    Format.fprintf oc "%s <- %s %s"
      (print_reg rd) (print_unop u)  (print_reg rs)
  | LStore(rt, i, rs, sz) ->
    Format.fprintf oc "%s{%d}[%d] <- %s" (print_reg rt) sz i (print_reg rs)
  | LLoad(rd, rt, i, sz) ->
    Format.fprintf oc "%s <- %s{%d}[%d]" (print_reg rd) (print_reg rt) sz i
  | LMov(rd, rs) ->
    Format.fprintf oc "%s <- %s" (print_reg rd) (print_reg rs)
  | LLabel l ->
    Format.fprintf oc "%s" l
  | LJmp l -> Format.fprintf oc "j %s" l
  | LJmpr r -> Format.fprintf oc "jmpr %s" (print_reg r)
  | LConst (rd, i) -> Format.fprintf oc "%s <- %d" (print_reg rd) i
  | LComment l -> Format.fprintf oc "; %s" l
  | LBranch(cmp, rs1, rs2, s) ->
    Format.fprintf oc "%s(%s,%s) ? j %s"
      (print_cmpop cmp) (print_reg rs1) (print_reg rs2) s
  | LCall fname ->
    Format.fprintf oc "call %s" fname
  | LHalt -> Format.fprintf oc "halt"

let dump_ltl_instr_list fname oc l =
  List.iteri (fun i ins ->
      Format.fprintf oc "%s:%d: " fname i;
      dump_ltl_instr oc ins;
      Format.fprintf oc "\n") l

let dump_ltl_fun oc fname lf =
  Format.fprintf oc "%s:\n" fname;
  dump_ltl_instr_list fname oc lf.ltlfunbody

let dump_ltl_prog oc lp =
  dump_prog dump_ltl_fun oc lp

let dump_allocation fname alloc =
  Format.printf "In function %s\n" fname;
  Hashtbl.iter (fun linr ltlloc ->
      Format.printf "LinReg %d allocated to %s\n" linr (print_loc ltlloc)
    ) alloc
